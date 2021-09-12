use super::super::structs::FixedU16CodePointArray;
use super::ast::*;
use super::ast_builder::*;
use super::error_reporter::*;
use super::node_ops::*;
use super::parser_def::*;
use super::parser_state::{ParserState, ParserStateStack};
use super::scanner::*;
use super::scope::*;
use super::skip_tree_builder::SkipTreeBuilder;
use super::source::*;
use super::source_position::*;
use super::token::Token;
use crate::context::{Context, LuxContext, ObjectRecordsInitializedContext};
use crate::utility::*;
use std::cell::RefCell;
use std::char::decode_utf16;
use std::collections::{HashSet, VecDeque};
use std::rc::Rc;

macro_rules! expect {
  (@notadvance $self:tt, $n:expr, $token:expr) => {
    if $n != $token {
      let pos = $self.source_position().clone();
      return parse_error!($self.region, format!("'{}' expected", $token.symbol()), &pos);
    }
  };
  ($self:tt, $n:expr, $token:expr) => {
    expect!(@notadvance $self, $n, $token);
    $self.advance()?;
  };
  (@oneof $self:tt, $n:expr, $($token:expr,)+) => {
    if !$n.one_of(&[$($token,)*]) {
      let s = String::new();
      $(
        s.push_str(format!("'{}' "$token.symbol()));
      )*;
      s.push_str("expected");
      let pos = $self.source_position().clone();
      return parse_error!($self.region, s, &pos);
    }
    $self.advance()?;
  };
}

#[cfg(feature = "print_ast")]
struct ParserDebugger {
  next_parse: Vec<&'static str>,
  has_error: bool,
  indent: String,
  buffer: String,
}

#[cfg(feature = "print_ast")]
impl ParserDebugger {
  fn new() -> Self {
    ParserDebugger {
      next_parse: Vec::<&'static str>::new(),
      has_error: false,
      indent: "".to_string(),
      buffer: String::new(),
    }
  }
  fn enter(
    &mut self,
    next_parse: &'static str,
    ctx: &ExprCtx,
    current_token: Token,
    position: &SourcePosition,
    has_error: bool,
  ) {
    self.next_parse.push(next_parse);
    self.has_error = has_error;

    self.buffer = format!(
      "{}\n{}Enter {}: CurrentToken = {} {:?} {:?}",
      self.buffer, self.indent, next_parse, current_token, position, ctx
    );
    #[cfg(feature = "print_ast_when_called")]
    println!(
      "{}Enter {}: CurrentToken = {} {:?} {:?}",
      self.indent, next_parse, current_token, position, ctx
    );
    self.indent = format!("  {}", self.indent);
  }

  fn leave<T>(&mut self, ctx: &ExprCtx, position: &SourcePosition, token: Token, result: &ParseResult<T>) {
    self.indent = self.indent.chars().take(self.indent.len() - 2).collect::<String>();
    self.buffer = format!(
      "{}\n{}Exit {} {} CurrentToken = {:?}{} {:?} {:?}",
      self.buffer,
      self.indent,
      *self.next_parse.last().unwrap(),
      if result.is_ok() { "Success" } else { "Failure" },
      token,
      if self.has_error { "[Error]" } else { "" },
      position,
      ctx
    );
    #[cfg(feature = "print_ast_when_called")]
    println!(
      "{}Exit {} {} CurrentToken = {:?}{} {:?} {:?}",
      self.indent,
      *self.next_parse.last().unwrap(),
      if result.is_ok() { "Success" } else { "Failure" },
      token,
      if self.has_error { "[Error]" } else { "" },
      position,
      ctx
    );
    self.next_parse.pop();
  }

  fn print_stack_trace(&self) {
    println!("{}", self.buffer);
  }
}

#[cfg(feature = "print_ast")]
macro_rules! next_parse {
  ($self:tt, $call:expr) => {{
    let cur = $self.cur();
    let source_position = $self.source_position().clone();
    let has_pending_error = $self.error_reporter.has_pending_error();
    let ctx = $self.expression_context.clone();
    $self
      .debugger
      .enter(stringify!($call), &ctx, cur, &source_position, has_pending_error);
    let result = $call;
    let cur = $self.cur();
    let pos = $self.source_position().clone();
    let ctx = $self.expression_context.clone();
    $self.debugger.leave(&ctx, &pos, cur, &result);
    result
  }};
}

macro_rules! append_var_if {
  ($self:expr, $token:expr, $value:expr, $pos:expr) => {{
    if $token == Token::Identifier {
      let value = $value.clone();
      let pos = $pos.clone();
      if let Ok(ctx) = $self.expression_context.to_arrow_ctx_mut() {
        ctx.new_var(value, pos);
      }
    }
  }};
}

#[cfg(not(feature = "print_ast"))]
struct ParserDebugger;
#[cfg(not(feature = "print_ast"))]
impl ParserDebugger {
  fn new() -> Self {
    ParserDebugger
  }
}

#[cfg(not(feature = "print_ast"))]
macro_rules! next_parse {
  ($self:tt, $call:expr) => {
    $call
  };
}

#[derive(PartialEq)]
enum TreeBuilderMode {
  Ast,
  Skip,
}

#[derive(Clone, Property)]
pub struct ParserOption {
  #[property(get(type = "copy"), set(disable))]
  disable_skip_parser: bool,
}
impl ParserOption {
  pub fn new() -> Self {
    ParserOption {
      disable_skip_parser: false,
    }
  }

  pub fn with(disable_skip_parser: bool) -> Self {
    ParserOption { disable_skip_parser }
  }
}

#[derive(PartialEq)]
pub enum ParserType {
  Script,
  Module,
}
pub struct Parser {
  context: LuxContext,
  parser_type: ParserType,
  parser_state: Exotic<ParserStateStack>,
  scanner: Exotic<Scanner>,
  region: Region,
  result: ParseResult<Ast>,
  source: Source,
  scanner_record: Option<ScannerRecord>,
  error_reporter: Exotic<ErrorReporter>,
  debugger: ParserDebugger,
  empty: Node<Empty>,
  use_strict_str: FixedU16CodePointArray,
  root_scope: Exotic<Scope>,
  current_scope: Exotic<Scope>,
  expression_context: ExprCtx,
  tree_builder_mode: TreeBuilderMode,
  ast_builder: Exotic<AstBuilder>,
  skip_tree_builder: Exotic<SkipTreeBuilder>,
  parser_option: ParserOption,
}

macro_rules! build {
  (@pos, $builder:expr, $name:ident, $pos:expr, $($args:expr),+$(,)?) => {{
    let p = $pos.runtime_source_position();
    paste! {
      $builder.[<new_ $name>]($($args),*, Some(&p))
    }
  }};
  (@pos, $builder:expr, $name:ident, $pos:expr) => {{
    let p = $pos.runtime_source_position();
    paste! {
      $builder.[<new_ $name>](Some(&p))
    }
  }};
  (@runtime_pos, $builder:expr, $name:ident, $pos:expr, $($args:expr),+$(,)?) => {{
    let p = $pos;
    paste! {
      $builder.[<new_ $name>]($($args),*, Some(&p))
    }
  }};
  (@runtime_pos, $builder:expr, $name:ident, $pos:expr) => {{
    let p = $pos;
    paste! {
      $builder.[<new_ $name>](Some(p))
    }
  }};
  ($self:tt, $builder:expr, $name:ident, $($args:expr),+$(,)?) => {{
    let p = $self.source_position().runtime_source_position();
    paste! {
      $builder.[<new_ $name>]($($args),*, Some(&p))
    }
  }};
  ($self:tt, $builder:expr, $name:ident) => {{
    let p = $self.source_position().runtime_source_position();
    paste! {
      $builder.[<new_ $name>](Some(p))
    }
  }}
}

macro_rules! is_spread_last_element {
  ($self:expr, $pos:expr) => {
    if $self.cur() == Token::Comma {
      $self.advance()?;
    }
    if $self.cur() != Token::RightParen {
      return parse_error!($self.region, "Spread must be last element", $pos);
    }
  };
}

impl ReportSyntaxError for Parser {
  fn error_reporter(&mut self) -> &mut ErrorReporter {
    return &mut self.error_reporter;
  }

  fn source_position(&self) -> &SourcePosition {
    return self.scanner.source_position();
  }
}

impl Parser {
  pub fn new(context: impl ObjectRecordsInitializedContext, source: Source, parser_option: ParserOption) -> Self {
    let mut region = Region::new();
    let empty = Empty::new(&mut region);
    let parser_state = region.alloc(ParserStateStack::new());
    let error_reporter = region.alloc(ErrorReporter::new(source.clone()));
    let mut parser = Parser {
      context: LuxContext::from_allocation_only_context(context),
      parser_type: ParserType::Script,
      parser_state,
      scanner: region.alloc(Scanner::new(
        region.clone(),
        source.clone(),
        parser_state,
        error_reporter,
      )),
      region: region.clone(),
      result: Ok(empty.into()),
      source: source.clone(),
      scanner_record: None,
      error_reporter,
      debugger: ParserDebugger::new(),
      empty,
      use_strict_str: FixedU16CodePointArray::from_utf8(context, "use strict"),
      root_scope: Scope::new(region.clone(), ScopeFlag::OPAQUE),
      current_scope: Exotic::new(std::ptr::null_mut()),
      expression_context: ExprCtx::Expr(ExpressionContext::new()),
      tree_builder_mode: TreeBuilderMode::Ast,
      ast_builder: region.alloc(AstBuilder::new(region.clone())),
      skip_tree_builder: region.alloc(SkipTreeBuilder::new(region.clone())),
      parser_option,
    };
    parser.current_scope = parser.root_scope;
    return parser;
  }

  pub fn has_error(&self) -> bool {
    return self.error_reporter.has_pending_error();
  }

  pub fn print_errors(&self) {
    self.error_reporter.print_errors();
  }

  pub fn last_error(&self) -> Option<Exotic<ErrorDescriptor>> {
    return self.error_reporter.last_error();
  }

  pub fn parse(&mut self, parser_type: ParserType) -> ParseResult<Ast> {
    self.scanner.next();
    self.parser_type = parser_type;
    self.parse_program();
    return self.result.clone();
  }

  fn runtime_source_position(&self) -> RuntimeSourcePosition {
    return self.scanner.source_position().runtime_source_position();
  }

  #[inline(always)]
  fn contextual_keyword(&self) -> Token {
    return self.scanner.contextual_keyword();
  }

  #[inline(always)]
  fn peek_contextual_keyword(&self) -> Token {
    return self.scanner.peek_contextual_keyword();
  }

  #[inline(always)]
  fn cur(&self) -> Token {
    return self.scanner.cur();
  }

  #[inline(always)]
  fn advance(&mut self) -> ParseResult<Token> {
    let next = self.scanner.next();
    if next == Token::Invalid {
      if self.error_reporter.has_pending_error() {
        return Err(self.error_reporter.last_error().unwrap());
      } else {
        return parse_error!(self.region, "Invalid token found", self.prev_source_position());
      }
    }
    return Ok(next);
  }

  #[inline(always)]
  fn peek(&mut self) -> ParseResult<Token> {
    let next = self.scanner.peek();
    if next == Token::Invalid {
      if self.error_reporter.has_pending_error() {
        return Err(self.error_reporter.last_error().unwrap());
      } else {
        return parse_error!(self.region, "Invalid token found", self.prev_source_position());
      }
    }
    return Ok(next);
  }

  #[inline(always)]
  fn value(&self) -> &Vec<u16> {
    return self.scanner.value();
  }

  #[inline(always)]
  fn numeric_value(&self) -> f64 {
    return self.scanner.numeric_value();
  }

  #[inline(always)]
  fn peek_value(&self) -> &Vec<u16> {
    return self.scanner.peek_value();
  }

  #[inline(always)]
  fn source_position(&self) -> &SourcePosition {
    return self.scanner.source_position();
  }

  #[inline(always)]
  fn prev_source_position(&self) -> &SourcePosition {
    return self.scanner.prev_source_position();
  }

  #[inline(always)]
  fn push_state(&mut self, s: ParserState) {
    self.parser_state.push_state(s);
  }

  #[inline(always)]
  fn pop_state(&mut self, s: ParserState) {
    self.parser_state.pop_state(s);
  }

  #[inline(always)]
  fn match_state(&self, s: ParserState) -> bool {
    return self.parser_state.match_state(s);
  }

  #[inline(always)]
  fn match_states(&self, s: &[ParserState]) -> bool {
    return self.parser_state.match_states(s);
  }

  #[inline(always)]
  fn is_in_state(&self, s: ParserState) -> bool {
    return self.parser_state.is_in_state(s);
  }

  #[inline(always)]
  fn is_in_states(&self, s: &[ParserState]) -> bool {
    return self.parser_state.is_in_states(s);
  }

  #[inline(always)]
  fn has_more(&self) -> bool {
    return self.cur() != Token::End;
  }

  #[inline(always)]
  fn has_line_break_before(&self) -> bool {
    return self.scanner.has_line_break_before();
  }

  #[inline(always)]
  fn has_line_break_after(&self) -> bool {
    return self.scanner.has_line_break_after();
  }

  #[inline(always)]
  fn record(&mut self) {
    return self.scanner_record = Some(self.scanner.record());
  }

  fn invalidate_pattern(&self, ec: &ExprCtx) -> Result<(), Exotic<ErrorDescriptor>> {
    if let Some(e) = ec.first_pattern_error() {
      return Err(e);
    }
    if self.current_scope.is_strict_mode() {
      if let Some(e) = ec.first_strict_mode_error() {
        return Err(e);
      }
    }
    return Ok(());
  }

  fn invalidate_arrow_parameters(&self, ec: &ExprCtx) -> Result<(), Exotic<ErrorDescriptor>> {
    if let Err(e) = self.invalidate_pattern(ec) {
      return Err(e);
    }
    if let Ok(ctx) = ec.to_arrow_ctx() {
      if let Some(e) = ctx.first_arrow_parameter_error() {
        return Err(e);
      }
    }
    return Ok(());
  }

  fn invalidate_unique_parameter(&mut self, ec: Option<&mut ExprCtx>) {
    let context = if ec.is_some() {
      ec.unwrap()
    } else {
      &mut self.expression_context
    };
    if let Ok(ctx) = context.to_arrow_ctx_mut() {
      let mut unique_set = HashSet::<&Vec<u16>>::new();
      let mut error = None;
      for (ref var, ref pos) in ctx.var_list().iter() {
        if unique_set.contains(var) {
          error = Some(parse_error!(@raw, self.region, "Duplicate parameter not allowed here", pos));
          break;
        } else {
          unique_set.insert(var);
        }
      }
      if let Some(e) = error {
        ctx.set_first_arrow_parameter_error(e);
      }
    }
  }

  #[inline(always)]
  fn new_expression_context(&mut self) -> ExprCtx {
    let old = std::mem::replace(&mut self.expression_context, ExprCtx::Expr(ExpressionContext::new()));
    self
      .expression_context
      .set_is_maybe_immediate_function(old.is_maybe_immediate_function());
    return old;
  }

  #[inline(always)]
  fn new_arrow_context(&mut self) -> ExprCtx {
    let old = std::mem::replace(
      &mut self.expression_context,
      ExprCtx::Arrow(ArrowFunctionContext::new()),
    );
    self
      .expression_context
      .set_is_maybe_immediate_function(old.is_maybe_immediate_function());
    return old;
  }

  #[inline(always)]
  fn set_expression_context(&mut self, ec: ExprCtx) -> ExprCtx {
    return std::mem::replace(&mut self.expression_context, ec);
  }

  #[inline(always)]
  fn declare_scope(&mut self, scope_flag: ScopeFlag) -> Exotic<Scope> {
    self.current_scope = Scope::new(self.region.clone(), scope_flag);
    return self.current_scope;
  }

  #[inline(always)]
  fn declare_child_scope(&mut self, scope_flag: ScopeFlag) -> Exotic<Scope> {
    let mut new_scope = Scope::new(self.region.clone(), scope_flag);
    self.current_scope.add_child_scope(new_scope);
    new_scope.set_parent_scope(Some(self.current_scope));
    self.current_scope = new_scope;
    return self.current_scope;
  }

  #[inline(always)]
  fn escape_scope(&mut self, scope: Exotic<Scope>) {
    if self.current_scope != scope {
      return;
    }
    if self.current_scope.is_strict_mode() {
      self.parser_state.leave_state(ParserState::InStrictMode);
    }
    if let Some(scope) = self.current_scope.parent_scope() {
      self.current_scope = scope;
      return;
    }
    unreachable!();
  }

  #[inline(always)]
  fn restore(&mut self) {
    if let Some(ref record) = self.scanner_record {
      self.scanner.restore(record);
    }
    self.scanner_record = None;
  }

  fn is_value_match_with(&self, expectation: FixedU16CodePointArray) -> bool {
    return expectation.eq_vec16(self.value());
  }

  fn value_to_utf8(value: &Vec<u16>) -> String {
    return decode_utf16(value.iter().cloned())
      .map(|r| r.unwrap_or('#'))
      .collect::<String>();
  }

  fn is_strict_mode(&self) -> bool {
    return self.root_scope.is_strict_mode() || self.current_scope.is_strict_mode();
  }

  #[cfg(feature = "print_ast")]
  pub fn print_stack_trace(&self) {
    self.debugger.print_stack_trace();
  }

  fn should_use_tree_builder(&self) -> bool {
    return self.tree_builder_mode == TreeBuilderMode::Ast || self.parser_option.disable_skip_parser;
  }

  fn parse_directive_prologue(&mut self, mut scope: Exotic<Scope>) -> ParseResult<Expr> {
    if self.cur() == Token::StringLiteral && self.is_value_match_with(self.use_strict_str) {
      if !self.current_scope.is_simple_parameter() {
        return parse_error!(
          self.region,
          "Declaring 'use strict' is not allowed if function has non-simple parameter list",
          self.source_position()
        );
      }
      self.advance()?;
      scope.mark_as_strict_mode();
      self.parser_state.enter_state(ParserState::InStrictMode);
      if self.cur() == Token::Terminate {
        self.advance()?;
      } else if !self.has_line_break_before() && self.cur() != Token::RightBrace {
        return parse_error!(self.region, "; expected", self.source_position());
      }
    }
    return Ok(self.empty.into());
  }

  fn parse_program(&mut self) {
    if self.error_reporter.has_pending_error() {
      self.result = Err(self.error_reporter.last_error().unwrap());
      return;
    }

    if let Err(e) = next_parse!(self, self.parse_directive_prologue(self.root_scope)) {
      self.result = Err(e);
      return;
    }

    self.result = if self.parser_type == ParserType::Script {
      self.parse_script()
    } else {
      self.parse_module()
    };

    if self.result.is_err() {
      let ed = self.result.as_ref().unwrap_err().clone();
      self.error_reporter.report_syntax_error(ed);
    }
  }

  fn parse_script(&mut self) -> ParseResult<Ast> {
    return if self.should_use_tree_builder() {
      next_parse!(self, self.parse_statement_list(self.ast_builder, |_| true))
    } else {
      next_parse!(self, self.parse_statement_list(self.skip_tree_builder, |_| true))
    }
    .map(|a| Into::<Ast>::into(a));
  }

  fn parse_module(&mut self) -> ParseResult<Ast> {
    return if self.should_use_tree_builder() {
      next_parse!(self, self.parse_module_body(self.ast_builder))
    } else {
      next_parse!(self, self.parse_module_body(self.skip_tree_builder))
    }
    .map(|a| Into::<Ast>::into(a));
  }

  fn parse_module_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let mut items = Vec::<Stmt>::new();
    while self.has_more() {
      let item = next_parse!(self, self.parse_module_item(builder))?;
      builder.push_stmt(&mut items, item);
    }
    return Ok(build!(@pos, builder, statements, start, items).into());
  }

  fn parse_terminator<T>(&mut self, expr: T) -> ParseResult<T> {
    if self.cur() == Token::Terminate {
      self.advance()?;
      return Ok(expr);
    } else if self.cur() != Token::End && self.cur() != Token::RightBrace && !self.has_line_break_after() {
      return parse_error!(self.region, "';' expected", self.source_position());
    }

    return Ok(expr);
  }

  fn parse_identifier<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    use Token::*;
    if !constraints.is_keyword_identifier_allowed() {
      if self
        .contextual_keyword()
        .one_of(&[Implements, Interface, Let, Package, Private, Protected, Public, Static])
        || self.cur() == Token::Yield
      {
        return parse_error!(
          self.region,
          format!("'{}' is reserved word", Parser::value_to_utf8(self.value())),
          &pos_range!(self.prev_source_position(), self.source_position())
        );
      }
      if self.cur() != Token::Identifier {
        return parse_error!(
          self.region,
          "Identifier expected",
          &pos_range!(self.prev_source_position(), self.source_position())
        );
      }
    }

    if self.contextual_keyword().one_of(&[Token::Eval, Token::Arguments]) {
      let e = parse_error!(@raw, self.region, "In strict mode code, 'eval' or 'arguments' not allow here", self.source_position());
      self.expression_context.set_first_strict_mode_error(e);
    }

    let val = if self.cur() == Token::Identifier {
      LiteralValue::String(
        FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
        self.contextual_keyword(),
      )
    } else {
      LiteralValue::String(
        FixedU16CodePointArray::from_utf8(self.context, self.cur().symbol()),
        self.cur(),
      )
    };

    return Ok(build!(self, builder, literal, Token::Identifier, val));
  }

  fn parse_identifier_reference<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    let result = match self.cur() {
      Token::Identifier | Token::Yield | Token::Await => {
        if self.is_strict_mode() && !constraints.is_keyword_identifier_allowed() {
          if self.cur() == Token::Yield {
            return parse_error!(
              self.region,
              "Keyword 'yield' is not allowed here in strict mode code",
              self.source_position()
            );
          }
          if self.cur() == Token::Await {
            return parse_error!(
              self.region,
              "Keyword 'await' is not allowed here in strict mode code",
              self.source_position()
            );
          }
        }
        next_parse!(self, self.parse_identifier(builder, constraints))
      }
      _ => next_parse!(self, self.parse_identifier(builder, constraints)),
    };
    self.advance()?;
    return result;
  }

  fn parse_primary_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::This => {
        self.advance()?;
        return Ok(build!(@pos, builder, literal, start, Token::This, LiteralValue::None));
      }
      Token::LeftBracket => {
        return next_parse!(self, self.parse_array_literal(builder, ParserConstraints::None));
      }
      Token::LeftBrace => {
        return next_parse!(self, self.parse_object_literal(builder, ParserConstraints::None));
      }
      Token::Function => {
        return next_parse!(self, self.parse_function_expression(builder, false, false));
      }
      Token::Class => {
        return next_parse!(self, self.parse_class_expression(builder));
      }
      Token::Identifier => {
        if self.contextual_keyword() == Token::Async && !self.has_line_break_after() && self.peek()? == Token::Function
        {
          self.advance()?;
          return next_parse!(self, self.parse_function_expression(builder, true, false));
        }
        return next_parse!(self, self.parse_identifier_reference(builder, ParserConstraints::None));
      }
      Token::ImplicitOctalLiteral => {
        if self.is_strict_mode() {
          return parse_error!(
            self.region,
            "Implicit octal literal not allowed in strict mode",
            self.source_position()
          );
        }
        return next_parse!(self, self.parse_literal(builder));
      }
      Token::NumericLiteral => {
        return next_parse!(self, self.parse_literal(builder));
      }
      Token::Null | Token::True | Token::False | Token::StringLiteral => {
        return next_parse!(self, self.parse_literal(builder));
      }
      Token::BackQuote => {
        return next_parse!(self, self.parse_template_literal(builder));
      }
      Token::OpDiv => {
        return next_parse!(self, self.parse_regular_expression(builder));
      }
      Token::LeftParen => {
        self.expression_context.set_is_maybe_immediate_function(true);
        defer!(self, |mut this| {
          this.expression_context.set_is_maybe_immediate_function(false);
        });
        return next_parse!(
          self,
          self.parse_cover_parenthesized_expression_and_arrow_parameter_list(builder)
        );
      }
      _ => {
        return parse_error!(self.region, "Unexpected token found", self.source_position());
      }
    }
  }

  fn parse_literal<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let result = match self.cur() {
      Token::Null | Token::True | Token::False => {
        let t = self.cur();
        Ok(build!(self, builder, literal, t, LiteralValue::None))
      }
      Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        let t = self.cur();
        let val = LiteralValue::Number(self.numeric_value());
        Ok(build!(self, builder, literal, t, val))
      }
      Token::StringLiteral => {
        let t = self.cur();
        let val = LiteralValue::String(
          FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
          Token::Invalid,
        );
        Ok(build!(self, builder, literal, t, val))
      }
      _ => parse_error!(self.region, "Literal expected", self.source_position()),
    };
    self.advance()?;
    return result;
  }

  fn parse_regular_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_array_literal<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    expect!(@notadvance self, self.cur(), Token::LeftBracket);
    self.advance()?;
    let mut is_spread_seen = false;
    let mut spread_start_position: Option<SourcePosition> = None;
    let mut spread_end_position: Option<SourcePosition> = None;
    let mut properties = Vec::<Expr>::new();
    while !self.cur().one_of(&[Token::RightBracket, Token::Invalid, Token::End]) {
      if is_spread_seen {
        self.expression_context.set_first_pattern_error(parse_error!(
          @raw,
          self.region,
          "Spread must be last element",
          &pos_range!(@just spread_start_position.unwrap(), spread_end_position.unwrap())
        ));
      }
      if self.cur() == Token::Comma {
        let start_pos = self.source_position().clone();
        self.advance()?;
        match self.cur() {
          Token::Comma | Token::RightBracket => {
            let expr = build!(@pos, builder, empty, start_pos);
            builder.push_expr(&mut properties, expr);
          }
          _ => {
            while self.cur() == Token::Comma {
              let pos = self.source_position().clone();
              let expr = build!(@pos, builder, empty, pos);
              builder.push_expr(&mut properties, expr);
              self.advance()?;
            }
          }
        }
      } else {
        if self.cur() == Token::Spread {
          is_spread_seen = true;
          spread_start_position = Some(self.prev_source_position().clone());
          append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
          let a = next_parse!(self, self.parse_spread_element(builder))?;
          spread_end_position = Some(self.prev_source_position().clone());
          builder.push_expr(&mut properties, a);
        } else {
          append_var_if!(self, self.cur(), self.value(), self.source_position());
          let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
          builder.push_expr(&mut properties, expr);
        }
      }
    }

    let array = build!(@pos, builder, structural_literal, start_pos,
      StructuralLiteralFlags::ARRAY
        | if is_spread_seen {
          StructuralLiteralFlags::HAS_SPREAD
        } else {
          StructuralLiteralFlags::NONE
        },
      properties,
      is_spread_seen
    );
    expect!(self, self.cur(), Token::RightBracket);
    return Ok(array.into());
  }

  fn parse_spread_element<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    debug_assert!(self.cur() == Token::Spread);
    self.advance()?;
    let maybe_identifier_value = self.value().clone();
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    if builder.is_identifier(expr) {
      if let Ok(arrow_ctx) = self.expression_context.to_arrow_ctx_mut() {
        arrow_ctx.new_var(maybe_identifier_value, start_pos);
      }
    }
    return Ok(build!(@pos, builder, unary_expression, start_pos,
      UnaryExpressionOperandPosition::Pre,
      Token::Spread,
      expr,
    ));
  }

  fn parse_object_literal<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    debug_assert!(self.cur() == Token::LeftBrace);
    self.advance()?;
    if self.cur() == Token::RightBrace {
      self.advance()?;
      return Ok(
        build!(
          @pos,
          builder,
          structural_literal,
          start,
          StructuralLiteralFlags::OBJECT,
          Vec::new(),
          false,
        )
        .into(),
      );
    }

    let mut properties = Vec::<Expr>::new();
    let mut is_spread_seen = false;
    while !self.cur().one_of(&[Token::RightBrace, Token::Invalid, Token::End]) {
      let start_pos = self.source_position().clone();
      let prop = next_parse!(self, self.parse_object_literal_property(builder, constraints))?;
      if is_spread_seen && builder.is_spread_expression(prop) {
        if let Ok(ctx) = self.expression_context.to_arrow_ctx_mut() {
          ctx.set_first_arrow_parameter_error(
            parse_error!(@raw, self.region, "Rest parameter must be last element", &start_pos),
          );
        }
      }
      builder.push_expr(&mut properties, prop);
      if self.cur() == Token::Comma {
        self.advance()?;
      }
    }

    expect!(self, self.cur(), Token::RightBrace);
    return Ok(
      build!(
        @pos,
        builder,
        structural_literal,
        start,
        StructuralLiteralFlags::OBJECT,
        properties,
        false,
      )
      .into(),
    );
  }

  fn parse_object_literal_property<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    macro_rules! append_var {
      ($self:expr, $value:expr, $pos:expr) => {{
        let pos = $pos.clone();
        if let Ok(arrow_ctx) = $self.expression_context.to_arrow_ctx_mut() {
          if let Some(str_value) = $value {
            arrow_ctx.new_var(str_value, pos);
          }
        }
      }};
    }

    let mut key: Option<Expr> = None;
    let mut is_computed_property_name = false;
    let mut is_key_identifier = false;
    let mut ident_value = None;
    let start = self.source_position().clone();
    let binding_tokens = [
      Token::Identifier,
      Token::LeftBracket,
      Token::StringLiteral,
      Token::NumericLiteral,
      Token::ImplicitOctalLiteral,
      Token::OpMul,
    ];

    if self.cur().one_of(&binding_tokens) {
      if self.cur() != Token::LeftBracket && self.peek()? == Token::LeftParen {
        return next_parse!(self, self.parse_method_definition(builder));
      }

      if self.contextual_keyword() == Token::Async
        && self.peek()?.one_of(&binding_tokens)
        && !self.has_line_break_after()
      {
        let state = if self.peek()? == Token::OpMul {
          ParserState::InAsyncGeneratorFunction
        } else {
          ParserState::InAsyncFunction
        };
        let mut this = scoped!(self, |this| {
          this.parser_state.pop_state(state);
        });
        this.parser_state.push_state(state);
        return next_parse!(this, this.parse_method_definition(builder));
      }

      if self.cur() == Token::OpMul && self.peek()?.one_of(&binding_tokens) {
        let mut this = scoped!(self, |this| {
          let n = this.parser_state.pop_state(ParserState::InGeneratorFunction);
          debug_assert!(
            n,
            format!(
              "State mismatched current state = {:?}",
              if let Some(cur) = this.parser_state.cur_state() {
                *cur
              } else {
                ParserState::_None
              }
            )
          )
        });
        this.parser_state.push_state(ParserState::InGeneratorFunction);
        return next_parse!(this, this.parse_method_definition(builder));
      }

      if self.cur() == Token::Identifier
        && self.contextual_keyword().one_of(&[Token::Get, Token::Set])
        && self.peek()? == Token::Identifier
      {
        return next_parse!(self, self.parse_method_definition(builder));
      }

      is_key_identifier = self.cur() == Token::Identifier;
      ident_value = if is_key_identifier {
        Some(self.value().clone())
      } else {
        None
      };
      key = Some(next_parse!(self, self.parse_property_name(builder))?);

      if self.cur() == Token::OpAssign {
        append_var!(self, ident_value, &start);
        let start_initializer = self.source_position().clone();
        if constraints.is_binding_pattern_allowed() || is_computed_property_name {
          return parse_error!(self.region, "Unexpected '=' detected", self.source_position());
        }
        self.advance()?;
        let value = next_parse!(self, self.parse_assignment_expression(builder))?;
        if let Some(key_node) = key {
          let node = build!(@pos, builder, object_property_expression, start,
            key_node,
            None,
            Some(value),
          );
          self
            .expression_context
            .set_first_value_error(parse_error!(@raw, self.region, "Unexpected initializer", &start_initializer));
          return Ok(node);
        }
        return parse_error!(self.region, "Expression expected", self.source_position());
      } else if self.cur().one_of(&[Token::Comma, Token::RightBrace]) {
        append_var!(self, ident_value, &start);
        if let Some(key_node) = key {
          return Ok(build!(@pos, builder, object_property_expression, start, key_node, None, None));
        }
        return parse_error!(self.region, "Expression expected", self.source_position());
      }
    } else if self.cur() == Token::Spread {
      append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
      let spread = next_parse!(self, self.parse_spread_element(builder))?;
      return Ok(build!(@pos, builder, object_property_expression, start,
        spread,
        None,
        None
      ));
    } else {
      return parse_error!(self.region, format!("Property name must be one of 'identifier', 'string literal', 'numeric literal' or 'computed property' but got {}", self.cur().symbol()), self.source_position());
    }

    if key.is_none() {
      return parse_error!(self.region, "Expression expected", self.source_position());
    }

    let key_node = key.unwrap();
    if self.cur() != Token::Colon {
      if is_key_identifier {
        append_var!(self, ident_value, &start);
        return Ok(build!(@pos, builder, object_property_expression,start,
          key_node,
          None,
          None,
        ));
      }
    }

    expect!(self, self.cur(), Token::Colon);
    let value_start_pos = self.source_position().clone();
    ident_value = Some(self.value().clone());
    let next_token = self.cur();
    let value = next_parse!(self, self.parse_assignment_expression(builder))?;
    if builder.is_identifier(value) || next_token == Token::Identifier {
      append_var!(self, ident_value, &value_start_pos);
    } else if !builder.is_structural_literal(value) {
      self.expression_context.set_first_pattern_error(
        parse_error!(@raw, self.region, "'Identifier' or 'Pattern' expected", &value_start_pos),
      );
    }

    if constraints.is_binding_pattern_allowed() {
      if let Err(e) = self.invalidate_pattern(&self.expression_context) {
        return Err(e);
      }
    }

    return Ok(build!(@pos, builder, object_property_expression, start,
      key_node,
      Some(value),
      None,
    ));
  }

  fn parse_property_name<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::Identifier => {
        let i = next_parse!(
          self,
          self.parse_identifier(builder, ParserConstraints::KeywordIdentifier)
        );
        self.advance()?;
        return i;
      }
      Token::StringLiteral | Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        return next_parse!(self, self.parse_literal(builder));
      }
      _ => {
        if self.cur().is_allowed_in_property_name() {
          let i = next_parse!(
            self,
            self.parse_identifier(builder, ParserConstraints::KeywordIdentifier)
          );
          self.advance()?;
          return i;
        }
        expect!(self, self.cur(), Token::LeftBracket);
        let ret = next_parse!(self, self.parse_assignment_expression(builder))?;
        expect!(self, self.cur(), Token::RightBracket);
        return Ok(ret);
      }
    };
  }

  fn parse_template_literal<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::BackQuote);
    let mut properties = Vec::<Expr>::new();
    while self.cur() != Token::Template {
      match self.cur() {
        Token::StringLiteral => {
          let val = LiteralValue::String(
            FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
            Token::Invalid,
          );
          let literal = build!(self, builder, literal, Token::StringLiteral, val);
          builder.push_expr(&mut properties, literal);
          self.advance()?;
        }
        Token::TemplateSubstitution => {
          self.parser_state.push_state(ParserState::InTemplateInterpolation);
          self.advance()?;
          let expr = next_parse!(self, self.parse_expression(builder, false))?;
          builder.push_expr(&mut properties, expr);
          self.parser_state.pop_state(ParserState::InTemplateInterpolation);
          expect!(self, self.cur(), Token::RightBrace);
        }
        _ => {
          self.advance()?;
        }
      };
    }

    self.parser_state.pop_state(ParserState::InTaggedTemplateLiteral);
    self.parser_state.pop_state(ParserState::InTemplateLiteral);
    self.advance()?;
    return Ok(build!(
        @pos,
        builder,
        template_literal, start_pos, properties));
  }

  fn parse_cover_parenthesized_expression_and_arrow_parameter_list<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
  ) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    if self.cur() == Token::RightParen {
      return parse_error!(self.region, "Unexpected token", self.source_position());
    }

    if self.cur() == Token::Spread {
      let mut items = Vec::<Expr>::new();
      append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
      let u = next_parse!(self, self.parse_spread_element(builder))?;
      let end = self.prev_source_position().clone();
      builder.push_expr(&mut items, u);
      is_spread_last_element!(self, &pos_range!(start, end));
      let mut node = build!(@pos, builder, expressions, start, items);
      node.set_parenthesized();
      return Ok(node.into());
    }

    let n = next_parse!(self, self.parse_expression(builder, false))?;

    let mut expr = if self.cur() == Token::Spread {
      let spread_start = self.source_position().clone();
      append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
      let u = next_parse!(self, self.parse_spread_element(builder))?;
      let end = self.prev_source_position().clone();
      if builder.exprs_push(n, u) {
        is_spread_last_element!(self, &pos_range!(spread_start, end));
        n
      } else {
        let items = vec![u];
        build!(@pos, builder, expressions, start, items)
      }
    } else {
      n
    };
    expr.set_parenthesized();
    if self.expression_context.to_arrow_ctx().is_ok() {
      self.invalidate_unique_parameter(None);
    }

    expect!(self, self.cur(), Token::RightParen);
    return Ok(expr);
  }

  fn parse_member_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();

    if self.cur() == Token::Super {
      self.advance()?;
      return next_parse!(
        self,
        self.parse_post_member_expression(
          builder,
          &start.runtime_source_position(),
          self.empty.into(),
          CallReceiverType::Super,
          ParserConstraints::None,
          true
        )
      );
    }

    if self.cur() == Token::New {
      self.advance()?;
      if self.cur() == Token::Dot {
        self.advance()?;
        if self.peek()? == Token::Identifier {
          if self.peek_contextual_keyword() == Token::Target {
            return Ok(build!(
              self,
              builder,
              property_access_expression,
              PropertyAccessType::Dot,
              CallReceiverType::New,
              None,
              None
            ));
          }
          return parse_error!(
            self.region,
            format!("new.target? but got {}", Parser::value_to_utf8(self.peek_value())),
            self.source_position()
          );
        }
        return parse_error!(
          self.region,
          "new.target? identifier 'target' expected",
          self.source_position()
        );
      }
      let start_call = self.source_position().clone();
      let m = next_parse!(self, self.parse_member_expression(builder))?;
      if self.cur() != Token::LeftParen {
        return Ok(build!(@pos, builder, new_expression, start, m));
      }
      let args = next_parse!(self, self.parse_arguments(builder))?;
      let call = build!(@pos,
        builder,
        call_expression,
        start_call,
        CallReceiverType::Expr,
        Some(m),
        Some(args)
      );
      let expr = build!(@pos, builder, new_expression, start, call);
      return next_parse!(
        self,
        self.parse_post_member_expression(
          builder,
          &start.runtime_source_position(),
          expr.into(),
          CallReceiverType::Expr,
          ParserConstraints::Template,
          false
        )
      );
    }

    let n = next_parse!(self, self.parse_primary_expression(builder))?;
    return next_parse!(
      self,
      self.parse_post_member_expression(
        builder,
        &start.runtime_source_position(),
        n,
        CallReceiverType::Expr,
        ParserConstraints::Template,
        false
      )
    );
  }

  fn parse_post_member_expression<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    source_position: &RuntimeSourcePosition,
    expr: Expr,
    receiver_type: CallReceiverType,
    constraints: ParserConstraints,
    error_if_default: bool,
  ) -> ParseResult<Expr> {
    let mut current = expr;

    loop {
      match self.cur() {
        Token::LeftParen => {
          if !constraints.is_call_allowed() {
            break;
          }
          let a = next_parse!(self, self.parse_arguments(builder))?;
          current = build!(@runtime_pos,
            builder,
            call_expression,
            source_position,
            receiver_type,
            Some(current),
            Some(a.into())
          )
        }
        Token::LeftBracket => {
          self.advance()?;
          let n = next_parse!(self, self.parse_expression(builder, false))?;
          let end = self.source_position().clone();
          expect!(self, self.cur(), Token::RightBracket);
          current = build!(@runtime_pos,
            builder,
            property_access_expression,
            source_position,
            PropertyAccessType::Element,
            receiver_type,
            Some(current),
            Some(n)
          );
        }
        Token::Dot => {
          self.advance()?;
          let ident = next_parse!(
            self,
            self.parse_identifier_reference(builder, ParserConstraints::KeywordIdentifier)
          )?;
          current = build!(@runtime_pos,
            builder,
            property_access_expression,
            source_position,
            PropertyAccessType::Dot,
            receiver_type,
            Some(current),
            Some(ident)
          );
        }
        Token::BackQuote => {
          if !constraints.is_template_allowed() {
            return parse_error!(self.region, "Unexpected '`' found", self.source_position());
          }
          self.parser_state.push_state(ParserState::InTaggedTemplateLiteral);
          let tmpl = next_parse!(self, self.parse_template_literal(builder))?;
          current = build!(@runtime_pos,
            builder,
            call_expression,
            source_position,
            CallReceiverType::Template,
            Some(current),
            Some(tmpl)
          )
        }
        _ => {
          if error_if_default {
            return parse_error!(
              self.region,
              format!("Unexpected {} found", self.cur().symbol()),
              self.source_position()
            );
          }
          return Ok(current);
        }
      }
    }

    return Ok(current);
  }

  fn parse_new_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    if self.cur() == Token::New {
      let start = self.source_position().clone();
      if self.peek()? == Token::New {
        self.advance()?;
        let callee = next_parse!(self, self.parse_new_expression(builder))?;
        return Ok(build!(@pos, builder, new_expression, start, callee));
      }
      return next_parse!(self, self.parse_member_expression(builder));
    }
    return next_parse!(self, self.parse_member_expression(builder));
  }

  fn parse_super_call<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Super);
    let args = next_parse!(self, self.parse_arguments(builder))?;
    return Ok(build!(
      self,
      builder,
      call_expression,
      CallReceiverType::Super,
      None,
      Some(args)
    ));
  }

  fn parse_import_call<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Import);
    expect!(self, self.cur(), Token::LeftParen);
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    expect!(self, self.cur(), Token::RightParen);
    return Ok(build!(
      self,
      builder,
      call_expression,
      CallReceiverType::Super,
      None,
      Some(expr)
    ));
  }

  fn parse_arguments<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    let mut items = Vec::<Expr>::new();
    loop {
      if self.cur() == Token::Spread {
        append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
        let expr = next_parse!(self, self.parse_spread_element(builder))?;
        builder.push_expr(&mut items, expr);
      } else if self.cur() == Token::RightParen {
        break;
      } else {
        append_var_if!(self, self.cur(), self.value(), self.source_position());
        let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
        if !builder.is_identifier(expr) {
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
        }
        builder.push_expr(&mut items, expr);
      }

      if self.cur() == Token::Comma {
        if self.peek()? == Token::LeftParen {
          return parse_error!(self.region, "Extra ',' found", self.source_position());
        }
        self.advance()?;
      } else {
        break;
      }
    }

    expect!(self, self.cur(), Token::RightParen);
    if self.expression_context.to_arrow_ctx().is_ok() {
      self.invalidate_unique_parameter(None);
    }
    return Ok(build!(@pos, builder, expressions, start_pos, items).into());
  }

  fn parse_left_hand_side_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::New => {
        if self.peek()? == Token::Dot {
          self.advance()?;
          return next_parse!(self, self.parse_member_expression(builder));
        }
        return next_parse!(self, self.parse_member_expression(builder));
      }
      Token::Super => {
        let pos = self.source_position().clone();
        self.current_scope.set_first_super_call_position(&pos);
        if self.peek()? == Token::LeftParen {
          return next_parse!(self, self.parse_super_call(builder));
        }
        return next_parse!(self, self.parse_member_expression(builder));
      }
      Token::Import => {
        return next_parse!(self, self.parse_import_call(builder));
      }
      _ => {
        let expr = next_parse!(self, self.parse_member_expression(builder))?;
        if self.cur() == Token::LeftParen {
          let a = next_parse!(self, self.parse_arguments(builder))?;
          let n = build!(@runtime_pos,
            builder,
            call_expression,
            expr.source_position(),
            CallReceiverType::Expr,
            Some(expr),
            Some(a)
          );
          return next_parse!(
            self,
            self.parse_post_member_expression(
              builder,
              expr.source_position(),
              n.into(),
              CallReceiverType::Expr,
              ParserConstraints::Template | ParserConstraints::Call,
              false
            )
          );
        } else {
          return Ok(expr);
        };
      }
    }
  }

  fn parse_update_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::OpIncrement | Token::OpDecrement => {
        let op = self.cur();
        self.advance()?;
        let n = next_parse!(self, self.parse_left_hand_side_expression(builder))?;
        return Ok(build!(@pos, builder, unary_expression, start, UnaryExpressionOperandPosition::Pre, op, n));
      }
      _ => {
        let n = next_parse!(self, self.parse_left_hand_side_expression(builder))?;
        let result = match self.cur() {
          Token::OpIncrement | Token::OpDecrement => {
            let token = self.cur();
            let sp = self.source_position().clone();
            self.advance()?;
            Ok(build!(
              @pos,
              builder,
              unary_expression,
              start,
              UnaryExpressionOperandPosition::Post,
              token,
              n
            ))
          }
          _ => Ok(n),
        };
        return result;
      }
    }
  }

  fn parse_unary_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::Delete | Token::Void | Token::Typeof | Token::OpPlus | Token::OpMinus | Token::OpTilde | Token::OpNot => {
        let op = self.cur();
        self.advance()?;
        self.expression_context.set_is_maybe_immediate_function(true);
        let rhs_exp = next_parse!(self, self.parse_unary_expression(builder))?;
        self.expression_context.set_is_maybe_immediate_function(false);
        return Ok(build!(
          @pos,
          builder,
          unary_expression,
          start,
          UnaryExpressionOperandPosition::Pre,
          op,
          rhs_exp
        ));
      }
      Token::Await => {
        if !self
          .parser_state
          .match_states(&[ParserState::InAsyncFunction, ParserState::InAsyncGeneratorFunction])
        {
          return parse_error!(
            self.region,
            "await is not allowed outside of async function.",
            self.source_position()
          );
        }
        return next_parse!(self, self.parse_await_expression(builder));
      }
      _ => {
        return next_parse!(self, self.parse_update_expression(builder));
      }
    }
  }

  fn parse_binary_operator_by_priority<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    mut prev_ast: Expr,
    priority: OperatorPriority,
    prev_priority: OperatorPriority,
  ) -> ParseResult<Expr> {
    let token = self.cur();
    self.advance()?;

    let expr = next_parse!(self, self.parse_unary_expression(builder))?;
    let left;
    let right;
    if prev_priority == OperatorPriority::None || prev_priority >= priority {
      left = prev_ast;
      right = expr;
      return Ok(build!(@runtime_pos, builder, binary_expr, left.source_position(), token, left, right));
    }

    let mut maybe_bin_ast = prev_ast;
    while match maybe_bin_ast {
      Expr::BinaryExpression(_) => true,
      _ => false,
    } {
      match maybe_bin_ast {
        Expr::BinaryExpression(node) => {
          let rhs = node.rhs();
          if !match rhs {
            Expr::BinaryExpression(_) => true,
            _ => false,
          } {
            break;
          }
          maybe_bin_ast = rhs;
        }
        _ => break,
      }
    }
    if let Ok(mut bin_expr) = Node::<BinaryExpression>::try_from(maybe_bin_ast) {
      right = expr;
      left = bin_expr.rhs();
      let ret = build!(@runtime_pos, builder, binary_expr, left.source_position(), token, left, right);
      bin_expr.set_rhs(ret);
    }
    return Ok(prev_ast);
  }

  fn parse_binary_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let mut last = next_parse!(self, self.parse_unary_expression(builder))?;
    let mut last_op = OperatorPriority::None;
    loop {
      match self.cur() {
        Token::OpLogicalOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::LogicalOr, last_op)
          )?;
          last_op = OperatorPriority::LogicalOr;
        }
        Token::OpLogicalAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::LogicalAnd, last_op)
          )?;
          last_op = OperatorPriority::LogicalAnd;
        }
        Token::OpOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseOr, last_op)
          )?;
          last_op = OperatorPriority::BitwiseOr;
        }
        Token::OpXor => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseXor, last_op)
          )?;
          last_op = OperatorPriority::BitwiseXor;
        }
        Token::OpAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseAnd, last_op)
          )?;
          last_op = OperatorPriority::BitwiseAnd;
        }
        Token::OpEq | Token::OpStrictEq | Token::OpNotEq | Token::OpStrictNotEq => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Equality, last_op)
          )?;
          last_op = OperatorPriority::Equality;
        }
        Token::OpGreaterThan
        | Token::OpGreaterThanOrEq
        | Token::OpLessThan
        | Token::OpLessThanOrEq
        | Token::Instanceof
        | Token::In => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Relational, last_op)
          )?;
          last_op = OperatorPriority::Relational;
        }
        Token::OpShl | Token::OpShr | Token::OpUShr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Shift, last_op)
          )?;
          last_op = OperatorPriority::Shift;
        }
        Token::OpPlus | Token::OpMinus => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Additive, last_op)
          )?;
          last_op = OperatorPriority::Additive;
        }
        Token::OpMul | Token::OpDiv | Token::OpMod => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Multiplicative, last_op)
          )?;
          last_op = OperatorPriority::Multiplicative;
        }
        Token::OpPow => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Exponentiation, last_op)
          )?;
          last_op = OperatorPriority::Exponentiation;
        }
        _ => return Ok(last),
      }
    }
  }

  fn parse_conditional_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let bin_expr = next_parse!(self, self.parse_binary_expression(builder))?;
    if self.cur() == Token::Question {
      self.advance()?;
      let lhs = next_parse!(self, self.parse_assignment_expression(builder))?;
      if self.cur() != Token::Colon {
        return parse_error!(self.region, "':' expected", self.source_position());
      }
      self.advance()?;
      let rhs = next_parse!(self, self.parse_assignment_expression(builder))?;
      return Ok(build!(@runtime_pos,
        builder,
        conditional_expression,
        bin_expr.source_position(),
        bin_expr,
        lhs,
        rhs
      ));
    }
    return Ok(bin_expr);
  }

  fn parse_assignment_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::Yield => {
        if !self.match_states(&[ParserState::InGeneratorFunction, ParserState::InAsyncGeneratorFunction]) {
          return parse_error!(
            self.region,
            "yield only allowed in generator or async generator.",
            self.source_position()
          );
        }
        return next_parse!(self, self.parse_yield_expression(builder));
      }
      Token::New => {
        return next_parse!(self, self.parse_assignment_expression_lhs(builder));
      }
      _ => {
        if self.cur().one_of(&[Token::Identifier, Token::Await, Token::Yield])
          && self.peek()? == Token::ArrowFunctionGlyph
        {
          let params = next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::None))?;
          return next_parse!(self, self.parse_concise_body(builder, false, true, params));
        }

        if self.cur() == Token::LeftParen && self.peek()? == Token::RightParen {
          let exprs = build!(@pos, builder, expressions, self.source_position(), Vec::new());
          self.advance()?;
          self.advance()?;
          expect!(self, self.cur(), Token::ArrowFunctionGlyph);
          return next_parse!(self, self.parse_concise_body(builder, false, true, exprs));
        }

        let expr_start_pos = self.prev_source_position().clone();
        let next_contextual_keyword = self.contextual_keyword();
        if next_contextual_keyword == Token::Async && !self.has_line_break_after() && self.peek()? == Token::Identifier
        {
          self.advance()?;
          let expr = next_parse!(self, self.parse_identifier_reference(builder, ParserConstraints::None))?;
          expect!(self, self.cur(), Token::ArrowFunctionGlyph);
          return next_parse!(self, self.parse_concise_body(builder, true, true, expr));
        }

        let mut ec = self.new_arrow_context();
        let expr = next_parse!(self, self.parse_conditional_expression(builder))?;
        let expr_end_pos = self.prev_source_position().clone();

        if self.cur() == Token::ArrowFunctionGlyph {
          if builder.is_new_call(expr) || builder.is_super_call(expr) {
            return parse_error!(self.region, "Super or new is not allowed here", &expr_start_pos);
          }
          let is_async = next_contextual_keyword == Token::Async;
          if builder.is_call_expr(expr) && !is_async {
            return parse_error!(self.region, "Unexpected token", self.source_position());
          }
          self.advance()?;

          if let Err(e) = self.invalidate_arrow_parameters(&self.expression_context) {
            self.set_expression_context(ec);
            return Err(e);
          }
          ec = self.set_expression_context(ec);
          let result = self.parse_concise_body(
            builder,
            is_async,
            ec.to_arrow_ctx_unchecked().is_simple_parameter(),
            expr,
          );
          return result;
        }

        if self.cur().is_assignment_operator() {
          if expr.is_parenthesized() || (!builder.is_identifier(expr) && !builder.is_structural_literal(expr)) {
            return parse_error!(
              self.region,
              "Invalid left hand side expression",
              &pos_range!(@just &expr_start_pos, &expr_end_pos)
            );
          }
          if self.cur() == Token::OpAssign {
            if let Err(e) = self.invalidate_pattern(&self.expression_context) {
              return Err(e);
            }
            if let Ok(arrow) = self.expression_context.to_arrow_ctx_mut() {
              arrow.propagate_arrow(&mut ec);
            }
          } else if !builder.is_identifier(expr) {
            return parse_error!(self.region, "Identifier expected", &expr_start_pos);
          }
          let op = self.cur();
          self.advance()?;
          let _ = self.new_expression_context();
          let assignment = next_parse!(self, self.parse_assignment_expression(builder))?;
          if let Some(e) = self.expression_context.first_value_error() {
            return Err(e);
          }
          self.set_expression_context(ec);
          return Ok(build!(@runtime_pos, builder, binary_expr, expr.source_position(), op, expr, assignment));
        }
        if let Some(e) = self.expression_context.first_value_error() {
          return Err(e);
        }
        self.expression_context.propagate(&mut ec);
        self.set_expression_context(ec);
        return Ok(expr);
      }
    }
  }

  fn parse_assignment_expression_lhs<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let lhs = next_parse!(self, self.parse_left_hand_side_expression(builder))?;
    if self.cur().is_assignment_operator() {
      let op = self.cur();
      self.advance()?;
      let rhs = next_parse!(self, self.parse_assignment_expression(builder))?;
      return Ok(build!(self, builder, binary_expr, op, lhs, rhs));
    }
    return Ok(lhs);
  }

  fn parse_expression<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    should_return_exprs: bool,
  ) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    append_var_if!(self, self.cur(), self.value(), self.source_position());
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    if self.cur() == Token::Comma {
      let mut items = Vec::<Expr>::new();
      builder.push_expr(&mut items, expr);
      while self.cur() == Token::Comma {
        self.advance()?;
        if self.cur() == Token::Spread {
          break;
        }
        append_var_if!(self, self.cur(), self.value(), self.source_position());
        let assignment_expr = next_parse!(self, self.parse_assignment_expression(builder))?;
        if !builder.is_identifier(assignment_expr) {
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
        }
        builder.push_expr(&mut items, assignment_expr);
      }
      return Ok(build!(@pos, builder, expressions, start_pos, items));
    }

    if should_return_exprs {
      return Ok(build!(@pos, builder, expressions, start_pos, vec![expr]));
    }
    return Ok(expr);
  }

  fn parse_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    use Token::*;
    match self.cur() {
      Terminate => {
        self.advance()?;
        return next_parse!(self, self.parse_statement(builder));
      }
      LeftBrace => {
        return next_parse!(self, self.parse_block_statement(builder));
      }
      Var => {
        return next_parse!(self, self.parse_variable_statement(builder));
      }
      If => {
        return next_parse!(self, self.parse_if_statement(builder));
      }
      Break => {
        return next_parse!(self, self.parse_break_statement(builder));
      }
      Return => {
        return next_parse!(self, self.parse_return_statement(builder));
      }
      With => {
        return next_parse!(self, self.parse_with_statement(builder));
      }
      Throw => {
        return next_parse!(self, self.parse_throw_statement(builder));
      }
      Try => {
        return next_parse!(self, self.parse_try_statement(builder));
      }
      Debugger => {
        return next_parse!(self, self.parse_debugger_statement(builder));
      }
      _ => {
        if self.peek()? == Colon {
          return next_parse!(self, self.parse_labelled_statement(builder));
        }
        return next_parse!(self, self.parse_expression_statement(builder));
      }
    }
  }

  fn parse_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let mut is_async = false;
    match self.cur() {
      Token::Identifier => {
        if self.contextual_keyword() == Token::Async {
          return self.parse_lexical_declaration(builder);
        }
        is_async = true;
        return next_parse!(self, self.parse_function_declaration(builder, is_async, false));
      }
      Token::Function => {
        return next_parse!(self, self.parse_function_declaration(builder, is_async, false));
      }
      _ => unreachable!(),
    };
  }

  fn parse_block_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_block<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_statement_list<Builder: NodeOps, T: Fn(Token) -> bool>(
    &mut self,
    mut builder: Exotic<Builder>,
    condition: T,
  ) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let mut items = Vec::<Stmt>::new();
    while self.has_more() && condition(self.cur()) {
      let stmt = next_parse!(self, self.parse_statement_list_item(builder))?;
      builder.push_stmt(&mut items, stmt);
    }

    return Ok(build!(@pos, builder, statements, start, items).into());
  }

  fn parse_statement_list_item<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    match self.cur() {
      Token::Class | Token::Const | Token::Function => {
        return next_parse!(self, self.parse_declaration(builder));
      }
      _ => {
        if self.cur() == Token::Identifier {
          let v = self.value();
          if (self.contextual_keyword() == Token::Async
            && self.peek()? == Token::Function
            && !self.has_line_break_after())
            || self.contextual_keyword() == Token::Let
          {
            return next_parse!(self, self.parse_declaration(builder));
          }
        }
      }
    };

    return next_parse!(self, self.parse_statement(builder));
  }
  fn parse_lexical_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }
  fn parse_lexical_binding<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }
  fn parse_variable_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }
  fn parse_variable_declaration_list<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }
  fn parse_variable_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_binding_pattern<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBrace => {
        return next_parse!(
          self,
          self.parse_object_literal(builder, ParserConstraints::BindingPattern)
        );
      }
      _ => {
        debug_assert!(self.cur() == Token::LeftBracket);
        return next_parse!(
          self,
          self.parse_array_literal(builder, ParserConstraints::BindingPattern)
        );
      }
    }
  }

  fn parse_binding_element<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBracket | Token::LeftBrace => {
        return next_parse!(self, self.parse_binding_pattern(builder));
      }
      _ => {
        return next_parse!(
          self,
          self.parse_single_name_binding(builder, ParserConstraints::Initializer)
        )
      }
    };
  }

  fn parse_single_name_binding<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    let mut identifier: Expr = self.empty.into();
    match self.cur() {
      Token::Identifier | Token::Yield | Token::Await => {
        if self.is_strict_mode() {
          if self.cur() == Token::Identifier && self.contextual_keyword().one_of(&[Token::Arguments, Token::Eval]) {
            return parse_error!(
              self.region,
              "Accessed to 'arguments' or 'eval' is not allowed in strict mode code",
              self.source_position()
            );
          }
          if self.cur() == Token::Yield {
            return parse_error!(
              self.region,
              "Keyword 'yield' is not allowed here in strict mode code",
              self.source_position()
            );
          }
        }
        let val = LiteralValue::String(
          FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
          self.contextual_keyword(),
        );
        identifier = build!(self, builder, literal, Token::Identifier, val);
      }
      _ => {
        return parse_error!(self.region, "Identifier expected", self.source_position());
      }
    };

    debug_assert!(match identifier {
      Expr::Empty(_) => false,
      _ => true,
    });
    if constraints.is_initializer_allowed() && self.cur() == Token::OpAssign {
      self.advance()?;
      let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
      return Ok(build!(self, builder, binary_expr, Token::OpAssign, identifier, expr).into());
    } else {
      self.advance()?;
    }
    return Ok(identifier);
  }

  fn parse_expression_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let expr = next_parse!(self, self.parse_expression(builder, false))?;
    let stmt = build!(@pos, builder, statement, start, expr);
    return next_parse!(self, self.parse_terminator(stmt.into()));
  }

  fn parse_if_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_iteration_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_for_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_for_binding<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_continue_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_break_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_return_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_with_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_switch_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_case_block<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_case_clauses<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_case_clause<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_default_caluse<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_labelled_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_labelled_item<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_throw_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_try_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_catch<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_finally<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_catch_parameter<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_debugger_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_function_declaration<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_async: bool,
    is_default: bool,
  ) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let function = next_parse!(self, self.parse_function_expression(builder, is_async, is_default))?;
    return Ok(build!(@pos, builder, statement, start, function));
  }

  fn parse_function_expression<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_async: bool,
    is_name_ommitable: bool,
  ) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    if is_async {
      self.advance()?;
    }

    expect!(self, self.cur(), Token::Function);
    let mut identifier: Option<Expr> = None;

    let is_generator = if self.cur() == Token::OpMul {
      self.advance()?;
      self.parser_state.push_state(ParserState::InGeneratorFunction);
      true
    } else {
      false
    };

    if self.cur() == Token::Identifier {
      identifier = Some(next_parse!(
        self,
        self.parse_identifier(builder, ParserConstraints::None)
      )?);
      self.advance()?;
    } else if !is_name_ommitable {
      return parse_error!(self.region, "Identifier expected", self.source_position());
    }

    let scope = self.declare_child_scope(ScopeFlag::OPAQUE);
    let mut this = scoped!(self, |this| {
      this.escape_scope(scope);
    });
    let formal_parameter_position = this.source_position().clone();
    expect!(this, this.cur(), Token::LeftParen);
    let mut params = next_parse!(this, this.parse_formal_parameters(builder, is_generator))?;
    params.set_source_position(&formal_parameter_position.runtime_source_position());
    expect!(this, this.cur(), Token::RightParen);

    let body_start_position = this.source_position().clone();
    expect!(this, this.cur(), Token::LeftBrace);
    this.tree_builder_mode = TreeBuilderMode::Skip;
    next_parse!(this, this.parse_directive_prologue(scope))?;
    let mut function_body_start = this.scanner.source_index() as u32;
    let mut function_body_end = 0;
    let body = if !this.expression_context.is_maybe_immediate_function() && !this.parser_option.disable_skip_parser {
      let skip_tree_builder = this.skip_tree_builder;
      next_parse!(this, this.parse_function_body(skip_tree_builder))?;
      function_body_end = this.scanner.source_index() as u32;
      None
    } else {
      function_body_start = 0;
      function_body_end = 0;
      let mut body = next_parse!(this, this.parse_function_body(builder))?;
      body.set_source_position(&body_start_position.runtime_source_position());
      Some(Into::<Ast>::into(body))
    };
    this.tree_builder_mode = TreeBuilderMode::Ast;
    this.escape_scope(scope);
    expect!(this, this.cur(), Token::RightBrace);

    let function_type = if this
      .parser_state
      .is_in_states(&[ParserState::InGeneratorFunction, ParserState::InAsyncGeneratorFunction])
    {
      FunctionType::Generator
    } else {
      FunctionType::Scoped
    };

    return Ok(build!(@pos,
      builder,
      function,
      start,
      is_async,
      identifier,
      function_type,
      scope,
      FunctionAccessor::None,
      params,
      body,
      function_body_start,
      function_body_end
    ));
  }

  fn parse_formal_parameters<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_generator: bool,
  ) -> ParseResult<Expr> {
    if self.cur() == Token::Spread {
      append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
      let start = self.source_position().clone();
      let result = next_parse!(self, self.parse_function_rest_parameter(builder))?;
      let end = self.prev_source_position().clone();
      self
        .expression_context
        .to_arrow_ctx_mut()
        .map(|ctx| ctx.set_is_simple_parameter(false));
      if self.cur() == Token::Comma {
        self.advance()?;
      }
      if self.cur() != Token::RightParen {
        return parse_error!(self.region, "Spread must be last element", &pos_range!(start, end));
      }
    }
    let p = next_parse!(self, self.parse_formal_parameter_list(builder, is_generator))?;
    if self.cur() == Token::Comma {
      self.advance()?;
      if self.cur() == Token::Spread {
        append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
        let start = self.source_position().clone();
        let rp = next_parse!(self, self.parse_function_rest_parameter(builder))?;
        let end = self.prev_source_position().clone();
        self
          .expression_context
          .to_arrow_ctx_mut()
          .map(|ctx| ctx.set_is_simple_parameter(false));
        builder.exprs_push(p, rp);
        if self.cur() == Token::Comma {
          self.advance()?;
        }
        if self.cur() != Token::RightParen {
          return parse_error!(self.region, "Spread must be last element", &pos_range!(start, end));
        }
      }
    }
    return Ok(p);
  }

  fn parse_formal_parameter_list<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_generator: bool,
  ) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    let mut params = Vec::<Expr>::new();
    loop {
      match self.cur() {
        Token::Identifier | Token::Yield | Token::Await => {
          if self.cur() == Token::Yield && is_generator {
            return parse_error!(self.region, "Yield not allowed here", self.source_position());
          }
          if self.expression_context.to_arrow_ctx_mut().is_ok() {
            let value = self.value().clone();
            let pos = self.source_position().clone();
            let ctx = self.expression_context.to_arrow_ctx_mut().unwrap();
            ctx.new_var(value, pos);
          }
          let be = next_parse!(self, self.parse_binding_element(builder))?;
          builder.push_expr(&mut params, be);
        }
        Token::LeftBrace | Token::LeftBracket => {
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
          let be = next_parse!(self, self.parse_binding_element(builder))?;
          builder.push_expr(&mut params, be);
        }
        Token::Comma => {
          self.advance()?;
        }
        Token::Super => {
          return parse_error!(self.region, "Super not allowed here", self.source_position());
        }
        _ => {
          return Ok(build!(@pos, builder, expressions, start_pos, params).into());
        }
      };
    }
    unreachable!();
  }

  fn parse_function_rest_parameter<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Spread);
    self.advance()?;
    match self.cur() {
      Token::LeftBrace | Token::LeftBracket => {
        return next_parse!(self, self.parse_binding_pattern(builder));
      }
      _ => {
        return next_parse!(
          self,
          self.parse_single_name_binding(builder, ParserConstraints::Initializer)
        )
      }
    }
  }

  fn parse_function_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    return next_parse!(self, self.parse_statement_list(builder, |t| t != Token::RightBrace));
  }

  fn parse_concise_body<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_async: bool,
    is_simple_parameter: bool,
    args: Expr,
  ) -> ParseResult<Expr> {
    let has_brace = self.cur() == Token::LeftBrace;
    let mut scope = self.declare_child_scope(ScopeFlag::TRANSPARENT);
    scope.set_is_simple_parameter(is_simple_parameter);
    let mut this = scoped!(self, |this| {
      this.escape_scope(scope);
    });
    let mut function_body_start = 0_u32;
    let mut function_body_end = 0_u32;
    let body = if has_brace {
      this.advance()?;
      next_parse!(this, this.parse_directive_prologue(scope))?;
      if this.expression_context.is_maybe_immediate_function() || this.parser_option.disable_skip_parser {
        Some(next_parse!(this, this.parse_statement_list(builder, |t| t != Token::RightBrace)).map(|t| t.into())?)
      } else {
        function_body_start = this.scanner.source_index() as u32;
        let skip_tree_builder = this.skip_tree_builder;
        next_parse!(
          this,
          this.parse_statement_list(skip_tree_builder, |t| t != Token::RightBrace)
        )?;
        function_body_end = this.scanner.source_index() as u32;
        None
      }
    } else {
      Some(next_parse!(this, this.parse_assignment_expression(builder)).map(|t| t.into())?)
    };
    this.escape_scope(scope);
    if has_brace {
      this.advance()?;
    }
    return Ok(build!(
      @runtime_pos,
      builder,
      function,
      args.source_position(),
      is_async,
      None,
      FunctionType::NonScoped,
      scope,
      FunctionAccessor::None,
      args,
      body,
      function_body_start,
      function_body_end
    ));
  }

  fn parse_method_definition<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    let is_getter = self.contextual_keyword() == Token::Get;
    let is_setter = self.contextual_keyword() == Token::Set;
    let mut is_generator = self.cur() == Token::OpMul;
    if ((is_getter || is_setter) && self.cur().one_of(&[Token::Identifier, Token::LeftBracket])) || is_generator {
      self.advance()?;
    }

    match self.cur() {
      Token::NumericLiteral
      | Token::ImplicitOctalLiteral
      | Token::StringLiteral
      | Token::LeftBracket
      | Token::Identifier => {
        let mut maybe_name: Option<Expr> = None;
        let mut formal_parameters: Option<Expr> = None;
        let mut is_async = self.cur() == Token::Identifier && self.contextual_keyword() == Token::Async;
        if is_async {
          self.advance()?;
        }
        if self.cur() == Token::OpMul {
          is_generator = true;
          self.advance()?;
          self.parser_state.push_state(ParserState::InAsyncGeneratorFunction);
        }
        maybe_name = Some(next_parse!(self, self.parse_property_name(builder))?);

        let param_start_pos = self.source_position().clone();
        let mut var_list = Vec::<(Vec<u16>, SourcePosition)>::new();
        expect!(self, self.cur(), Token::LeftParen);
        let ec = self.new_arrow_context();
        if is_getter {
          if self.cur() != Token::RightParen {
            return parse_error!(
              self.region,
              "Getter must not have any formal parameters",
              self.source_position()
            );
          }
          self.advance()?;
          formal_parameters = Some(build!(@pos, builder, expressions, param_start_pos, Vec::new()).into());
        } else if is_setter {
          if self.cur() == Token::RightParen {
            return parse_error!(
              self.region,
              "Setter must have exactly one formal parameter",
              self.source_position()
            );
          }
          formal_parameters = Some(next_parse!(self, self.parse_property_set_parameter_list(builder))?);
          var_list.append(self.expression_context.to_arrow_ctx_mut_unchecked().var_list_mut());
          if self.cur() != Token::RightParen {
            return parse_error!(
              self.region,
              "Setter must have exactly one formal parameter",
              self.source_position()
            );
          }
          self.advance()?;
        } else {
          formal_parameters = Some(next_parse!(self, self.parse_formal_parameters(builder, is_generator))?);
          self.invalidate_unique_parameter(None);
          let maybe_error = self.invalidate_arrow_parameters(&self.expression_context);
          if let Err(e) = maybe_error {
            return Err(e);
          }
          var_list.append(self.expression_context.to_arrow_ctx_mut_unchecked().var_list_mut());
          expect!(self, self.cur(), Token::RightParen);
        }

        expect!(self, self.cur(), Token::LeftBrace);
        let scope = self.declare_child_scope(ScopeFlag::OPAQUE);
        self.current_scope.declare_vars(&var_list);
        self
          .current_scope
          .set_is_simple_parameter(self.expression_context.to_arrow_ctx_unchecked().is_simple_parameter());
        self.set_expression_context(ec);

        let mut this = scoped!(self, |this| {
          this.escape_scope(scope);
        });
        next_parse!(this, this.parse_directive_prologue(scope))?;
        let name = maybe_name.unwrap();
        let formal_params = formal_parameters.unwrap();
        let function_body_start = if this.parser_option.disable_skip_parser {
          0_u32
        } else {
          this.scanner.source_index() as u32
        };
        let body = if this.parser_option.disable_skip_parser {
          let ast_builder = this.ast_builder;
          Some(next_parse!(this, this.parse_function_body(ast_builder))?.into())
        } else {
          let skip_tree_builder = this.skip_tree_builder;
          next_parse!(this, this.parse_function_body(skip_tree_builder))?;
          None
        };
        if this.current_scope.first_super_call_position().is_some() {
          let pos = this.current_scope.first_super_call_position().unwrap().clone();
          return parse_error!(this.region, "Super not allowed here", &pos);
        }
        let function_body_end = if this.parser_option.disable_skip_parser {
          0_u32
        } else {
          this.scanner.source_index() as u32
        };
        expect!(this, this.cur(), Token::RightBrace);
        this.escape_scope(scope);
        let accessor_type = if is_getter {
          FunctionAccessor::Getter
        } else if is_setter {
          FunctionAccessor::Setter
        } else {
          FunctionAccessor::None
        };
        let mut function = build!(
          @pos,
          builder,
          function,
          start,
          is_async,
          Some(name),
          if is_generator {
            FunctionType::Generator
          } else {
            FunctionType::Scoped
          },
          scope,
          accessor_type,
          formal_params,
          body,
          function_body_start,
          function_body_end
        );
        if is_async && is_generator {
          this.parser_state.pop_state(ParserState::InAsyncGeneratorFunction);
        }
        return Ok(build!(@pos, builder, object_property_expression, start, name, Some(function), None));
      }
      _ => unreachable!(),
    };
  }

  fn parse_property_set_parameter_list<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    let mut params = Vec::new();
    match self.cur() {
      Token::Identifier | Token::LeftBrace | Token::LeftBracket => {
        if self.cur() == Token::Identifier {
          if self.expression_context.to_arrow_ctx_mut().is_ok() {
            let value = self.value().clone();
            let pos = self.source_position().clone();
            let ctx = self.expression_context.to_arrow_ctx_mut().unwrap();
            ctx.new_var(value, pos);
          }
        } else {
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
        }
        let param = next_parse!(self, self.parse_binding_element(builder))?;
        builder.push_expr(&mut params, param);
        return Ok(build!(@pos, builder, expressions, start_pos, params));
      }
      _ => {
        return parse_error!(self.region, "Unexpected token found", self.source_position());
      }
    };
    unreachable!();
  }

  fn parse_generator_method<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_generator_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_generator_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_generator_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_yield_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Yield);
    let has_termination = self.cur() == Token::Terminate;
    if has_termination || self.has_line_break_before() {
      if has_termination {
        self.advance()?;
      }
      let empty = self.empty.into();
      return Ok(build!(
        @pos,
        builder,
        unary_expression,
        pos,
        UnaryExpressionOperandPosition::Pre,
        Token::Yield,
        empty
      ));
    }

    if self.cur() == Token::OpMul {
      self.advance()?;
      let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
      return Ok(build!(
        @pos,
        builder,
        unary_expression,
        pos,
        UnaryExpressionOperandPosition::Pre,
        Token::YieldAggregator,
        expr
      ));
    }

    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    return Ok(build!(
      @pos,
      builder,
      unary_expression,
      pos,
      UnaryExpressionOperandPosition::Pre,
      Token::Yield,
      expr
    ));
  }

  fn parse_await_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::Await);
    let expr = next_parse!(self, self.parse_unary_expression(builder))?;
    return Ok(build!(
      @pos,
      builder,
      unary_expression,
      start,
      UnaryExpressionOperandPosition::Pre,
      Token::Await,
      expr
    ));
  }

  fn parse_class_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_class_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_class_tail<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_class_heritage<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_class_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_class_element_list<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_class_element<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_module_item<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    match self.cur() {
      Token::Import => return next_parse!(self, self.parse_import_declaration(builder)),
      Token::Export => return next_parse!(self, self.parse_export_declaration(builder)),
      _ => return next_parse!(self, self.parse_statement_list_item(builder)),
    }
  }

  fn parse_import_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    debug_assert!(self.cur() == Token::Import);
    let mut start = self.source_position().clone();
    self.advance()?;
    if self.cur() == Token::StringLiteral {
      let val = LiteralValue::String(
        FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
        Token::Invalid,
      );
      let t = self.cur();
      let str = build!(@pos, builder, literal, start, t, val);
      return Ok(build!(@pos, builder, import_decl, start, None, str));
    }

    let mut default_binding_node = None;
    start = self.source_position().clone();
    if self.cur() == Token::Identifier {
      let default_binding = next_parse!(self, self.parse_identifier(builder, ParserConstraints::None))?;
      self.advance()?;
      default_binding_node = Some(default_binding);
    }

    let mut namespace_import = None;
    let mut named_import_list = None;
    match self.cur() {
      Token::LeftBrace => {
        named_import_list = Some(next_parse!(self, self.parse_named_import(builder))?);
      }
      Token::OpMul => {
        namespace_import = Some(next_parse!(self, self.parse_name_space_import(builder))?);
      }
      _ => return parse_error!(self.region, "Unexpected token", self.source_position()),
    }
    let ib = build!(@pos, builder, import_binding, start, default_binding_node, namespace_import, named_import_list);

    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::From {
      self.advance()?;
      if self.cur() == Token::StringLiteral {
        let val = LiteralValue::String(
          FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
          Token::Invalid,
        );
        let t = self.cur();
        let str = build!(self, builder, literal, t, val);
        if self.cur() == Token::Terminate {
          self.advance()?;
        } else if !self.has_line_break_before() {
          return parse_error!(self.region, "';' expected", self.source_position());
        }
        return Ok(build!(self, builder, import_decl, Some(ib.into()), str));
      }
    }

    return parse_error!(self.region, "Module specifier expected", self.source_position());
  }

  fn parse_name_space_import<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Export);
    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::As {
      return Ok(build!(self, builder, import_specifier, true, None, None));
    }
    return parse_error!(self.region, "'as' expected", self.source_position());
  }

  fn parse_named_import<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list(builder));
  }

  fn parse_export_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    expect!(self, self.cur(), Token::Export);
    let mut export_type = ExportDeclarationType::NamespaceExport;
    let mut export_clause: Option<Ast> = None;
    if self.cur() == Token::Default {
      export_type = ExportDeclarationType::DefaultExport;
      self.advance()?;
      match self.cur() {
        Token::Class => {
          export_clause = Some(next_parse!(self, self.parse_class_declaration(builder))?.into());
        }
        Token::Function => {
          export_clause = Some(next_parse!(self, self.parse_function_declaration(builder, false, true))?.into());
        }
        Token::Identifier => {
          export_clause = Some(next_parse!(self, self.parse_assignment_expression(builder))?.into());
        }
        _ => {
          export_clause = Some(next_parse!(self, self.parse_assignment_expression(builder))?.into());
        }
      }

      return Ok(build!(self, builder, export_decl, export_type, export_clause, None));
    }

    match self.cur() {
      Token::Var => {
        export_clause = Some(next_parse!(self, self.parse_variable_statement(builder))?.into());
      }
      Token::Const | Token::Function | Token::Class => {
        export_clause = Some(next_parse!(self, self.parse_declaration(builder))?.into());
      }
      Token::Identifier => {
        if self.contextual_keyword() == Token::Let {
          export_clause = Some(next_parse!(self, self.parse_declaration(builder))?.into());
        }
        return parse_error!(self.region, "Unexpected token found", self.source_position());
      }
      _ => {
        export_clause = Some(next_parse!(self, self.parse_export_clause(builder))?.into());
      }
    }

    let mut from_clause: Option<Ast> = None;
    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::From {
      self.advance()?;
      if self.cur() == Token::StringLiteral {
        let val = LiteralValue::String(
          FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
          Token::Invalid,
        );
        from_clause = Some(build!(self, builder, literal, Token::StringLiteral, val).into());
      } else {
        return parse_error!(self.region, "Module specifier expected", self.source_position());
      }
    }

    if self.cur() == Token::Terminate {
      self.advance()?;
    } else if self.has_line_break_after() {
      return parse_error!(self.region, "';' expected", self.source_position());
    }

    return Ok(build!(
      self,
      builder,
      export_decl,
      export_type,
      export_clause,
      from_clause
    ));
  }

  fn parse_export_clause<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list(builder));
  }

  fn parse_named_list<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftBrace);
    let mut list = Vec::<Expr>::new();

    while self.has_more() && self.cur() != Token::RightBrace {
      let identifier = next_parse!(
        self,
        self.parse_identifier_reference(builder, ParserConstraints::KeywordIdentifier)
      )?;
      let node = if self.cur() == Token::Identifier && self.contextual_keyword() == Token::As {
        self.advance()?;
        let value_ref = next_parse!(
          self,
          self.parse_identifier(builder, ParserConstraints::KeywordIdentifier)
        )?;
        build!(
          self,
          builder,
          import_specifier,
          false,
          Some(identifier),
          Some(value_ref)
        )
      } else {
        build!(self, builder, import_specifier, false, Some(identifier), None)
      };
      builder.push_expr(&mut list, node);

      if self.cur() == Token::Comma {
        self.advance()?;
      } else if self.cur() != Token::RightBrace {
        return parse_error!(self.region, "'}' expected", self.source_position());
      }
    }

    return Ok(build!(@pos, builder, named_import_list, start, list).into());
  }
}
