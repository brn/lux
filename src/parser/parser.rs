use super::super::structs::FixedU16CodePointArray;
use super::ast::*;
use super::ast_builder::*;
use super::error_formatter::*;
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
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use termion::{color, style};

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
    expect!(@notadvance @oneof $self, $n, $($token,)*);
    $self.advance()?;
  };
  (@notadvance @oneof $self:tt, $n:expr, $($token:expr,)+) => {
    if !$n.one_of(&[$($token,)*]) {
      let mut s = String::new();
      $(
        s.push_str(&format!("'{}' ", $token.symbol()));
      )*
      s.push_str("expected");
      let pos = $self.source_position().clone();
      return parse_error!($self.region, s, &pos);
    }
  }
}

#[cfg(feature = "print_ast")]
struct DebugInfo {
  is_enter: bool,
  parser_name: &'static str,
  value: String,
  ctx: ExprCtx,
  token: Token,
  position: SourcePosition,
  is_success: bool,
}

#[cfg(feature = "print_ast")]
struct ParserDebugger {
  #[cfg(feature = "print_ast_when_called")]
  indent: String,
  debug_info_list: Vec<DebugInfo>,
}

macro_rules! _color_debugger_parts {
  ($format:expr, $value:expr) => {
    format!(
      concat!("{}{}[{}", $format, "{}{}]{}"),
      style::Bold,
      color::Fg(color::Blue),
      style::Reset,
      format!("{}{:?}{}", style::Bold, $value, style::Reset),
      style::Bold,
      color::Fg(color::Blue),
      style::Reset
    )
  };
}

bitflags! {
  struct DeclarationParseOption: u8 {
    const NONE = 0;
    const IGNORE_TERMINATION = 1;
    const CONST_INITIALIZER_REQUIRED = 2;
    const EXPORT_RHS = 0x4;
    const DEFAULT_EXPORT_RHS = 0x8;
  }
}
impl DeclarationParseOption {
  fn from_stmt() -> Self {
    return DeclarationParseOption::CONST_INITIALIZER_REQUIRED;
  }

  fn from_named_export_stmt() -> Self {
    return DeclarationParseOption::CONST_INITIALIZER_REQUIRED
      | DeclarationParseOption::IGNORE_TERMINATION
      | DeclarationParseOption::EXPORT_RHS;
  }
}

#[cfg(feature = "print_ast")]
impl ParserDebugger {
  fn new() -> Self {
    ParserDebugger {
      #[cfg(feature = "print_ast_when_called")]
      indent: "".to_string(),
      debug_info_list: Vec::new(),
    }
  }

  fn enter(&mut self, parser_name: &'static str, value: &str, ctx: &ExprCtx, token: Token, position: &SourcePosition, has_error: bool) {
    self.debug_info_list.push(DebugInfo {
      is_enter: true,
      parser_name,
      value: value.to_string().clone(),
      ctx: ctx.clone(),
      token,
      position: position.clone(),
      is_success: true,
    });
    #[cfg(feature = "print_ast_when_called")]
    {
      self.print_enter(self.debug_info_list.last().as_ref().unwrap(), &self.indent);
      self.indent = format!("  {}", self.indent);
    }
  }

  fn leave<T>(&mut self, parser_name: &'static str, ctx: &ExprCtx, position: &SourcePosition, token: Token, result: &ParseResult<T>) {
    self.debug_info_list.push(DebugInfo {
      is_enter: false,
      parser_name,
      value: String::new(),
      ctx: ctx.clone(),
      token,
      position: position.clone(),
      is_success: result.is_ok(),
    });
    #[cfg(feature = "print_ast_when_called")]
    {
      self.indent = self.indent.chars().take(self.indent.len() - 2).collect::<String>();
      self.print_leave(self.debug_info_list.last().as_ref().unwrap(), &self.indent);
    }
  }

  fn print_stack_trace(&self) {
    let mut indent = String::new();
    for debug_info in self.debug_info_list.iter().as_ref() {
      match debug_info.is_enter {
        true => {
          self.print_enter(debug_info, &indent);
          indent = format!("  {}", indent);
        }
        _ => {
          indent = indent.chars().take(indent.len() - 2).collect::<String>();
          self.print_leave(debug_info, &indent);
        }
      }
    }
  }

  fn print_enter(&self, debug_info: &DebugInfo, indent: &str) {
    let format_prologue = format!("{}", indent);
    let next_parse_name = format!(
      "{}{}Enter{} {}:",
      style::Bold,
      color::Fg(color::LightBlue),
      style::Reset,
      debug_info.parser_name,
    );
    let cur_token = _color_debugger_parts!("CurrentToken = {}", debug_info.token);
    let value_part = _color_debugger_parts!(
      "value = {}",
      if debug_info.value.len() == 0 {
        debug_info.token.symbol()
      } else {
        &debug_info.value
      }
    );
    let pos_part = _color_debugger_parts!("{}", debug_info.position);
    let ctx_part = _color_debugger_parts!("{}", debug_info.ctx);
    println!(
      "{}{} {} {} {} {}",
      format_prologue, next_parse_name, cur_token, value_part, pos_part, ctx_part
    );
  }

  fn print_leave(&self, debug_info: &DebugInfo, indent: &str) {
    let format_prologue = format!("{}", indent);
    let last_parse_name = format!(
      "{}{}Exit{} {}:",
      style::Bold,
      color::Fg(color::LightBlue),
      style::Reset,
      debug_info.parser_name,
    );
    let result = if debug_info.is_success {
      format!("{}{}Success{}", style::Bold, color::Fg(color::Green), style::Reset)
    } else {
      format!("{}{}Failure{}", style::Bold, color::Fg(color::Red), style::Reset)
    };
    let cur_token = _color_debugger_parts!("CurrentToken = {}", debug_info.token);
    let pos_part = _color_debugger_parts!("{}", debug_info.position);
    let ctx_part = _color_debugger_parts!("{}", debug_info.ctx);
    println!(
      "{}{} {} {} {} {}",
      format_prologue, last_parse_name, result, cur_token, pos_part, ctx_part
    );
  }
}

#[cfg(feature = "print_ast")]
macro_rules! next_parse {
  ($self:tt, $call:expr) => {{
    let cur = $self.cur();
    let source_position = $self.source_position().clone();
    let has_pending_error = $self.error_reporter.has_pending_error();
    let ctx = $self.expression_context.clone();
    let val = Parser::value_to_utf8($self.value());
    $self
      .debugger
      .enter(stringify!($call), &val, &ctx, cur, &source_position, has_pending_error);
    let result = $call;
    let cur = $self.cur();
    let pos = $self.source_position().clone();
    let ctx = $self.expression_context.clone();
    $self.debugger.leave(stringify!($call), &ctx, &pos, cur, &result);
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

macro_rules! next_skip_parse {
  ($self:expr, $call:expr) => {{
    $self.use_skip_tree_builder();
    let r = next_parse!($self, $call);
    $self.finish_skip_parsing();
    r
  }};
}

#[derive(Clone, Property, Debug)]
pub struct ParserOption {
  #[property(get(type = "copy"), set(disable))]
  disable_skip_parser: bool,

  #[property(get(type = "copy"), set(disable))]
  should_parse_with_skip_parser: bool,

  #[property(get(type = "copy"), set(disable))]
  allow_undefined_named_module: bool,

  #[property(get(type = "copy"), set(disable))]
  is_root_super_allowed: bool,

  #[property(get(type = "copy"), set(disable))]
  is_root_new_target_allowed: bool,

  #[property(get(type = "copy"), set(disable))]
  is_strict_mode: bool,
}
impl ParserOption {
  pub fn with_disable_skip_parser(mut self) -> Self {
    self.disable_skip_parser = true;
    return self;
  }

  pub fn with_skip_parser(mut self) -> Self {
    self.should_parse_with_skip_parser = true;
    return self;
  }

  pub fn with_allow_super(mut self) -> Self {
    self.is_root_super_allowed = true;
    return self;
  }

  pub fn with_allow_new_target(mut self) -> Self {
    self.is_root_new_target_allowed = true;
    return self;
  }
}
impl Default for ParserOption {
  fn default() -> Self {
    ParserOption {
      disable_skip_parser: false,
      should_parse_with_skip_parser: false,
      allow_undefined_named_module: false,
      is_root_new_target_allowed: false,
      is_root_super_allowed: false,
      is_strict_mode: false,
    }
  }
}

pub struct ParserOptionBuilder {
  pub disable_skip_parser: bool,
  pub should_parse_with_skip_parser: bool,
  pub allow_undefined_named_module: bool,
  pub is_root_super_allowed: bool,
  pub is_root_new_target_allowed: bool,
  pub is_strict_mode: bool,
}
impl Default for ParserOptionBuilder {
  fn default() -> Self {
    ParserOptionBuilder {
      disable_skip_parser: false,
      should_parse_with_skip_parser: false,
      is_root_new_target_allowed: false,
      is_root_super_allowed: false,
      is_strict_mode: false,
      allow_undefined_named_module: false,
    }
  }
}
impl ParserOptionBuilder {
  pub fn build(&self) -> ParserOption {
    ParserOption {
      disable_skip_parser: self.disable_skip_parser,
      should_parse_with_skip_parser: self.should_parse_with_skip_parser,
      is_root_new_target_allowed: self.is_root_new_target_allowed,
      is_root_super_allowed: self.is_root_super_allowed,
      is_strict_mode: self.is_strict_mode,
      allow_undefined_named_module: self.allow_undefined_named_module,
    }
  }
}

#[derive(PartialEq, Clone, Copy)]
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
  constructor_str: FixedU16CodePointArray,
  root_scope: Exotic<Scope>,
  current_scope: Exotic<Scope>,
  expression_context: ExprCtx,
  statement_block: StatementBlock,
  ast_builder: Exotic<AstBuilder>,
  skip_tree_builder: Exotic<SkipTreeBuilder>,
  err: SyntaxErrorFactory,
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

macro_rules! scoped_expr_ctx {
  ($self:expr, $item:block) => {{
    let p = $self.new_expression_context();
    let r = $item;
    $self.set_expression_context(p);
    r
  }};
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
      scanner: region.alloc(Scanner::new(region.clone(), source.clone(), parser_state, error_reporter)),
      region: region.clone(),
      result: Ok(empty.into()),
      source: source.clone(),
      scanner_record: None,
      error_reporter,
      debugger: ParserDebugger::new(),
      empty,
      use_strict_str: FixedU16CodePointArray::from_utf8(context, "use strict"),
      constructor_str: FixedU16CodePointArray::from_utf8(context, "constructor"),
      root_scope: Scope::new(
        region.clone(),
        ScopeFlag::OPAQUE
          | ScopeFlag::ROOT_SCOPE
          | if parser_option.is_root_super_allowed() {
            ScopeFlag::ALLOW_SUPER_CALL | ScopeFlag::ALLOW_SUPER_PROPERTY
          } else {
            ScopeFlag::NONE
          }
          | if parser_option.is_strict_mode() {
            ScopeFlag::STRICT_MODE
          } else {
            ScopeFlag::NONE
          }
          | if parser_option.is_root_new_target_allowed() {
            ScopeFlag::ALLOW_NEW_TARGET
          } else {
            ScopeFlag::NONE
          },
      ),
      current_scope: Exotic::new(std::ptr::null_mut()),
      expression_context: ExprCtx::Expr(ExpressionContext::new()),
      ast_builder: region.alloc(AstBuilder::new(region.clone())),
      skip_tree_builder: region.alloc(SkipTreeBuilder::new(region.clone())),
      err: SyntaxErrorFactory::new(region.clone()),
      parser_option,
      statement_block: StatementBlock::new(),
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
    if self.parser_type == ParserType::Module {
      self.root_scope.mark_as_strict_mode();
    }
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
        return self.err.invalid_token_found(self.prev_source_position().clone());
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
        return self.err.invalid_token_found(self.prev_source_position().clone());
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
    let context = if ec.is_some() { ec.unwrap() } else { &mut self.expression_context };
    if let Ok(ctx) = context.to_arrow_ctx_mut() {
      let mut unique_set = HashSet::<&Vec<u16>>::new();
      let mut error = None;
      for (ref var, ref pos) in ctx.var_list().iter() {
        if unique_set.contains(var) {
          error = Some(self.err.duplicate_parameter_not_allowed_here_raw(pos.clone()));
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
    if self.expression_context.to_arrow_ctx().is_ok() {
      return self.new_arrow_context();
    };
    let ctx = ExprCtx::Expr(self.expression_context.create_child_context());
    return std::mem::replace(&mut self.expression_context, ctx);
  }

  #[inline(always)]
  fn new_statement_block(&mut self) -> StatementBlock {
    let next_block = self.statement_block.next_block();
    return std::mem::replace(&mut self.statement_block, next_block);
  }

  #[inline(always)]
  fn set_statement_block(&mut self, stmt_block: StatementBlock) -> StatementBlock {
    return std::mem::replace(&mut self.statement_block, stmt_block);
  }

  #[inline(always)]
  fn new_arrow_context(&mut self) -> ExprCtx {
    let child = self.expression_context.create_child_arrow_context();
    return std::mem::replace(&mut self.expression_context, ExprCtx::Arrow(child));
  }

  #[inline(always)]
  fn set_expression_context(&mut self, ec: ExprCtx) -> ExprCtx {
    return std::mem::replace(&mut self.expression_context, ec);
  }

  #[inline(always)]
  fn set_expression_context_with_propagation(&mut self, mut ec: ExprCtx) -> ExprCtx {
    self.expression_context.propagate(&mut ec);
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

  #[inline]
  fn declare_identifir_var_if(&mut self) -> bool {
    if self.cur() == Token::Identifier {
      let value = self.value().clone();
      let pos = self.source_position().clone();
      if let Ok(ctx) = self.expression_context.to_arrow_ctx_mut() {
        ctx.new_var(value, pos);
      }
      return true;
    }
    return false;
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
    return decode_utf16(value.iter().cloned()).map(|r| r.unwrap_or('#')).collect::<String>();
  }

  fn utf8_to_utf16(value: &str) -> Vec<u16> {
    return value.encode_utf16().collect::<Vec<_>>();
  }

  fn is_strict_mode(&self) -> bool {
    return self.root_scope.is_strict_mode()
      || self.current_scope.is_strict_mode()
      || self.parser_state.is_in_state(ParserState::InClassScope);
  }

  #[inline(always)]
  fn is_eol(&self, token: Token) -> bool {
    return token.one_of(&[Token::End, Token::RightBrace]) || self.has_line_break_before();
  }

  #[cfg(feature = "print_ast")]
  pub fn print_stack_trace(&self) {
    self.debugger.print_stack_trace();
  }

  fn should_use_tree_builder(&self) -> bool {
    return (!self.parser_state.is_in_state(ParserState::InSkipParsing) && self.expression_context.is_maybe_immediate_function())
      || self.parser_option.disable_skip_parser;
  }

  fn use_skip_tree_builder(&mut self) {
    self.parser_state.enter_state(ParserState::InSkipParsing)
  }

  fn finish_skip_parsing(&mut self) {
    self.parser_state.leave_state(ParserState::InSkipParsing)
  }

  fn parse_directive_prologue(&mut self, mut scope: Exotic<Scope>) -> ParseResult<Expr> {
    if self.cur() == Token::StringLiteral && self.is_value_match_with(self.use_strict_str) {
      if !self.current_scope.is_simple_parameter() {
        return self.err.use_strict_after_non_simple_param(self.source_position().clone());
      }
      self.advance()?;
      scope.mark_as_strict_mode();
      self.parser_state.enter_state(ParserState::InStrictMode);
      if self.cur() == Token::Terminate {
        self.advance()?;
      } else if !self.has_line_break_before() && !self.cur().one_of(&[Token::RightBrace, Token::End]) {
        return self.err.unexpected_token_found(self.source_position().clone());
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

    if self.result.is_ok() {
      if let Some(pos) = self.root_scope.first_super_call_position() {
        if !self.parser_option.is_root_super_allowed() {
          self.result = parse_error!(self.region, "Super not allowed here", &pos);
        }
      } else if let Some(pos) = self.root_scope.get_unfilled_will_be_exported_var() {
        if !self.parser_option.allow_undefined_named_module() {
          self.result = parse_error!(self.region, "exported variable is not defined", &pos);
        }
      }
    }

    if self.result.is_err() {
      let ed = self.result.as_ref().unwrap_err().clone();
      self.error_reporter.report_syntax_error(ed);
    }
  }

  fn parse_script(&mut self) -> ParseResult<Ast> {
    if !self.parser_option.should_parse_with_skip_parser() {
      return next_parse!(self, self.parse_statement_list(self.ast_builder, |_| true)).map(|a| Into::<Ast>::into(a));
    } else {
      return next_parse!(self, self.parse_statement_list(self.skip_tree_builder, |_| true)).map(|a| Into::<Ast>::into(a));
    }
  }

  fn parse_module(&mut self) -> ParseResult<Ast> {
    if !self.parser_option.should_parse_with_skip_parser() {
      return next_parse!(self, self.parse_module_body(self.ast_builder)).map(|a| Into::<Ast>::into(a));
    } else {
      return next_parse!(self, self.parse_module_body(self.skip_tree_builder)).map(|a| Into::<Ast>::into(a));
    }
  }

  fn parse_module_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let mut items = Vec::<Stmt>::new();
    while self.has_more() {
      let item = next_parse!(self, self.parse_module_item(builder))?;
      if Node::<Empty>::is(item) {
        continue;
      }
      builder.push_stmt(&mut items, item);
    }
    return Ok(build!(@pos, builder, statements, start, items).into());
  }

  fn parse_terminator<T>(&mut self, expr: T) -> ParseResult<T> {
    if self.cur() == Token::Terminate {
      self.advance()?;
      return Ok(expr);
    } else if !self.is_eol(self.cur()) {
      return parse_error!(self.region, "';' expected", self.source_position());
    }

    return Ok(expr);
  }

  fn parse_identifier<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, constraints: ParserConstraints) -> ParseResult<Expr> {
    use Token::*;
    if !constraints.is_keyword_identifier_allowed() {
      if self
        .contextual_keyword()
        .one_of(&[Implements, Interface, Let, Package, Private, Protected, Public, Static])
      {
        return parse_error!(
          self.region,
          format!("'{}' is reserved word", Parser::value_to_utf8(self.value())),
          self.source_position()
        );
      }
      if !self.cur().one_of(&[Token::Identifier, Token::Yield, Token::Await]) {
        return parse_error!(self.region, "Identifier expected", self.source_position());
      }
    }

    self.expression_context.set_assignment_target_type(AssignmentTargetType::Simple);

    if self.contextual_keyword().one_of(&[Token::Eval, Token::Arguments]) {
      let error = self.err.eval_or_arguments_in_strict_mode_code_raw(self.source_position().clone());
      if self.is_strict_mode() {
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      }
      self.expression_context.set_first_strict_mode_error(error);
    }

    let val = if self.cur() == Token::Identifier {
      LiteralValue::String(
        FixedU16CodePointArray::from_u16_vec(self.context, self.value()),
        self.contextual_keyword(),
      )
    } else {
      LiteralValue::String(FixedU16CodePointArray::from_utf8(self.context, self.cur().symbol()), self.cur())
    };

    return Ok(build!(self, builder, literal, Token::Identifier, val));
  }

  fn parse_identifier_reference<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
  ) -> ParseResult<Expr> {
    let result = match self.cur() {
      Token::Identifier | Token::Yield => {
        if !constraints.is_keyword_identifier_allowed() {
          if (self.is_strict_mode() || self.current_scope.is_generator_context()) && self.cur() == Token::Yield {
            return parse_error!(self.region, "Keyword 'yield' is not allowed here", self.source_position());
          }
          if self.current_scope.is_async_context() && self.cur() == Token::Await {
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
    self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
        return next_parse!(self, self.parse_function_expression(builder, FunctionAttribute::NONE));
      }
      Token::Class => {
        return next_parse!(self, self.parse_class_expression(builder));
      }
      Token::Identifier | Token::Await | Token::Yield => {
        if self.contextual_keyword() == Token::Async && !self.has_line_break_after() && self.peek()? == Token::Function {
          return next_parse!(self, self.parse_function_expression(builder, FunctionAttribute::ASYNC));
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
        let parent_ctx = self.new_expression_context();
        if self.peek()?.one_of(&[Token::Function, Token::LeftParen]) || self.peek_contextual_keyword() == Token::Async {
          self.expression_context.set_is_maybe_immediate_function(true);
        }
        let r = next_parse!(self, self.parse_cover_parenthesized_expression_and_arrow_parameter_list(builder));
        self.set_expression_context_with_propagation(parent_ctx);
        return r;
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
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()), Token::Invalid);
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

  fn parse_array_literal<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, constraints: ParserConstraints) -> ParseResult<Expr> {
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
          if let Some(e) = self.expression_context.first_value_error() {
            return Err(e);
          }
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
    self.expression_context.set_assignment_target_type(AssignmentTargetType::Simple);
    return Ok(array.into());
  }

  fn parse_spread_element<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    debug_assert!(self.cur() == Token::Spread);
    self.advance()?;
    let maybe_identifier_value = self.value().clone();
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    if let Some(e) = self.expression_context.first_value_error() {
      return Err(e);
    }
    let expr_end_pos = self.prev_source_position().clone();
    if builder.is_identifier(expr) {
      if let Ok(arrow_ctx) = self.expression_context.to_arrow_ctx_mut() {
        arrow_ctx.new_var(maybe_identifier_value, start_pos);
      }
    } else {
      self
        .expression_context
        .set_first_pattern_error(parse_error!(@raw, self.region, "Invalid pattern found", &pos_range!(@start start_pos, expr_end_pos)))
    }
    return Ok(build!(@pos, builder, unary_expression, start_pos,
      UnaryExpressionOperandPosition::Pre,
      Token::Spread,
      expr,
    ));
  }

  fn parse_object_literal<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, constraints: ParserConstraints) -> ParseResult<Expr> {
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
    let mut key_map = HashMap::new();
    while !self.cur().one_of(&[Token::RightBrace, Token::Invalid, Token::End]) {
      let start_pos = self.source_position().clone();
      let prop = next_parse!(self, self.parse_object_literal_property(builder, constraints, &mut key_map))?;
      if builder.is_spread_expression(prop) {
        if is_spread_seen {
          if let Ok(ctx) = self.expression_context.to_arrow_ctx_mut() {
            ctx.set_first_arrow_parameter_error(parse_error!(@raw, self.region, "Rest parameter must be last element", &start_pos));
          }
        } else {
          is_spread_seen = true;
        }
      }
      builder.push_expr(&mut properties, prop);
      if self.cur() == Token::Comma {
        self.advance()?;
      }
    }

    expect!(self, self.cur(), Token::RightBrace);
    self.expression_context.set_assignment_target_type(AssignmentTargetType::Simple);
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

  fn invalidate_object_literal_key_duplication(&mut self, key_map: &mut HashMap<Vec<u16>, SourcePosition>, value: Option<&Vec<u16>>) {
    if self.expression_context.first_value_error().is_some() {
      return;
    }
    let val = if value.is_some() { value.unwrap() } else { self.value() };
    if let Some(ref first) = key_map.get(val) {
      let pos = self.source_position().clone();
      let e = parse_error!(@raw, self.region, &format!(
        "Object literal may not contains any duplicated key\n\n{}\nBut found",
        format_error(&self.source.filename(), self.source.source_code(), "First defined", first, false)
      ), &pos);
      self.expression_context.set_first_value_error(e);
    } else {
      key_map.insert(val.clone(), self.source_position().clone());
    }
  }

  fn parse_object_literal_property<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    constraints: ParserConstraints,
    key_map: &mut HashMap<Vec<u16>, SourcePosition>,
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

    let is_computed_property_name = self.cur() == Token::LeftBracket;
    let start = self.source_position().clone();
    let (function_attr, _) = next_parse!(self, self.parse_method_attributes(false))?;

    match self.cur() {
      Token::NumericLiteral | Token::ImplicitOctalLiteral | Token::StringLiteral | Token::LeftBracket | Token::Identifier => {
        let is_key_identifier = self.cur() == Token::Identifier;
        if self.cur().one_of(&[Token::Identifier, Token::StringLiteral]) {
          self.invalidate_object_literal_key_duplication(key_map, None);
        } else if self.cur().one_of(&[Token::NumericLiteral, Token::ImplicitOctalLiteral]) {
          let k = self.numeric_value().to_string();
          let value = Parser::utf8_to_utf16(&k);
          self.invalidate_object_literal_key_duplication(key_map, Some(&value));
        }
        let mut ident_value = if is_key_identifier { Some(self.value().clone()) } else { None };
        let key = next_parse!(self, self.parse_property_name(builder))?;
        if self.cur() == Token::LeftParen {
          let method_start = self.source_position().clone();
          let body = next_parse!(self, self.parse_method_body(builder, &start, key, function_attr, false))?;
          let method_end = self.source_position().clone();
          self.expression_context.set_first_pattern_error(
            parse_error!(@raw, self.region, "Invalid object pattern found", &pos_range!(@start method_start, method_end)),
          );
          return Ok(build!(@pos, builder, object_property_expression, start, key, Some(body), None));
        }

        if self.cur() == Token::OpAssign {
          append_var!(self, ident_value, &start);
          let start_initializer = self.source_position().clone();
          if is_computed_property_name {
            let e = parse_error!(@raw, self.region, "Unexpected '=' detected", self.source_position());
            self.expression_context.set_first_pattern_error(e);
          }
          self.advance()?;
          let parent_ctx = self.new_expression_context();
          let value = next_parse!(self, self.parse_assignment_expression(builder))?;
          if let Some(e) = self.expression_context.first_value_error() {
            return Err(e);
          }
          self.set_expression_context(parent_ctx);
          let node = build!(@pos, builder, object_property_expression, start,
                            key,
                            None,
                            Some(value),
          );
          if !is_key_identifier {
            self
              .expression_context
              .set_first_pattern_error(parse_error!(@raw, self.region, "Identifier required", &start_initializer));
          }
          self
            .expression_context
            .set_first_value_error(parse_error!(@raw, self.region, "Unexpected initializer", &start_initializer));
          return Ok(node);
        }

        if self.cur() != Token::Colon {
          if is_key_identifier {
            append_var!(self, ident_value, &start);
            return Ok(build!(@pos, builder, object_property_expression,start,
              key,
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
        let value_end_pos = self.prev_source_position().clone();
        if builder.is_identifier(value) || builder.is_initializer(value) {
          append_var!(self, ident_value, &value_start_pos);
        } else if !builder.is_structural_literal(value) {
          self.expression_context.set_first_pattern_error(
            parse_error!(@raw, self.region, "'Identifier' or 'Pattern' expected", &pos_range!(@start value_start_pos, value_end_pos)),
          );
        }

        if constraints.is_binding_pattern_allowed() {
          if let Err(e) = self.invalidate_pattern(&self.expression_context) {
            return Err(e);
          }
        }

        return Ok(build!(@pos, builder, object_property_expression, start,
          key,
          Some(value),
          None,
        ));
      }
      Token::Spread => {
        append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
        let spread = next_parse!(self, self.parse_spread_element(builder))?;
        return Ok(build!(@pos, builder, object_property_expression, start,
                         spread,
                         None,
                         None
        ));
      }
      _ => {
        return parse_error!(self.region, "Unexpected token found", self.source_position());
      }
    };
  }

  fn parse_property_name<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::Identifier => {
        let i = next_parse!(self, self.parse_identifier(builder, ParserConstraints::KeywordIdentifier));
        self.advance()?;
        return i;
      }
      Token::StringLiteral | Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        return next_parse!(self, self.parse_literal(builder));
      }
      _ => {
        if self.cur().is_allowed_in_property_name() {
          let i = next_parse!(self, self.parse_identifier(builder, ParserConstraints::KeywordIdentifier));
          self.advance()?;
          return i;
        }
        expect!(self, self.cur(), Token::LeftBracket);
        let ret = next_parse!(self, self.parse_assignment_expression(builder))?;
        if let Some(e) = self.expression_context.first_value_error() {
          return Err(e);
        }
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
          let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()), Token::Invalid);
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

    if self.cur() != Token::ArrowFunctionGlyph {
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
    }
    return Ok(expr);
  }

  fn parse_member_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();

    if self.cur() == Token::Super {
      self.advance()?;
      self.current_scope.set_first_super_property_position(&start);
      expect!(@notadvance @oneof self, self.cur(), Token::Dot, Token::LeftBracket,);
      return next_parse!(
        self,
        self.parse_post_member_expression(
          builder,
          &start.runtime_source_position(),
          self.empty.into(),
          CallReceiverType::Super,
          ParserConstraints::None,
          false
        )
      );
    }

    if self.cur() == Token::New {
      let new_start_pos = self.source_position().clone();
      self.advance()?;
      if self.cur() == Token::Dot {
        self.advance()?;
        if self.cur() == Token::Identifier {
          if self.contextual_keyword() == Token::Target {
            if !self.current_scope.is_new_target_allowed() {
              return parse_error!(self.region, "new.target is not allowed here", self.source_position());
            }
            self.advance()?;
            return Ok(build!(
              @pos,
              builder,
              property_access_expression,
              new_start_pos,
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
        return parse_error!(self.region, "new.target? identifier 'target' expected", self.source_position());
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
    let mut is_last_optional_chaining = false;
    let mut is_optional_chaining_seen = false;
    loop {
      let token = self.cur();
      match self.cur() {
        Token::LeftParen => {
          if !is_last_optional_chaining && !constraints.is_call_allowed() {
            break;
          }
          let a = next_parse!(self, self.parse_arguments(builder))?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
          if !is_optional_chaining_seen {
            self.expression_context.set_assignment_target_type(AssignmentTargetType::Simple);
          }
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
          let ident = next_parse!(self, self.parse_identifier_reference(builder, ParserConstraints::KeywordIdentifier))?;
          if !is_optional_chaining_seen {
            self.expression_context.set_assignment_target_type(AssignmentTargetType::Simple);
          }
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
        Token::OpOptionalChaining => {
          is_optional_chaining_seen = true;
          self.advance()?;
          let expr = match self.cur() {
            Token::LeftBracket => {
              self.advance()?;
              let old = self.new_expression_context();
              self.parser_state.enter_state(ParserState::AllowIn);
              let expr = next_parse!(self, self.parse_expression(builder, false))?;
              self.parser_state.leave_state(ParserState::AllowIn);
              self.set_expression_context(old);
              expect!(self, self.cur(), Token::RightBracket);
              expr
            }
            Token::BackQuote => {
              return self.err.template_literal_not_allowed_after_op_chain(self.source_position().clone());
            }
            Token::Identifier => {
              let expr = next_parse!(self, self.parse_identifier(builder, ParserConstraints::KeywordIdentifier))?;
              self.advance()?;
              expr
            }
            Token::LeftParen => next_parse!(self, self.parse_arguments(builder))?,
            _ => {
              return self.err.unexpected_token_found(self.source_position().clone());
            }
          };
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          current = build!(
            @runtime_pos,
            builder,
            property_access_expression,
            source_position,
            PropertyAccessType::OptionalChaining,
            receiver_type,
            Some(current),
            Some(expr)
          );
        }
        Token::BackQuote => {
          if is_last_optional_chaining {
            return self.err.template_literal_not_allowed_after_op_chain(self.source_position().clone());
          }
          if !constraints.is_template_allowed() {
            return parse_error!(self.region, "Unexpected '`' found", self.source_position());
          }
          self.parser_state.push_state(ParserState::InTaggedTemplateLiteral);
          let tmpl = next_parse!(self, self.parse_template_literal(builder))?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
      is_last_optional_chaining = token == Token::OpOptionalChaining;
    }

    return Ok(current);
  }

  fn parse_super_call<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Super);
    let args = next_parse!(self, self.parse_arguments(builder))?;
    return Ok(build!(@pos, builder, call_expression, start_pos, CallReceiverType::Super, None, Some(args)));
  }

  fn parse_import_call<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Import);
    if self.cur() == Token::Dot {
      self.advance()?;
      if self.cur() == Token::Identifier && self.contextual_keyword() == Token::Meta {
        let n = build!(@pos, builder, import_meta, start_pos);
        self.advance()?;
        return Ok(n);
      }
      return parse_error!(self.region, "import.meta? identifier meta expected", self.source_position());
    }
    expect!(self, self.cur(), Token::LeftParen);
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    if let Some(e) = self.expression_context.first_value_error() {
      return Err(e);
    }
    expect!(self, self.cur(), Token::RightParen);
    return Ok(build!(self, builder, call_expression, CallReceiverType::Import, None, Some(expr)));
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
        if let Some(e) = self.expression_context.first_value_error() {
          return Err(e);
        }
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
        let n = next_parse!(self, self.parse_member_expression(builder));
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
        return n;
      }
      Token::Super => {
        let pos = self.source_position().clone();
        if self.peek()? == Token::LeftParen {
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          self.current_scope.set_first_super_call_position(&pos);
          return next_parse!(self, self.parse_super_call(builder));
        }
        return next_parse!(self, self.parse_member_expression(builder));
      }
      Token::Import => {
        let n = next_parse!(self, self.parse_import_call(builder));
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
        return n;
      }
      _ => {
        let expr = next_parse!(self, self.parse_member_expression(builder))?;
        if self.cur() == Token::LeftParen {
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
    match self.cur() {
      Token::OpIncrement | Token::OpDecrement => {
        let op = self.cur();
        let start = self.source_position().clone();
        self.advance()?;
        let parent_ctx = self.new_expression_context();
        let expr_start = self.prev_source_position().clone();
        if (self.cur() == Token::Await && self.current_scope.is_async_context())
          || (self.cur() == Token::Yield && self.current_scope.is_generator_context())
        {
          return self.err.invalid_update_expression(self.source_position().clone());
        }
        let n = next_parse!(self, self.parse_unary_expression(builder))?;
        let end_expr_pos = self.prev_source_position().clone();
        let child_ctx = self.set_expression_context(parent_ctx);
        if child_ctx.assignment_target_type() == AssignmentTargetType::Invalid {
          return self.err.invalid_update_expression(pos_range!(@just expr_start, end_expr_pos));
        }
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
        return Ok(build!(@pos, builder, unary_expression, start, UnaryExpressionOperandPosition::Pre, op, n));
      }
      _ => {
        let mut parent_ctx = self.new_expression_context();
        let start = self.source_position().clone();
        let expr_start = self.prev_source_position().clone();
        let n = next_parse!(self, self.parse_left_hand_side_expression(builder))?;
        let end_expr_pos = self.prev_source_position().clone();
        let result = match self.cur() {
          Token::OpIncrement | Token::OpDecrement => {
            let child_ctx = self.set_expression_context(parent_ctx);
            if child_ctx.assignment_target_type() == AssignmentTargetType::Invalid {
              return self.err.invalid_update_expression(pos_range!(@just expr_start, end_expr_pos));
            }
            let token = self.cur();
            let sp = self.source_position().clone();
            self.advance()?;
            self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
          _ => {
            self.expression_context.propagate(&mut parent_ctx);
            self.set_expression_context(parent_ctx);
            Ok(n)
          }
        };
        return result;
      }
    }
  }

  fn parse_unary_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    let expr_start = self.prev_source_position().clone();
    match self.cur() {
      Token::Delete | Token::Void | Token::Typeof | Token::OpPlus | Token::OpMinus | Token::OpTilde | Token::OpNot => {
        let op = self.cur();
        self.advance()?;
        let parent_ctx = self.new_expression_context();
        if self.cur().one_of(&[Token::Function, Token::LeftParen]) || self.contextual_keyword() == Token::Async {
          self.expression_context.set_is_maybe_immediate_function(true);
        }
        let rhs_exp = next_parse!(self, self.parse_unary_expression(builder))?;
        if op == Token::Delete && self.is_strict_mode() && builder.is_identifier(rhs_exp) {
          return parse_error!(
            self.region,
            "In strict mode code, identifier not allowed to delete target",
            &pos_range!(@just expr_start, self.prev_source_position())
          );
        }
        self.set_expression_context_with_propagation(parent_ctx);
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
        if !self.current_scope.is_async_context() {
          return next_parse!(self, self.parse_update_expression(builder));
        }
        self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
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
        Token::OpNullCoalescing => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::NullCoalescing, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::NullCoalescing;
        }
        Token::OpLogicalOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::LogicalOr, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::LogicalOr;
        }
        Token::OpLogicalAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::LogicalAnd, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::LogicalAnd;
        }
        Token::OpOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseOr, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::BitwiseOr;
        }
        Token::OpXor => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseXor, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::BitwiseXor;
        }
        Token::OpAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::BitwiseAnd, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::BitwiseAnd;
        }
        Token::OpEq | Token::OpStrictEq | Token::OpNotEq | Token::OpStrictNotEq => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Equality, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Equality;
        }
        Token::OpGreaterThan | Token::OpGreaterThanOrEq | Token::OpLessThan | Token::OpLessThanOrEq | Token::Instanceof | Token::In => {
          if self.cur() == Token::In && !self.expression_context.is_in_allowed() {
            return Ok(last);
          }
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Relational, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Relational;
        }
        Token::OpShl | Token::OpShr | Token::OpUShr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Shift, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Shift;
        }
        Token::OpPlus | Token::OpMinus => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Additive, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Additive;
        }
        Token::OpMul | Token::OpDiv | Token::OpMod => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Multiplicative, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Multiplicative;
        }
        Token::OpPow => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(builder, last, OperatorPriority::Exponentiation, last_op)
          )?;
          self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
          last_op = OperatorPriority::Exponentiation;
        }
        _ => return Ok(last),
      }
    }
  }

  fn parse_conditional_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let bin_expr = next_parse!(self, self.parse_binary_expression(builder))?;
    if self.cur() == Token::Question {
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      self.advance()?;
      let lhs = next_parse!(self, self.parse_assignment_expression(builder))?;
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
      if self.cur() != Token::Colon {
        return parse_error!(self.region, "':' expected", self.source_position());
      }
      self.advance()?;
      let rhs = next_parse!(self, self.parse_assignment_expression(builder))?;
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
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
    if self.cur() == Token::Yield && self.current_scope.is_generator_context() {
      return next_parse!(self, self.parse_yield_expression(builder));
    }

    if self.cur().one_of(&[Token::Identifier, Token::Await, Token::Yield]) && self.peek()? == Token::ArrowFunctionGlyph {
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      let params = next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::None))?;
      return next_parse!(self, self.parse_concise_body(builder, true, FunctionAttribute::NONE, params, None));
    }

    if self.cur() == Token::LeftParen && self.peek()? == Token::RightParen {
      let exprs = build!(@pos, builder, expressions, self.source_position(), Vec::new());
      self.advance()?;
      self.advance()?;
      expect!(self, self.cur(), Token::ArrowFunctionGlyph);
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      return next_parse!(self, self.parse_concise_body(builder, true, FunctionAttribute::NONE, exprs, None));
    }

    let expr_start_pos = self.prev_source_position().clone();
    let next_contextual_keyword = self.contextual_keyword();
    if next_contextual_keyword == Token::Async && !self.has_line_break_after() && self.peek()? == Token::Identifier {
      self.advance()?;
      let vars = vec![(self.value().clone(), self.source_position().clone())];
      let expr = next_parse!(self, self.parse_identifier_reference(builder, ParserConstraints::None))?;
      expect!(self, self.cur(), Token::ArrowFunctionGlyph);
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      return next_parse!(
        self,
        self.parse_concise_body(builder, true, FunctionAttribute::ASYNC, expr, Some(&vars))
      );
    }

    let mut ec = self.new_arrow_context();
    let expr = next_parse!(self, self.parse_conditional_expression(builder))?;
    let expr_end_pos = self.prev_source_position().clone();

    if self.cur() == Token::ArrowFunctionGlyph {
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      let mut attr = FunctionAttribute::NONE;
      if builder.is_new_call(expr) || builder.is_super_call(expr) {
        return parse_error!(self.region, "Super or new is not allowed here", &expr_start_pos);
      }
      if next_contextual_keyword == Token::Async {
        attr |= FunctionAttribute::ASYNC;
      }
      if builder.is_call_expr(expr) && !attr.contains(FunctionAttribute::ASYNC) {
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
        ec.to_arrow_ctx_unchecked().is_simple_parameter(),
        attr,
        expr,
        Some(ec.to_arrow_ctx_unchecked().var_list()),
      );
      return result;
    }

    if self.cur().is_assignment_operator() {
      if self.expression_context.assignment_target_type() == AssignmentTargetType::Invalid {
        return self
          .err
          .invalid_left_hand_side_expression(pos_range!(@just &expr_start_pos, &expr_end_pos));
      }
      ec.set_assignment_target_type(AssignmentTargetType::Invalid);
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
      let assignment_start_pos = self.source_position().clone();
      let assignment = next_parse!(self, self.parse_assignment_expression(builder))?;
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
      if let Ok(ctx) = ec.to_arrow_ctx_mut() {
        if op == Token::OpAssign && builder.is_yield_expr(assignment) {
          ctx.set_first_arrow_parameter_error(parse_error!(@raw, self.region, "Yield expression not allowed here", &assignment_start_pos));
        }
      }
      self.set_expression_context(ec);
      return Ok(build!(@runtime_pos, builder, binary_expr, expr.source_position(), op, expr, assignment));
    }
    self.expression_context.propagate(&mut ec);
    self.set_expression_context(ec);
    return Ok(expr);
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

  fn parse_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, should_return_exprs: bool) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    append_var_if!(self, self.cur(), self.value(), self.source_position());
    let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
    if self.cur() == Token::Comma {
      self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
      let mut items = Vec::<Expr>::new();
      builder.push_expr(&mut items, expr);
      while self.cur() == Token::Comma {
        self.advance()?;
        if self.cur() == Token::Spread {
          let pos = self.source_position().clone();
          self
            .expression_context
            .set_first_value_error(parse_error!(@raw, self.region, "Unexpected spread operator", &pos));
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

  fn parse_expression_with_value_validation<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    should_return_exprs: bool,
  ) -> ParseResult<Expr> {
    let cur_ctx = self.new_expression_context();
    let condition = next_parse!(self, self.parse_expression(builder, should_return_exprs))?;
    let value_ctx = self.set_expression_context(cur_ctx);
    if let Some(e) = value_ctx.first_value_error() {
      return Err(e);
    }
    return Ok(condition);
  }

  fn parse_assignment_expression_with_value_validation<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let cur_ctx = self.new_expression_context();
    let condition = next_parse!(self, self.parse_assignment_expression(builder))?;
    let value_ctx = self.set_expression_context(cur_ctx);
    if let Some(e) = value_ctx.first_value_error() {
      return Err(e);
    }
    return Ok(condition);
  }

  fn parse_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    use Token::*;
    match self.cur() {
      Terminate => {
        self.advance()?;
        return Ok(self.empty.into());
      }
      LeftBrace => {
        return next_parse!(self, self.parse_block_statement(builder));
      }
      Var => {
        return next_parse!(
          self,
          self.parse_lexical_or_variable_declaration(
            builder,
            VariableDeclarationType::Var,
            DeclarationParseOption::CONST_INITIALIZER_REQUIRED
          )
        );
      }
      If => {
        return next_parse!(self, self.parse_if_statement(builder));
      }
      Switch => {
        return next_parse!(self, self.parse_switch_statement(builder));
      }
      While => {
        return next_parse!(self, self.parse_while_statement(builder));
      }
      Do => {
        return next_parse!(self, self.parse_do_while_statement(builder));
      }
      For => {
        return next_parse!(self, self.parse_for_statement(builder));
      }
      Break => {
        return next_parse!(self, self.parse_break_statement(builder));
      }
      Continue => {
        return next_parse!(self, self.parse_continue_statement(builder));
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
    };
  }

  fn parse_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, option: DeclarationParseOption) -> ParseResult<Stmt> {
    let mut is_async = false;
    match self.cur() {
      Token::Identifier | Token::Const => {
        if self.contextual_keyword() != Token::Async || self.peek()? != Token::Function {
          return next_parse!(
            self,
            self.parse_lexical_or_variable_declaration(
              builder,
              if self.contextual_keyword() == Token::Let {
                VariableDeclarationType::Let
              } else {
                VariableDeclarationType::Const
              },
              option
            )
          );
        }
        return next_parse!(self, self.parse_function_declaration(builder, FunctionAttribute::ASYNC, option));
      }
      Token::Class => {
        return next_parse!(self, self.parse_class_declaration(builder, option));
      }
      Token::Function => {
        return next_parse!(self, self.parse_function_declaration(builder, FunctionAttribute::NONE, option));
      }
      _ => unreachable!(),
    };
  }

  #[inline]
  fn parse_block_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let scope = self.declare_child_scope(ScopeFlag::LEXICAL);
    let ret = next_parse!(self, self.parse_block_statement_with_scope(builder, scope));
    self.escape_scope(scope);
    return ret;
  }

  fn parse_block_statement_with_scope<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    scope: Exotic<Scope>,
  ) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftBrace);
    let stmts = next_parse!(self, self.parse_statement_list(builder, |t| t != Token::RightBrace))?;
    let block = build!(
      @pos,
      builder,
      block,
      start_pos,
      stmts,
      scope
    );
    expect!(self, self.cur(), Token::RightBrace);
    return Ok(block);
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
      if self.cur() == Token::Terminate {
        self.advance()?;
      }
      if Node::<Empty>::is(stmt) {
        continue;
      }
      builder.push_stmt(&mut items, stmt);
    }

    if items.len() > 1 {
      return Ok(build!(@pos, builder, statements, start, items).into());
    } else if items.len() == 1 {
      return Ok(*items.last().unwrap());
    }
    return Ok(self.empty.into());
  }

  fn parse_statement_list_item<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    match self.cur() {
      Token::Class | Token::Const | Token::Function => {
        return next_parse!(self, self.parse_declaration(builder, DeclarationParseOption::from_stmt()));
      }
      _ => {
        if self.cur() == Token::Identifier {
          let v = self.value();
          if (self.contextual_keyword() == Token::Async && self.peek()? == Token::Function && !self.has_line_break_after())
            || self.contextual_keyword() == Token::Let
          {
            return next_parse!(self, self.parse_declaration(builder, DeclarationParseOption::from_stmt()));
          }
        }
      }
    };

    return next_parse!(self, self.parse_statement(builder));
  }

  fn parse_lexical_or_variable_declaration<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    var_type: VariableDeclarationType,
    option: DeclarationParseOption,
  ) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();

    #[cfg(debug_assertions)]
    if var_type == VariableDeclarationType::Const {
      debug_assert_eq!(self.cur(), Token::Const);
    } else if var_type == VariableDeclarationType::Let {
      debug_assert_eq!(self.contextual_keyword(), Token::Let);
    } else {
      debug_assert_eq!(self.cur(), Token::Var);
    };

    self.advance()?;
    let mut vars = Vec::new();
    let mut is_vars = false;
    self.new_arrow_context();

    loop {
      let lhs_pos = self.source_position().clone();
      let lhs = match self.cur() {
        Token::Identifier | Token::Yield | Token::Await => {
          let token = self.cur();
          if self.cur() == Token::Identifier {
            if self.contextual_keyword() == Token::Let {
              return parse_error!(
                self.region,
                "Lexical declaration may not contains let keyword as an identifier",
                &lhs_pos
              );
            }
            let value = self.value().clone();
            self.expression_context.to_arrow_ctx_mut_unchecked().new_var(value, lhs_pos.clone());
          } else {
            let value = Parser::utf8_to_utf16(self.cur().symbol());
            self.expression_context.to_arrow_ctx_mut_unchecked().new_var(value, lhs_pos.clone());
          };
          next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::None))?
        }
        Token::LeftBrace | Token::LeftBracket => {
          let lhs = next_parse!(self, self.parse_binding_pattern(builder))?;
          if let Some(e) = self.expression_context.first_pattern_error() {
            return Err(e);
          }
          lhs
        }
        _ => {
          return parse_error!(self.region, "Unexpected token found", self.source_position());
        }
      };

      if let Some((ref cur, ref first)) = self.current_scope.declare_vars(
        if var_type == VariableDeclarationType::Var {
          if self.statement_block.is_export() {
            VariableType::ExportLegacyVar
          } else {
            VariableType::LegacyVar
          }
        } else {
          if self.statement_block.is_export() {
            VariableType::ExportLexical
          } else {
            VariableType::Lexical
          }
        },
        self.expression_context.to_arrow_ctx_mut_unchecked().var_list(),
      ) {
        return parse_error!(
          self.region,
          &format!(
            "Lexical declaration may not contains any duplicated identifiers\n\n{}\nBut found",
            format_error(&self.source.filename(), self.source.source_code(), "First defined", first, false)
          ),
          cur
        );
      }

      self.expression_context.reset();
      let rhs = if self.cur() == Token::OpAssign {
        self.advance()?;
        let rhs = next_parse!(self, self.parse_assignment_expression(builder))?;
        if let Some(e) = self.expression_context.first_value_error() {
          return Err(e);
        }
        Some(rhs)
      } else {
        if var_type == VariableDeclarationType::Const {
          let e = parse_error!(
          @raw,
            self.region,
            "const declared variable requires initial value",
            &pos_range!(@start start_pos, lhs_pos)
          );
          if option.contains(DeclarationParseOption::CONST_INITIALIZER_REQUIRED) {
            return Err(e);
          } else {
            self.statement_block.set_first_const_initializer_error(e);
          }
        }
        None
      };

      let var = build!(
        @pos,
        builder,
        var,
        start_pos,
        var_type,
        lhs,
        rhs
      );

      if self.cur() != Token::Comma {
        let stmt = if !is_vars {
          var
        } else {
          builder.push_stmt(&mut vars, var);
          build!(
            @pos,
            builder,
            vars,
            start_pos,
            vars
          )
        };
        if !option.contains(DeclarationParseOption::IGNORE_TERMINATION) {
          return next_parse!(self, self.parse_terminator(stmt));
        }
        return Ok(stmt);
      } else {
        is_vars = true;
        builder.push_stmt(&mut vars, var);
        self.advance()?;
      }
    }
  }

  fn parse_binding_pattern<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBrace => {
        return next_parse!(self, self.parse_object_literal(builder, ParserConstraints::BindingPattern));
      }
      _ => {
        debug_assert!(self.cur() == Token::LeftBracket);
        return next_parse!(self, self.parse_array_literal(builder, ParserConstraints::BindingPattern));
      }
    }
  }

  fn parse_binding_element<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBracket | Token::LeftBrace => {
        return next_parse!(self, self.parse_binding_pattern(builder));
      }
      _ => return next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::Initializer)),
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
        if self.cur() == Token::Identifier && self.contextual_keyword().one_of(&[Token::Arguments, Token::Eval]) {
          let e = parse_error!(
            @raw,
            self.region,
            "Accessed to 'arguments' or 'eval' is not allowed in strict mode code",
            self.source_position()
          );
          if self.is_strict_mode() {
            return Err(e);
          }
          self.expression_context.set_first_strict_mode_error(e);
        }
        if self.cur() == Token::Yield {
          let e = parse_error!(
            @raw,
            self.region,
            "Keyword 'yield' is not allowed here in strict mode code",
            self.source_position()
          );
          if self.is_strict_mode() {
            return Err(e);
          }
          self.expression_context.set_first_strict_mode_error(e);
        }
        if !constraints.is_keyword_identifier_allowed() {
          use Token::*;
          if self
            .contextual_keyword()
            .one_of(&[Implements, Interface, Let, Package, Private, Protected, Public, Static])
          {
            return parse_error!(
              self.region,
              format!("'{}' is reserved word", Parser::value_to_utf8(self.value())),
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
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
      return Ok(build!(self, builder, binary_expr, Token::OpAssign, identifier, expr).into());
    } else {
      self.advance()?;
    }
    return Ok(identifier);
  }

  fn parse_expression_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    let value = self.value().clone();
    let expr = next_parse!(self, self.parse_expression(builder, false))?;
    if let Some(e) = self.expression_context.first_value_error() {
      return Err(e);
    }
    let stmt = build!(@pos, builder, statement, start, expr);
    return next_parse!(self, self.parse_terminator(stmt.into()));
  }

  fn parse_statement_with_labelled_function_validateion<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    let prev_block = self.new_statement_block();
    let stmt = next_parse!(self, self.parse_statement(builder))?;
    let end_pos = self.prev_source_position().clone();
    let cur_block = self.set_statement_block(prev_block);
    if let Some(e) = cur_block.first_labelled_function_error() {
      return Err(e);
    }
    return Ok(stmt);
  }

  fn parse_if_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::If);
    expect!(self, self.cur(), Token::LeftParen);
    let condition = next_parse!(self, self.parse_expression(builder, false))?;
    expect!(self, self.cur(), Token::RightParen);
    let then_stmt = next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?;
    let else_stmt = if self.cur() == Token::Else {
      self.advance()?;
      Some(next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?)
    } else {
      None
    };
    return Ok(build!(
      @pos,
      builder,
      if_stmt,
      start_pos,
      condition,
      then_stmt,
      else_stmt
    ));
  }

  fn parse_iteration_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_for_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(true);
    self.statement_block.set_is_continuable(true);
    let start_pos = self.source_position().clone();
    let scope = self.declare_child_scope(ScopeFlag::LEXICAL);
    expect!(self, self.cur(), Token::For);
    let maybe_await_pos = self.source_position().clone();
    let is_for_await = if self.cur() == Token::Await {
      self.advance()?;
      true
    } else {
      false
    };
    expect!(self, self.cur(), Token::LeftParen);
    let lhs_start_pos = self.source_position().clone();
    let parent_ctx = self.new_expression_context();
    let lhs: Ast = if self.cur() == Token::Var {
      next_parse!(
        self,
        self.parse_lexical_or_variable_declaration(builder, VariableDeclarationType::Var, DeclarationParseOption::IGNORE_TERMINATION)
      )?
      .into()
    } else if self.contextual_keyword() == Token::Let {
      next_parse!(
        self,
        self.parse_lexical_or_variable_declaration(builder, VariableDeclarationType::Let, DeclarationParseOption::IGNORE_TERMINATION)
      )?
      .into()
    } else if self.cur() == Token::Const {
      next_parse!(
        self,
        self.parse_lexical_or_variable_declaration(builder, VariableDeclarationType::Const, DeclarationParseOption::IGNORE_TERMINATION)
      )?
      .into()
    } else if self.cur() == Token::Terminate {
      self.empty.into()
    } else if !self.contextual_keyword().one_of(&[Token::Let, Token::Async, Token::Of]) {
      self.expression_context.set_is_in_allowed(false);
      next_parse!(self, self.parse_expression(builder, false))?.into()
    } else {
      return parse_error!(self.region, "Unexpected token found", self.source_position());
    };
    let cur_ctx = self.set_expression_context(parent_ctx);
    let lhs_end_pos = self.prev_source_position().clone();

    let result = match self.cur() {
      // For in declaration branch
      Token::In => {
        if is_for_await {
          return parse_error!(self.region, "Unexpected await keyword", &maybe_await_pos);
        }
        if let Some(e) = cur_ctx.first_pattern_error() {
          return Err(e);
        }
        if cur_ctx.assignment_target_type() == AssignmentTargetType::Invalid {
          return parse_error!(
            self.region,
            "Invalid left-hand-side expression",
            &pos_range!(@start lhs_start_pos, lhs_end_pos)
          );
        }
        if !builder.is_valid_for_of_in_lhs(lhs) {
          return parse_error!(
            self.region,
            "Invalid for-in declaration",
            &pos_range!(@start lhs_start_pos, lhs_end_pos)
          );
        }
        self.advance()?;
        let rhs = scoped_expr_ctx!(self, {
          self.expression_context.set_is_in_allowed(true);
          next_parse!(self, self.parse_expression_with_value_validation(builder, false))?
        });
        expect!(self, self.cur(), Token::RightParen);
        let body = if self.cur() == Token::LeftBrace {
          next_parse!(self, self.parse_block_statement_with_scope(builder, scope))?
        } else {
          next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?
        };
        Ok(build!(
          @pos,
          builder,
          for_in_stmt,
          start_pos,
          lhs,
          rhs,
          body
        ))
      }
      // For declaration branch
      Token::Terminate => {
        if is_for_await {
          return parse_error!(self.region, "Unexpected await keyword", &maybe_await_pos);
        }

        if let Some(e) = self.statement_block.first_const_initializer_error() {
          return Err(e);
        }

        self.advance()?;
        let condition = scoped_expr_ctx!(self, {
          self.expression_context.set_is_in_allowed(true);
          if self.cur() != Token::Terminate {
            next_parse!(self, self.parse_expression_with_value_validation(builder, false))?
          } else {
            self.empty.into()
          }
        });
        expect!(self, self.cur(), Token::Terminate);
        let computation = scoped_expr_ctx!(self, {
          self.expression_context.set_is_in_allowed(true);
          if self.cur() != Token::RightParen {
            next_parse!(self, self.parse_expression_with_value_validation(builder, false))?
          } else {
            self.empty.into()
          }
        });
        expect!(self, self.cur(), Token::RightParen);
        let body = if self.cur() == Token::LeftBrace {
          next_parse!(self, self.parse_block_statement_with_scope(builder, scope))?
        } else {
          next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?
        };
        Ok(build!(
          @pos,
          builder,
          for_stmt,
          start_pos,
          lhs,
          condition,
          computation,
          body
        ))
      }
      _ => {
        // For of declaration branch
        if self.contextual_keyword() == Token::Of {
          if let Some(e) = cur_ctx.first_pattern_error() {
            return Err(e);
          }
          if cur_ctx.assignment_target_type() == AssignmentTargetType::Invalid {
            return parse_error!(
              self.region,
              "Invalid left-hand-side expression",
              &pos_range!(@start lhs_start_pos, lhs_end_pos)
            );
          }
          if !builder.is_valid_for_of_in_lhs(lhs) {
            return parse_error!(
              self.region,
              "Invalid for-in declaration",
              &pos_range!(@start lhs_start_pos, lhs_end_pos)
            );
          }
          self.advance()?;
          let rhs = scoped_expr_ctx!(self, {
            self.expression_context.set_is_in_allowed(true);
            next_parse!(self, self.parse_assignment_expression_with_value_validation(builder))?
          });
          expect!(self, self.cur(), Token::RightParen);
          let body = if self.cur() == Token::LeftBrace {
            next_parse!(self, self.parse_block_statement_with_scope(builder, scope))?
          } else {
            next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?
          };
          Ok(build!(
            @pos,
            builder,
            for_of_stmt,
            start_pos,
            is_for_await,
            lhs,
            rhs,
            body
          ))
        } else {
          return parse_error!(self.region, "Unexpected token found", self.source_position());
        }
      }
    };

    self.escape_scope(scope);
    self.set_statement_block(prev_block);
    return result;
  }

  fn parse_for_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    unreachable!();
  }

  fn parse_for_binding<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_continue_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    if !self.statement_block.is_continuable() {
      return parse_error!(self.region, "Continue statement not allowed here", self.source_position());
    }
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Continue);
    let identifier = if !self.has_line_break_before() && self.cur().one_of(&[Token::Identifier, Token::Yield, Token::Await]) {
      let val = Some(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
      if !self.current_scope.is_label_exists(self.value()) {
        return parse_error!(self.region, "Label not exists", self.source_position());
      }
      self.advance()?;
      val
    } else {
      None
    };
    return Ok(build!(
      @pos,
      builder,
      continue_stmt,
      start_pos,
      identifier
    ));
  }

  fn parse_break_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    if !self.statement_block.is_breakable() {
      return parse_error!(self.region, "Break statement not allowed here", self.source_position());
    }
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Break);
    let identifier = if !self.has_line_break_before() && self.cur().one_of(&[Token::Identifier, Token::Yield, Token::Await]) {
      let val = Some(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
      if !self.current_scope.is_label_exists(self.value()) {
        return parse_error!(self.region, "Label not exists", self.source_position());
      }
      self.advance()?;
      val
    } else {
      None
    };
    return Ok(build!(
      @pos,
      builder,
      break_stmt,
      start_pos,
      identifier
    ));
  }

  fn parse_return_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    if !self.statement_block.is_returnable() {
      return parse_error!(self.region, "Return statement not allowed here", self.source_position());
    }
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Return);
    if self.is_eol(self.cur()) {
      let stmt = build!(
        @pos,
        builder,
        return_stmt,
        start_pos,
        None
      );
      return self.parse_terminator(stmt);
    }
    let expr = next_parse!(self, self.parse_expression(builder, false))?;
    return Ok(build!(
      @pos,
      builder,
      return_stmt,
      start_pos,
      Some(expr)
    ));
  }

  fn parse_with_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    if self.is_strict_mode() {
      return parse_error!(
        self.region,
        "In strict mode code, with statement not allowed",
        self.source_position()
      );
    }
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::With);
    expect!(self, self.cur(), Token::LeftParen);
    let expr = next_parse!(self, self.parse_expression(builder, false))?;
    expect!(self, self.cur(), Token::RightParen);
    let stmt_start_pos = self.source_position().clone();
    let stmt = next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?;
    return Ok(build!(
      @pos,
      builder,
      with_stmt,
      start_pos,
      expr,
      stmt
    ));
  }

  fn parse_while_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(true);
    self.statement_block.set_is_continuable(true);
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::While);
    expect!(self, self.cur(), Token::LeftParen);
    let condition = next_parse!(self, self.parse_expression_with_value_validation(builder, false))?;
    expect!(self, self.cur(), Token::RightParen);
    let stmt = next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?;
    self.set_statement_block(prev_block);
    return Ok(build!(
      @pos,
      builder,
      while_stmt,
      start_pos,
      condition,
      stmt
    ));
  }

  fn parse_do_while_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(true);
    self.statement_block.set_is_continuable(true);
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Do);
    let stmt = next_parse!(self, self.parse_statement_with_labelled_function_validateion(builder))?;

    expect!(self, self.cur(), Token::While);
    expect!(self, self.cur(), Token::LeftParen);
    let condition = next_parse!(self, self.parse_expression_with_value_validation(builder, false))?;
    expect!(self, self.cur(), Token::RightParen);

    self.set_statement_block(prev_block);
    return Ok(build!(
      @pos,
      builder,
      do_while_stmt,
      start_pos,
      condition,
      stmt
    ));
  }

  fn parse_switch_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(true);
    let swtich_start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Switch);
    expect!(self, self.cur(), Token::LeftParen);
    let condition = next_parse!(self, self.parse_expression_with_value_validation(builder, false))?;
    expect!(self, self.cur(), Token::RightParen);
    expect!(self, self.cur(), Token::LeftBrace);

    let scope = self.declare_child_scope(ScopeFlag::LEXICAL);
    let mut cases = Vec::new();
    while self.cur() != Token::RightBrace {
      let next_start_pos = self.source_position().clone();
      match self.cur() {
        Token::Case => {
          self.advance()?;
          let condition = next_parse!(self, self.parse_expression_with_value_validation(builder, false))?;
          expect!(self, self.cur(), Token::Colon);
          let stmts = next_parse!(
            self,
            self.parse_statement_list(builder, |t| !t.one_of(&[Token::Case, Token::Default, Token::RightBrace]))
          )?;
          let case = build!(
            @pos,
            builder,
            switch_case,
            next_start_pos,
            Some(condition),
            stmts
          );
          cases.push(case);
        }
        Token::Default => {
          self.advance()?;
          expect!(self, self.cur(), Token::Colon);
          let stmts = next_parse!(
            self,
            self.parse_statement_list(builder, |t| !t.one_of(&[Token::Case, Token::Default, Token::RightBrace]))
          )?;
          let case = build!(
            @pos,
            builder,
            switch_case,
            next_start_pos,
            None,
            stmts
          );
          cases.push(case);
        }
        _ => {
          return parse_error!(self.region, "Unexpected token found", self.source_position());
        }
      };
    }

    expect!(self, self.cur(), Token::RightBrace);
    self.set_statement_block(prev_block);
    return Ok(build!(
      @pos,
      builder,
      switch_stmt,
      swtich_start_pos,
      scope,
      condition,
      cases
    ));
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
    let start_pos = self.source_position().clone();
    let value = self.value().clone();
    if let Some(pos) = self.current_scope.push_label_without_duplication(value, start_pos.clone()) {
      return parse_error!(
        self.region,
        format!(
          "Duplicated label found\n\n{}\nBut found",
          format_error(&self.source.filename(), self.source.source_code(), "First defined", &pos, false)
        ),
        &start_pos
      );
    }
    let identifier = next_parse!(self, self.parse_identifier_reference(builder, ParserConstraints::None))?;
    expect!(self, self.cur(), Token::Colon);
    let stmt = if self.cur() == Token::Function {
      if self.is_strict_mode() {
        return parse_error!(
          self.region,
          "In strict mode code, function declaration may not have label",
          self.source_position()
        );
      }
      if self.statement_block.first_labelled_function_error().is_none() {
        self.statement_block.set_first_labelled_function_error(parse_error!(
          @raw,
          self.region,
          "Statement may not contains labelled function.",
          self.source_position()
        ));
      }
      next_parse!(
        self,
        self.parse_function_declaration(builder, FunctionAttribute::NONE, DeclarationParseOption::NONE)
      )
    } else {
      next_parse!(self, self.parse_statement(builder))
    }?;
    self.current_scope.pop_label();
    return Ok(build!(
      @pos,
      builder,
      labelled_stmt,
      start_pos,
      identifier,
      stmt
    ));
  }

  fn parse_labelled_item<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    unreachable!();
  }

  fn parse_throw_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Throw);
    if self.is_eol(self.cur()) || self.cur() == Token::Terminate {
      return parse_error!(self.region, "Throw statement requires expression", &start_pos);
    }
    let expr = next_parse!(self, self.parse_expression(builder, false))?;
    return Ok(build!(
      @pos,
      builder,
      throw_stmt,
      start_pos,
      expr
    ));
  }

  fn parse_try_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Try);
    let try_block = next_parse!(self, self.parse_block_statement(builder))?;
    let catch_block = if self.cur() == Token::Catch {
      Some(next_parse!(self, self.parse_catch(builder))?)
    } else {
      None
    };
    let finally_block = if self.cur() == Token::Finally {
      Some(next_parse!(self, self.parse_finally(builder))?)
    } else {
      None
    };

    return Ok(build!(
      @pos,
      builder,
      try_catch_stmt,
      start_pos,
      try_block,
      catch_block,
      finally_block
    ));
  }

  fn parse_catch<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Catch);
    let parent_ctx = self.new_arrow_context();
    let (param, lexical_vars, legacy_vars) = if self.cur() == Token::LeftParen {
      self.advance()?;
      let is_identifier = self.cur() == Token::Identifier;
      self.declare_identifir_var_if();
      let ret = Some(next_parse!(self, self.parse_binding_element(builder))?);
      self.invalidate_unique_parameter(None);
      if let Err(e) = self.invalidate_arrow_parameters(&self.expression_context) {
        return Err(e);
      }
      expect!(self, self.cur(), Token::RightParen);
      (
        ret,
        if !is_identifier {
          Some(self.expression_context.to_arrow_ctx_unchecked().var_list().clone())
        } else {
          None
        },
        if is_identifier {
          Some(self.expression_context.to_arrow_ctx_unchecked().var_list().clone())
        } else {
          None
        },
      )
    } else {
      (None, None, None)
    };
    self.set_expression_context(parent_ctx);

    let mut scope = self.declare_child_scope(ScopeFlag::LEXICAL);
    if let Some(ref legacy_vars) = legacy_vars {
      scope.declare_vars(VariableType::CatchParameterLegacyVar, legacy_vars);
    }
    if let Some(ref lexical_vars) = lexical_vars {
      scope.declare_vars(VariableType::Lexical, lexical_vars);
    }
    let body = next_parse!(self, self.parse_block_statement_with_scope(builder, scope))?;
    self.escape_scope(scope);

    return Ok(build!(
      @pos,
      builder,
      catch_block,
      start_pos,
      param,
      body
    ));
  }

  fn parse_finally<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    expect!(self, self.cur(), Token::Finally);
    return next_parse!(self, self.parse_block_statement(builder));
  }

  fn parse_debugger_statement<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Debugger);
    return Ok(build!(
      @pos,
      builder,
      debugger_stmt,
      start_pos
    ));
  }

  fn parse_function_expression<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, attr: FunctionAttribute) -> ParseResult<Expr> {
    let node = next_parse!(self, self.parse_function(builder, attr, true))?;
    self.expression_context.set_assignment_target_type(AssignmentTargetType::Invalid);
    return Ok(Expr::try_from(node).expect("Function failed to cast to expression"));
  }

  fn parse_function_declaration<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    attr: FunctionAttribute,
    option: DeclarationParseOption,
  ) -> ParseResult<Stmt> {
    let node = next_parse!(
      self,
      self.parse_function(builder, attr, option.contains(DeclarationParseOption::DEFAULT_EXPORT_RHS))
    )?;
    return Ok(Stmt::try_from(node).expect("Function failed to cast to declaration"));
  }

  fn parse_function<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    mut attr: FunctionAttribute,
    is_name_ommitable: bool,
  ) -> ParseResult<Ast> {
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(false);
    self.statement_block.set_is_continuable(false);
    self.statement_block.set_is_returnable(true);
    let start = self.source_position().clone();
    if attr.contains(FunctionAttribute::ASYNC) {
      self.advance()?;
    }
    expect!(self, self.cur(), Token::Function);
    let mut identifier: Option<Expr> = None;
    let mut var_list = Vec::<(Vec<u16>, SourcePosition)>::new();

    if self.cur() == Token::OpMul {
      self.advance()?;
      attr |= FunctionAttribute::GENERATOR;
    }

    if self.cur() == Token::Identifier {
      let val = self.value().clone();
      self.declare_var(
        if self.is_strict_mode() {
          if self.statement_block.is_export() {
            VariableType::ExportLexical
          } else {
            VariableType::Lexical
          }
        } else {
          if self.statement_block.is_export() {
            VariableType::ExportLegacyVar
          } else {
            VariableType::LegacyVar
          }
        },
        (val, self.source_position().clone()),
      )?;
      identifier = Some(next_parse!(self, self.parse_identifier(builder, ParserConstraints::None))?);
      if self.is_strict_mode() {
        if let Some(e) = self.expression_context.first_strict_mode_error() {
          return Err(e);
        }
      }
      self.advance()?;
    } else if !is_name_ommitable {
      return parse_error!(self.region, "Identifier expected", self.source_position());
    }

    let formal_parameter_position = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    let parent_ctx = self.new_arrow_context();
    let mut scope = self.declare_child_scope(
      ScopeFlag::OPAQUE
        | ScopeFlag::ALLOW_NEW_TARGET
        | if attr.contains(FunctionAttribute::ASYNC) {
          ScopeFlag::ASYNC_CONTEXT
        } else {
          ScopeFlag::NONE
        }
        | if attr.contains(FunctionAttribute::GENERATOR) {
          ScopeFlag::GENERATOR_CONTEXT
        } else {
          ScopeFlag::NONE
        },
    );
    let mut params = next_parse!(
      self,
      self.parse_formal_parameters(builder, attr.contains(FunctionAttribute::GENERATOR))
    )?;
    if let Some(pos) = self.current_scope.first_super_call_position() {
      return parse_error!(self.region, "Super not allowed here", &pos);
    }
    params.set_source_position(&formal_parameter_position.runtime_source_position());
    var_list.extend_from_slice(self.expression_context.to_arrow_ctx_mut_unchecked().var_list());
    expect!(self, self.cur(), Token::RightParen);
    let mut child_ctx = self.set_expression_context(parent_ctx);
    if child_ctx.to_arrow_ctx_unchecked().is_simple_parameter() {
      self.current_scope.mark_as_simple_parameter();
    }
    self.current_scope.declare_vars(VariableType::FormalParameter, &var_list);

    let mut function_body_start = (self.scanner.source_index()) as u32;
    expect!(self, self.cur(), Token::LeftBrace);
    next_parse!(self, self.parse_directive_prologue(scope))?;
    if self.is_strict_mode() || !scope.is_simple_parameter() {
      self.invalidate_unique_parameter(Some(&mut child_ctx));
      let maybe_error = self.invalidate_arrow_parameters(&child_ctx);
      if let Err(e) = maybe_error {
        return Err(e);
      }
    }
    if self.is_strict_mode() {
      if let Some(e) = child_ctx.first_strict_mode_error() {
        return Err(e);
      }
    }
    let mut function_body_end = 0;

    let parent_ctx = self.new_expression_context();
    let body = if self.should_use_tree_builder() {
      self.expression_context.set_is_maybe_immediate_function(false);
      function_body_start = 0;
      function_body_end = 0;
      let body = next_parse!(self, self.parse_function_body(builder))?;
      Some(Into::<Ast>::into(body))
    } else {
      let skip_tree_builder = self.skip_tree_builder;
      next_skip_parse!(self, self.parse_function_body(skip_tree_builder))?;
      function_body_end = (self.scanner.source_index() - 1) as u32;
      None
    };

    if !self.current_scope.is_super_call_allowed() && self.current_scope.first_super_call_position().is_some() {
      let pos = self.current_scope.first_super_call_position().unwrap().clone();
      return parse_error!(self.region, "Super not allowed here", &pos);
    }
    if !self.current_scope.is_super_property_allowed() && self.current_scope.first_super_property_position().is_some() {
      let pos = self.current_scope.first_super_property_position().unwrap().clone();
      return parse_error!(self.region, "Super not allowed here", &pos);
    }
    self.set_expression_context(parent_ctx);
    self.escape_scope(scope);
    expect!(self, self.cur(), Token::RightBrace);
    self.escape_scope(scope);
    self.set_statement_block(prev_block);
    return Ok(build!(
      @pos,
      builder,
      function,
      start,
      identifier,
      attr,
      scope,
      params,
      body,
      function_body_start,
      function_body_end
    ));
  }

  fn parse_formal_parameters<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, is_generator: bool) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    let mut params = Vec::<Expr>::new();
    loop {
      let mut next_expr;
      let expr_start_pos = self.source_position().clone();
      match self.cur() {
        Token::Identifier | Token::Yield | Token::Await => {
          if self.cur() == Token::Yield && is_generator {
            return parse_error!(self.region, "Yield not allowed here", self.source_position());
          }
          self.declare_identifir_var_if();
          next_expr = next_parse!(self, self.parse_binding_element(builder))?;
        }
        Token::LeftBrace | Token::LeftBracket => {
          next_expr = next_parse!(self, self.parse_binding_element(builder))?;
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
        }
        Token::Super => {
          return parse_error!(self.region, "Super not allowed here", self.source_position());
        }
        Token::Spread => {
          append_var_if!(self, self.peek()?, self.peek_value(), self.source_position());
          let start = self.source_position().clone();
          let rp = next_parse!(self, self.parse_function_rest_parameter(builder))?;
          let end = self.prev_source_position().clone();
          builder.push_expr(&mut params, rp);
          if self.cur() == Token::Comma {
            self.advance()?;
          }
          if self.cur() != Token::RightParen {
            return parse_error!(self.region, "Spread must be last element", &pos_range!(start, end));
          }
          self
            .expression_context
            .to_arrow_ctx_mut()
            .map(|ctx| ctx.set_is_simple_parameter(false));
          return Ok(build!(@pos, builder, expressions, start_pos, params).into());
        }
        _ => {
          expect!(@notadvance self, self.cur(), Token::RightParen);
          return Ok(build!(@pos, builder, expressions, start_pos, params).into());
        }
      };

      if self.cur() == Token::OpAssign {
        self.advance()?;
        let cur_ctx = self.new_expression_context();
        let init_start_pos = self.source_position().clone();
        let init = next_parse!(self, self.parse_assignment_expression(builder))?;
        let value_ctx = self.set_expression_context(cur_ctx);
        if let Some(e) = value_ctx.first_value_error() {
          return Err(e);
        }
        if !self.current_scope.is_super_property_allowed() {
          if let Some(pos) = self.current_scope.first_super_property_position() {
            return parse_error!(self.region, "Super not allowed here", &pos);
          }
        }
        if !self.current_scope.is_super_call_allowed() {
          if let Some(pos) = self.current_scope.first_super_call_position() {
            return parse_error!(self.region, "Super not allowed here", &pos);
          }
        }
        if is_generator && builder.is_yield_expr(init) {
          return parse_error!(self.region, "Yield expression not allowed here", &init_start_pos);
        }
        self
          .expression_context
          .to_arrow_ctx_mut()
          .map(|ctx| ctx.set_is_simple_parameter(false));
        next_expr = build!(
          @pos,
          builder,
          binary_expr,
          expr_start_pos,
          Token::OpAssign,
          next_expr,
          init
        );
      }
      builder.push_expr(&mut params, next_expr);

      if self.cur() == Token::Comma {
        self.advance()?;
      }
    }
    unreachable!();
  }

  fn parse_function_rest_parameter<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::Spread);
    let expr = match self.cur() {
      Token::LeftBrace | Token::LeftBracket => next_parse!(self, self.parse_binding_pattern(builder))?,
      _ => next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::Initializer))?,
    };
    return Ok(build!(
      @pos,
      builder,
      unary_expression,
      start,
      UnaryExpressionOperandPosition::Pre,
      Token::Spread,
      expr
    ));
  }

  fn parse_function_body<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    return next_parse!(self, self.parse_statement_list(builder, |t| t != Token::RightBrace));
  }

  fn parse_concise_body<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_simple_parameter: bool,
    attr: FunctionAttribute,
    args: Expr,
    params: Option<&Vec<(Vec<u16>, SourcePosition)>>,
  ) -> ParseResult<Expr> {
    let has_brace = self.cur() == Token::LeftBrace;
    let mut scope = self.declare_child_scope(
      ScopeFlag::TRANSPARENT
        | if attr.contains(FunctionAttribute::ASYNC) {
          ScopeFlag::ASYNC_CONTEXT
        } else {
          ScopeFlag::NONE
        }
        | if is_simple_parameter {
          ScopeFlag::SIMPLE_PARAMETER
        } else {
          ScopeFlag::NONE
        },
    );
    let mut function_body_start = 0_u32;
    let mut function_body_end = 0_u32;
    if let Some(params) = params {
      scope.declare_vars(VariableType::LegacyVar, params);
    }
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(false);
    self.statement_block.set_is_continuable(false);
    let body = if has_brace {
      self.statement_block.set_is_returnable(true);
      self.advance()?;
      next_parse!(self, self.parse_directive_prologue(scope))?;
      if self.should_use_tree_builder() {
        self.expression_context.set_is_maybe_immediate_function(false);
        Some(next_parse!(self, self.parse_statement_list(builder, |t| t != Token::RightBrace)).map(|t| t.into())?)
      } else {
        function_body_start = self.scanner.source_index() as u32;
        let skip_tree_builder = self.skip_tree_builder;
        next_skip_parse!(self, self.parse_statement_list(skip_tree_builder, |t| t != Token::RightBrace))?;
        function_body_end = self.scanner.source_index() as u32;
        None
      }
    } else {
      Some(next_parse!(self, self.parse_assignment_expression(builder)).map(|t| t.into())?)
    };
    self.escape_scope(scope);
    if has_brace {
      self.advance()?;
    }
    self.set_statement_block(prev_block);
    return Ok(
      Expr::try_from(build!(
        @runtime_pos,
        builder,
        function,
        args.source_position(),
        None,
        attr,
        scope,
        args,
        body,
        function_body_start,
        function_body_end
      ))
      .expect("Failed to convert Function to Expr"),
    );
  }

  fn parse_method_attributes(&mut self, is_class_field: bool) -> ParseResult<(FunctionAttribute, ClassFieldFlag)> {
    let mut class_field_flag = ClassFieldFlag::NONE;
    let mut attr = FunctionAttribute::NONE;

    if self.contextual_keyword() == Token::Static {
      if is_class_field {
        class_field_flag |= ClassFieldFlag::STATIC;
        self.advance()?;
      } else {
        return parse_error!(self.region, "Unexpected static keyword", self.source_position());
      }
    }

    if self.contextual_keyword() == Token::Get {
      attr |= FunctionAttribute::GETTER;
      self.advance()?;
      if self.contextual_keyword() == Token::PrivateIdentifier {
        class_field_flag |= ClassFieldFlag::PRIVATE;
      } else {
        class_field_flag |= ClassFieldFlag::PUBLIC;
      }
      return Ok((attr, class_field_flag));
    } else if self.contextual_keyword() == Token::Set {
      attr |= FunctionAttribute::SETTER;
      self.advance()?;
      if self.contextual_keyword() == Token::PrivateIdentifier {
        class_field_flag |= ClassFieldFlag::PRIVATE;
      } else {
        class_field_flag |= ClassFieldFlag::PUBLIC;
      }
      return Ok((attr, class_field_flag));
    }
    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::Async {
      attr |= FunctionAttribute::ASYNC;
      self.advance()?;
    }
    if self.cur() == Token::OpMul {
      attr |= FunctionAttribute::GENERATOR;
      self.advance()?;
    }

    if self.contextual_keyword() == Token::PrivateIdentifier {
      class_field_flag |= ClassFieldFlag::PRIVATE;
    } else if self.is_value_match_with(self.constructor_str) && is_class_field && !class_field_flag.contains(ClassFieldFlag::STATIC) {
      attr |= FunctionAttribute::CONSTRUCTOR;
      class_field_flag |= ClassFieldFlag::PUBLIC;
    } else {
      class_field_flag |= ClassFieldFlag::PUBLIC;
    }

    return Ok((attr, class_field_flag));
  }

  fn parse_method_body<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    start: &SourcePosition,
    name: Expr,
    attr: FunctionAttribute,
    is_allow_direct_super: bool,
  ) -> ParseResult<Expr> {
    let mut formal_parameters: Option<Expr> = None;
    let mut var_list = Vec::<(Vec<u16>, SourcePosition)>::new();
    expect!(self, self.cur(), Token::LeftParen);
    let param_start_pos = self.source_position().clone();
    let ec = self.new_arrow_context();
    let prev_block = self.new_statement_block();
    self.statement_block.set_is_breakable(false);
    self.statement_block.set_is_continuable(false);
    self.statement_block.set_is_returnable(true);

    let scope_flag = ScopeFlag::OPAQUE
      | if attr.contains(FunctionAttribute::CONSTRUCTOR) {
        ScopeFlag::ALLOW_NEW_TARGET
          | if is_allow_direct_super {
            ScopeFlag::ALLOW_SUPER_CALL
          } else {
            ScopeFlag::NONE
          }
      } else if is_allow_direct_super {
        ScopeFlag::ALLOW_SUPER_PROPERTY
      } else {
        ScopeFlag::NONE
      }
      | if attr.contains(FunctionAttribute::ASYNC) {
        ScopeFlag::ASYNC_CONTEXT
      } else {
        ScopeFlag::NONE
      }
      | if attr.contains(FunctionAttribute::GENERATOR) {
        ScopeFlag::GENERATOR_CONTEXT
      } else {
        ScopeFlag::NONE
      };
    let mut scope = self.declare_child_scope(scope_flag);

    if attr.contains(FunctionAttribute::GETTER) {
      if self.cur() != Token::RightParen {
        return parse_error!(self.region, "Getter must not have any formal parameters", self.source_position());
      }
      self.advance()?;
      formal_parameters = Some(build!(@pos, builder, expressions, param_start_pos, Vec::new()).into());
    } else if attr.contains(FunctionAttribute::SETTER) {
      if self.cur() == Token::RightParen {
        return parse_error!(self.region, "Setter must have exactly one formal parameter", self.source_position());
      }
      formal_parameters = Some(next_parse!(
        self,
        self.parse_property_set_parameter_list(builder, &param_start_pos)
      )?);
      var_list.append(self.expression_context.to_arrow_ctx_mut_unchecked().var_list_mut());
      if self.cur() != Token::RightParen {
        return parse_error!(self.region, "Setter must have exactly one formal parameter", self.source_position());
      }
      self.advance()?;
    } else {
      formal_parameters = Some(next_parse!(
        self,
        self.parse_formal_parameters(builder, attr.contains(FunctionAttribute::GENERATOR))
      )?);
      self.invalidate_unique_parameter(None);
      let maybe_error = self.invalidate_arrow_parameters(&self.expression_context);
      if let Err(e) = maybe_error {
        return Err(e);
      }
      var_list.append(self.expression_context.to_arrow_ctx_mut_unchecked().var_list_mut());
      expect!(self, self.cur(), Token::RightParen);
    }

    let function_body_start = if self.parser_option.disable_skip_parser {
      0_u32
    } else {
      self.scanner.source_index() as u32
    };
    expect!(self, self.cur(), Token::LeftBrace);
    if self.expression_context.to_arrow_ctx_unchecked().is_simple_parameter() {
      scope.mark_as_simple_parameter();
    }
    self.current_scope.declare_vars(VariableType::FormalParameter, &var_list);
    let cur_ctx = self.set_expression_context(ec);

    let mut this = scoped!(self, |this| {
      this.escape_scope(scope);
    });
    next_parse!(this, this.parse_directive_prologue(scope))?;
    if this.is_strict_mode() {
      if let Some(e) = cur_ctx.first_strict_mode_error() {
        return Err(e);
      }
    }
    let formal_params = formal_parameters.unwrap();
    let body = if this.should_use_tree_builder() {
      let ast_builder = this.ast_builder;
      Some(next_parse!(this, this.parse_function_body(ast_builder))?.into())
    } else {
      let skip_tree_builder = this.skip_tree_builder;
      next_skip_parse!(this, this.parse_function_body(skip_tree_builder))?;
      None
    };
    if this.current_scope.first_super_call_position().is_some() && !this.current_scope.is_super_call_allowed() {
      let pos = this.current_scope.first_super_call_position().unwrap();
      return parse_error!(this.region, "Super not allowed here", &pos);
    }
    let function_body_end = if this.parser_option.disable_skip_parser {
      0_u32
    } else {
      (this.scanner.source_index() - 1) as u32
    };
    expect!(this, this.cur(), Token::RightBrace);
    this.escape_scope(scope);
    this.set_statement_block(prev_block);
    let function = build!(
      @pos,
      builder,
      function,
      start,
      Some(name),
      attr,
      scope,
      formal_params,
      body,
      function_body_start,
      function_body_end
    );

    return Ok(Expr::try_from(function).expect("Failed to convert Function to Expr"));
  }

  fn parse_property_set_parameter_list<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    source_position: &SourcePosition,
  ) -> ParseResult<Expr> {
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
        return Ok(build!(@pos, builder, expressions, source_position, params));
      }
      _ => {
        return parse_error!(self.region, "Unexpected token found", self.source_position());
      }
    };
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
      if let Some(e) = self.expression_context.first_value_error() {
        return Err(e);
      }
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
    if let Some(e) = self.expression_context.first_value_error() {
      return Err(e);
    }
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

  fn parse_class_declaration<Builder: NodeOps>(&mut self, builder: Exotic<Builder>, option: DeclarationParseOption) -> ParseResult<Stmt> {
    return Ok(
      Stmt::try_from(next_parse!(
        self,
        self.parse_class(builder, option.contains(DeclarationParseOption::DEFAULT_EXPORT_RHS), true)
      )?)
      .expect("Failed to convert Class to Stmt"),
    );
  }

  fn parse_class_expression<Builder: NodeOps>(&mut self, builder: Exotic<Builder>) -> ParseResult<Expr> {
    return Ok(Expr::try_from(next_parse!(self, self.parse_class(builder, true, false))?).expect("Failed to convert Class to Expr"));
  }

  fn parse_class<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    is_name_omittable: bool,
    should_declare_name: bool,
  ) -> ParseResult<Ast> {
    self.parser_state.enter_state(ParserState::InClassScope);
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Class);
    let mut name = None;
    if !self.cur().one_of(&[Token::Extends, Token::LeftBrace]) {
      if should_declare_name {
        self.declare_var(
          if self.statement_block.is_export() {
            VariableType::ExportLexical
          } else {
            VariableType::Lexical
          },
          (self.value().clone(), self.source_position().clone()),
        )?;
      }
      name = Some(next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::None))?);
    } else if !is_name_omittable {
      return parse_error!(self.region, "Class name required", &start_pos);
    }
    let mut heritage = None;
    if self.cur() == Token::Extends {
      self.advance()?;
      heritage = Some(next_parse!(self, self.parse_left_hand_side_expression(builder))?);
      expect!(self, self.cur(), Token::LeftBrace);
    } else {
      expect!(self, self.cur(), Token::LeftBrace);
    }
    let mut fields = Vec::new();
    let mut methods = Vec::new();
    next_parse!(self, self.parse_class_body(builder, &mut fields, &mut methods, heritage.is_some()))?;

    expect!(self, self.cur(), Token::RightBrace);
    self.parser_state.leave_state(ParserState::InClassScope);

    return Ok(build!(
      @pos,
      builder,
      class,
      start_pos,
      name,
      heritage,
      methods,
      fields
    ));
  }

  fn parse_class_body<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    fields: &mut Vec<Stmt>,
    methods: &mut Vec<Stmt>,
    has_heritage: bool,
  ) -> ParseResult<()> {
    let mut is_constructor_seen = false;
    while self.cur() != Token::RightBrace {
      let start_pos = self.source_position().clone();
      let mut start_pos_after_static = self.source_position().clone();
      let (attrs, class_field_attr) = next_parse!(self, self.parse_method_attributes(true))?;
      if class_field_attr.contains(ClassFieldFlag::STATIC) {
        start_pos_after_static.set_start_col(start_pos.start_col() + (Token::Static.symbol().len() as u64) + 1);
      }
      if attrs.contains(FunctionAttribute::GETTER) {
        start_pos_after_static.set_start_col(start_pos_after_static.start_col() + (Token::Get.symbol().len() as u64) + 1);
      } else if attrs.contains(FunctionAttribute::SETTER) {
        start_pos_after_static.set_start_col(start_pos_after_static.start_col() + (Token::Set.symbol().len() as u64) + 1);
      }

      let prop_name_pos = self.source_position().clone();
      let is_constructor = self.cur().one_of(&[Token::Identifier, Token::StringLiteral]) && self.is_value_match_with(self.constructor_str);
      let contextual_keyword = self.contextual_keyword();
      let name = next_parse!(self, self.parse_property_name(builder))?;
      if self.cur() == Token::LeftParen {
        if !class_field_attr.contains(ClassFieldFlag::STATIC) && is_constructor {
          if contextual_keyword == Token::PrivateIdentifier {
            return parse_error!(self.region, "constructor function may not be private", &prop_name_pos);
          }
          if attrs.intersects(FunctionAttribute::GETTER | FunctionAttribute::SETTER) {
            return parse_error!(self.region, "constructor function may not be getter or setter", &prop_name_pos);
          }
          if attrs.contains(FunctionAttribute::ASYNC) {
            return parse_error!(self.region, "constructor function may not be async function", &prop_name_pos);
          }
          if attrs.contains(FunctionAttribute::GENERATOR) {
            return parse_error!(self.region, "constructor function may not be generator function", &prop_name_pos);
          }
          if is_constructor_seen {
            return parse_error!(self.region, "constructor function allowed only one declration", &prop_name_pos);
          }
          is_constructor_seen = true;
        } else if class_field_attr.contains(ClassFieldFlag::STATIC) && contextual_keyword == Token::Prototype {
          return parse_error!(self.region, "prototype static property is not allowed", &prop_name_pos);
        }
        let value = next_parse!(
          self,
          self.parse_method_body(builder, &start_pos_after_static, name, attrs, has_heritage)
        )?;
        let field = build!(
          @pos,
          builder,
          class_field,
          start_pos,
          class_field_attr,
          value
        );
        builder.push_stmt(methods, field);
      } else {
        let value = if self.cur() == Token::OpAssign {
          self.advance()?;
          let expr = next_parse!(self, self.parse_assignment_expression(builder))?;
          if let Some(e) = self.expression_context.first_value_error() {
            return Err(e);
          }
          build!(
            @pos,
            builder,
            binary_expr,
            start_pos,
            Token::OpAssign,
            name,
            expr,
          )
        } else {
          name
        };
        let field = build!(
          @pos,
          builder,
          class_field,
          start_pos,
          class_field_attr,
          value
        );
        builder.push_stmt(fields, field);
      }
    }

    return Ok(());
  }

  fn parse_module_item<Builder: NodeOps>(&mut self, builder: Exotic<Builder>) -> ParseResult<Stmt> {
    match self.cur() {
      Token::Import => return next_parse!(self, self.parse_import_declaration(builder)),
      Token::Export => return next_parse!(self, self.parse_export_declaration(builder)),
      _ => return next_parse!(self, self.parse_statement_list_item(builder)),
    }
  }

  fn parse_import_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    debug_assert!(self.cur() == Token::Import);
    let start = self.source_position().clone();
    self.advance()?;
    if self.cur() == Token::StringLiteral {
      let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()), Token::Invalid);
      let t = self.cur();
      let str = build!(@pos, builder, literal, start, t, val);
      return Ok(build!(@pos, builder, import_decl, start, None, str));
    }

    let ib = next_parse!(self, self.parse_import_binding(builder, true))?;

    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::From {
      self.advance()?;
      let literal_pos = self.source_position().clone();
      if self.cur() == Token::StringLiteral {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()), Token::Invalid);
        let t = self.cur();
        self.advance()?;
        let str = build!(@pos, builder, literal, literal_pos, t, val);
        let node = build!(@pos, builder, import_decl, start, Some(ib.into()), str);
        return next_parse!(self, self.parse_terminator(node));
      }
    }

    return parse_error!(self.region, "Module specifier expected", self.source_position());
  }

  fn parse_import_binding<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, is_default_allowed: bool) -> ParseResult<Expr> {
    let binding_start = self.source_position().clone();
    match self.cur() {
      Token::LeftBrace => {
        let node = next_parse!(self, self.parse_named_import(builder))?;
        if !is_default_allowed {
          return Ok(node);
        }
        return Ok(build!(@pos, builder, import_binding, binding_start, None, Some(node)));
      }
      Token::OpMul => {
        let node = next_parse!(self, self.parse_namespace_import(builder, true))?;
        if !is_default_allowed {
          return Ok(node);
        }
        return Ok(build!(@pos, builder, import_binding, binding_start, None, Some(node)));
      }
      Token::Identifier => {
        if !is_default_allowed {
          return parse_error!(self.region, "default import only allowed once", self.source_position());
        }
        let args = (self.value().clone(), self.source_position().clone());
        self.declare_module_header_var(args)?;
        let default_binding = next_parse!(self, self.parse_identifier(builder, ParserConstraints::None))?;
        self.advance()?;
        let other_binding = if self.cur() == Token::Comma {
          self.advance()?;
          Some(next_parse!(self, self.parse_import_binding(builder, false))?)
        } else {
          None
        };
        return Ok(build!(@pos, builder, import_binding, binding_start, Some(default_binding), other_binding));
      }
      _ => return parse_error!(self.region, "Import specifier expected", &binding_start),
    };
  }

  fn parse_namespace_import<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>, is_as_required: bool) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::OpMul);
    if self.cur() == Token::Identifier && self.contextual_keyword() == Token::As {
      self.advance()?;
      let args = (self.value().clone(), self.source_position().clone());
      self.declare_module_header_var(args)?;
      let expr = next_parse!(self, self.parse_single_name_binding(builder, ParserConstraints::None))?;
      return Ok(build!(@pos, builder, import_specifier, start, true, None, Some(expr)));
    }
    if is_as_required {
      return parse_error!(self.region, "'as' expected", self.source_position());
    }
    return Ok(build!(@pos, builder, import_specifier, start, true, None, None));
  }

  #[inline]
  fn parse_named_import<Builder: NodeOps>(&mut self, builder: Exotic<Builder>) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list(builder, true, false));
  }

  fn parse_export_declaration<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Stmt> {
    let start_pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Export);
    let mut export_type = ExportDeclarationType::NAMESPACE;
    let mut export_clause: Option<Ast> = None;
    let mut should_check_terminator = true;
    let parent_block = self.new_statement_block();
    self.statement_block.set_is_export(true);

    if self.cur() == Token::Default {
      if self.current_scope.is_default_exported() {
        return parse_error!(self.region, "Default export may not contains twice", self.source_position());
      }
      self.current_scope.mark_as_default_exported();
      export_type = ExportDeclarationType::DEFAULT;
      self.advance()?;
      match self.cur() {
        Token::Class => {
          should_check_terminator = false;
          export_clause = Some(
            next_parse!(
              self,
              self.parse_class_declaration(builder, DeclarationParseOption::DEFAULT_EXPORT_RHS)
            )?
            .into(),
          );
        }
        Token::Function => {
          should_check_terminator = false;
          export_clause = Some(
            next_parse!(
              self,
              self.parse_function_declaration(builder, FunctionAttribute::NONE, DeclarationParseOption::DEFAULT_EXPORT_RHS)
            )?
            .into(),
          );
        }
        Token::Identifier => {
          if self.contextual_keyword() == Token::Async && self.peek()? == Token::Function {
            should_check_terminator = false;
            export_clause = Some(
              next_parse!(
                self,
                self.parse_function_declaration(builder, FunctionAttribute::ASYNC, DeclarationParseOption::DEFAULT_EXPORT_RHS)
              )?
              .into(),
            );
          } else {
            export_clause = Some(next_parse!(self, self.parse_assignment_expression(builder))?.into());
          }
        }
        _ => {
          export_clause = Some(next_parse!(self, self.parse_assignment_expression(builder))?.into());
          if let Some(e) = self.expression_context.first_value_error() {
            return Err(e);
          }
        }
      }
      self.set_statement_block(parent_block);

      let node = build!(@pos, builder, export_decl, start_pos, export_type, export_clause, None);
      if should_check_terminator {
        return next_parse!(self, self.parse_terminator(node));
      } else {
        return Ok(node);
      }
    }

    let mut from_clause = None;
    match self.cur() {
      Token::Var => {
        should_check_terminator = false;
        export_clause = Some(
          next_parse!(
            self,
            self.parse_lexical_or_variable_declaration(
              builder,
              VariableDeclarationType::Var,
              DeclarationParseOption::from_named_export_stmt()
            )
          )?
          .into(),
        );
      }
      Token::Const | Token::Function | Token::Class => {
        should_check_terminator = false;
        export_clause = Some(
          next_parse!(
            self,
            self.parse_declaration(builder, DeclarationParseOption::from_named_export_stmt())
          )?
          .into(),
        );
      }
      Token::Identifier => {
        if self.contextual_keyword().one_of(&[Token::Let, Token::Async]) {
          should_check_terminator = false;
          export_clause = Some(
            next_parse!(
              self,
              self.parse_declaration(builder, DeclarationParseOption::from_named_export_stmt())
            )?
            .into(),
          );
        } else {
          return parse_error!(self.region, "Unexpected token found", self.source_position());
        }
      }
      _ => {
        let is_from_required = if self.cur() == Token::OpMul {
          export_clause = Some(next_parse!(self, self.parse_namespace_import(builder, false))?.into());
          true
        } else {
          export_clause = Some(next_parse!(self, self.parse_export_clause(builder))?.into());
          false
        };
        if self.cur() == Token::Identifier && self.contextual_keyword() == Token::From {
          self.advance()?;
          if self.cur() == Token::StringLiteral {
            let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()), Token::Invalid);
            from_clause = Some(build!(self, builder, literal, Token::StringLiteral, val).into());
            self.advance()?;
          } else {
            return parse_error!(self.region, "Module specifier expected", self.source_position());
          }
        } else if is_from_required {
          return parse_error!(self.region, "From expected", self.source_position());
        }
      }
    }

    let export_decl = build!(@pos, builder, export_decl, start_pos, export_type, export_clause, from_clause);
    if should_check_terminator {
      return next_parse!(self, self.parse_terminator(export_decl));
    }
    return Ok(export_decl);
  }

  fn parse_export_clause<Builder: NodeOps>(&mut self, mut builder: Exotic<Builder>) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list(builder, false, true));
  }

  #[inline]
  fn parse_named_list<Builder: NodeOps>(
    &mut self,
    mut builder: Exotic<Builder>,
    allow_reserved_keyword: bool,
    should_register_will_be_exported_var: bool,
  ) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftBrace);
    let mut list = Vec::<Expr>::new();
    let constraints = if allow_reserved_keyword {
      ParserConstraints::KeywordIdentifier
    } else {
      ParserConstraints::None
    };

    while self.has_more() && self.cur() != Token::RightBrace {
      let specifier_start_pos = self.source_position().clone();
      let name_value = self.value().clone();
      let identifier = next_parse!(self, self.parse_identifier_reference(builder, constraints))?;
      let node = if self.cur() == Token::Identifier && self.contextual_keyword() == Token::As {
        self.advance()?;
        let pos = self.source_position().clone();
        let val = self.value().clone();
        if should_register_will_be_exported_var {
          self.declare_var(VariableType::WillExportVar, (val, pos))?;
        } else {
          self.declare_module_header_var((val, pos))?;
        }
        let value_ref = next_parse!(self, self.parse_identifier_reference(builder, constraints))?;
        build!(@pos, builder, import_specifier, specifier_start_pos, false, Some(identifier), Some(value_ref))
      } else {
        if should_register_will_be_exported_var {
          self.declare_var(VariableType::WillExportVar, (name_value, specifier_start_pos.clone()))?;
        } else {
          self.declare_module_header_var((name_value, specifier_start_pos.clone()))?;
        }
        build!(@pos, builder, import_specifier, specifier_start_pos, false, Some(identifier), None)
      };
      builder.push_expr(&mut list, node);

      if self.cur() == Token::Comma {
        self.advance()?;
      } else if self.cur() != Token::RightBrace {
        return parse_error!(self.region, "'}' expected", self.source_position());
      }
    }

    expect!(self, self.cur(), Token::RightBrace);
    return Ok(build!(@pos, builder, named_import_list, start, list).into());
  }

  fn declare_module_header_var(&mut self, var: (Vec<u16>, SourcePosition)) -> ParseResult<()> {
    return self.declare_var(VariableType::Lexical, var);
  }

  fn declare_var(&mut self, var_type: VariableType, var: (Vec<u16>, SourcePosition)) -> ParseResult<()> {
    if let Some((cur, first)) = self.current_scope.declare_var(var_type, var) {
      return parse_error!(
        self.region,
        &format!(
          "Lexical declaration may not contains any duplicated identifiers\n\n{}\nBut found",
          format_error(&self.source.filename(), self.source.source_code(), "First defined", &first, false)
        ),
        &cur
      );
    }
    return Ok(());
  }
}
