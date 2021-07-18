use super::super::structs::FixedU16CodePointArray;
use super::ast::*;
use super::error_reporter::*;
use super::parser_def::*;
use super::parser_state::{ParserState, ParserStateStack};
use super::scanner::*;
use super::source_position::*;
use super::token::Token;
use crate::context::{Context, LuxContext, ObjectRecordsInitializedContext};
use crate::utility::*;
use std::char::decode_utf16;

macro_rules! expect {
  ($self:tt, $n:expr, $token:expr) => {
    if $n != $token {
      return Err(format!("'{}' expected", $token.symbol()));
    }
    $self.advance();
  };
  (@oneof $self:tt, $n:expr, $($token:expr,)+) => {
    if !$n.one_of(&[$($token,)*]) {
      let s = String::new();
      $(
        s.push_str(format!("'{}' "$token.symbol()));
      )*;
      s.push_str("expected");
      return Err(s);
    }
    $self.advance();
  };
}

#[cfg(feature = "print_ast")]
struct ParserDebugger {
  next_parse: &'static str,
  current_token: Token,
  position: SourcePosition,
  has_error: bool,
  indent: String,
  buffer: String,
}

#[cfg(feature = "print_ast")]
impl ParserDebugger {
  fn new() -> Self {
    ParserDebugger {
      next_parse: "",
      current_token: Token::Invalid,
      position: SourcePosition::new(),
      has_error: false,
      indent: "".to_string(),
      buffer: String::new(),
    }
  }
  fn enter(&mut self, next_parse: &'static str, current_token: Token, position: &SourcePosition, has_error: bool) {
    self.next_parse = next_parse;
    self.current_token = current_token;
    self.position = *position;
    self.has_error = has_error;

    self.buffer = format!(
      "{}\n{}Enter {}: CurrentToken = {} {:?}",
      self.buffer, self.indent, self.next_parse, self.current_token, self.position
    );
    self.indent = format!("  {}", self.indent);
  }

  fn leave<T>(&mut self, result: &ParseResult<T>) {
    self.indent = self.indent.chars().take(self.indent.len() - 2).collect::<String>();
    self.buffer = format!(
      "{}\n{}Exit {} {} CurrentToken = {:?}{} {:?}",
      self.buffer,
      self.indent,
      self.next_parse,
      if result.is_ok() { "Success" } else { "Failure" },
      self.current_token,
      if self.has_error { "[Error]" } else { "" },
      self.position
    );
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
    $self
      .debugger
      .enter(stringify!($call), cur, &source_position, has_pending_error);
    let result = $call;
    $self.debugger.leave(&result);
    result
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
enum ParserType {
  Script,
  Module,
}
pub struct Parser {
  context: LuxContext,
  parser_type: ParserType,
  parser_state: Exotic<ParserStateStack>,
  scanner: Exotic<Scanner>,
  region: Region,
  result: Option<Ast>,
  source: FixedU16CodePointArray,
  scanner_record: Option<ScannerRecord>,
  error_reporter: ErrorReporter,
  is_strict_mode: bool,
  debugger: ParserDebugger,
  empty: Node<Empty>,
}

macro_rules! new_node {
  ($self:tt, $name:tt, $($args:expr),+$(,)?) => {
    $name::new(&mut $self.region, $($args),*)
  };
  ($self:tt, $name:tt) => {
    $name::new(&mut $self.region)
  }
}

macro_rules! new_node_with_pos {
  ($self:tt, $name:tt, $pos:expr, $($args:expr),+$(,)?) => {{
    let p = $pos;
    let mut node = $name::new(&mut $self.region, $($args),*);
    node.set_source_position(&p);
    node
  }};
  ($self:tt, $name:tt, $pos:expr) => {{
    let p = $pos;
    let mut node = $name::new(&mut $self.region);
    node.set_source_position(&p);
    node
  }}
}

macro_rules! new_node_with_positions {
  ($self:tt, $name:tt, $pos:expr, $end:expr, $($args:expr),+$(,)?) => {{
    let p = $pos;
    let pe = $end;
    let mut node = $name::new(&mut $self.region, $($args),*);
    node.set_start_position(&p);
    node.set_end_position(&pe);
    node
  }};
  ($self:tt, $name:tt, $pos:expr, $end:expr) => {{
    let p = $pos;
    let pe = $end;
    let mut node = $name::new(&mut $self.region);
    node.set_start_position(&p);
    node.set_end_position(&pe);
    node
  }}
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
  pub fn new(context: impl ObjectRecordsInitializedContext, source: &str) -> Self {
    let mut region = Region::new();
    let empty = Empty::new(&mut region);
    let mut parser = Parser {
      context: LuxContext::from_allocation_only_context(context),
      parser_type: ParserType::Script,
      parser_state: Exotic::new(std::ptr::null_mut()),
      scanner: Exotic::new(std::ptr::null_mut()),
      region,
      result: None,
      source: FixedU16CodePointArray::from_utf8(context, source),
      scanner_record: None,
      error_reporter: ErrorReporter::new(),
      is_strict_mode: false,
      debugger: ParserDebugger::new(),
      empty,
    };
    parser.parser_state = parser.region.alloc(ParserStateStack::new());
    parser.scanner = parser.region.alloc(Scanner::new(parser.source, parser.parser_state));
    return parser;
  }

  #[inline(always)]
  fn cur(&self) -> Token {
    return self.scanner.cur();
  }

  #[inline(always)]
  fn advance(&mut self) -> Token {
    return self.scanner.next();
  }

  #[inline(always)]
  fn peek(&mut self) -> Token {
    return self.scanner.peek();
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
    return self.cur() != Token::End && self.scanner.has_more();
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

  #[inline(always)]
  fn restore(&mut self) {
    if let Some(ref record) = self.scanner_record {
      self.scanner.restore(record);
    }
    self.scanner_record = None;
  }

  fn is_value_match_with(&self, expectation: &str) -> bool {
    return self.is_value_match_with_str(self.value(), expectation);
  }

  fn is_value_match_with_str(&self, value: &Vec<u16>, expectation: &str) -> bool {
    let bytes = expectation.as_bytes();
    for (i, uc) in value.iter().enumerate() {
      if i >= bytes.len() || (bytes[i] as u16) != *uc {
        return false;
      }
    }
    return true;
  }

  fn value_to_utf8(value: &Vec<u16>) -> String {
    return decode_utf16(value.iter().cloned())
      .map(|r| r.unwrap_or('#'))
      .collect::<String>();
  }
}

impl ParserDef for Parser {
  fn parse_directive_prologue(&mut self) {
    if self.cur() == Token::StringLiteral && self.is_value_match_with("use strict") {
      self.advance();
      self.is_strict_mode = true;
      if self.cur() == Token::Terminate {
        self.advance();
      } else if !self.has_line_break_before() {
        report_syntax_error!(noreturn self, "; expected");
      }
    }
  }

  fn parse_program(&mut self) {
    self.parse_directive_prologue();
    if self.error_reporter.has_pending_error() {
      return;
    }
    if self.parser_type == ParserType::Script {
      if let Ok(result) = self.parse_script() {
        self.result = Some(result);
      }
    } else {
      if let Ok(result) = self.parse_module() {
        self.result = Some(result);
      }
    }
  }

  fn parse_script(&mut self) -> ParseResult<Ast> {
    return next_parse!(self, self.parse_script_body().map(|a| Into::<Ast>::into(a)));
  }

  fn parse_script_body(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }

  fn parse_module(&mut self) -> ParseResult<Ast> {
    return self.parse_module_body().map(|a| Into::<Ast>::into(a));
  }

  fn parse_module_body(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }

  fn parse_terminator<T>(&mut self, expr: T) -> ParseResult<T> {
    if self.cur() == Token::Terminate {
      self.advance();
      return Ok(expr);
    } else if self.cur() == Token::End && self.cur() != Token::RightBrace && !self.has_line_break_after() {
      return Err("';' expected".to_string());
    }

    return Ok(expr);
  }

  fn parse_identifier(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_identifier_reference(&mut self) -> ParseResult<Expr> {
    scoped!(|| {
      self.advance();
    });
    let start = self.source_position().clone();
    match self.cur() {
      Token::Identifier | Token::Yield | Token::Await => {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        return Ok(new_node_with_pos!(self, Literal, &start, Token::Identifier, val).into());
      }
      _ => {
        return Err("Literal expected".to_string());
      }
    }
  }

  fn parse_primary_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::This => {
        self.advance();
        let expr = new_node_with_pos!(self, Literal, &start, Token::This, LiteralValue::None);
        return Ok(expr.into());
      }
      Token::LeftBracket => {
        return next_parse!(self, self.parse_array_literal(ParserConstraints::None));
      }
      Token::LeftBrace => {
        return next_parse!(self, self.parse_array_literal(ParserConstraints::None));
      }
      Token::Function => {
        if self.peek() == Token::OpMul {
          return next_parse!(self, self.parse_generator_expression());
        }
        return next_parse!(self, self.parse_function_expression(false));
      }
      Token::Class => {
        return next_parse!(self, self.parse_class_expression());
      }
      Token::Identifier => {
        if self.is_value_match_with("async") {
          if self.peek() == Token::Terminate {
            return next_parse!(self, self.parse_async_function_expression());
          }
        }
        return next_parse!(self, self.parse_identifier_reference());
      }
      Token::ImplicitOctalLiteral => {
        if self.is_strict_mode {
          return Err("Implicit octal literal not allowed in strict mode".to_string());
        }
        return next_parse!(self, self.parse_literal());
      }
      Token::Null | Token::True | Token::False | Token::StringLiteral => {
        return next_parse!(self, self.parse_literal());
      }
      Token::BackQuote => {
        return next_parse!(self, self.parse_template_literal());
      }
      Token::OpDiv => {
        return next_parse!(self, self.parse_regular_expression());
      }
      Token::LeftParen => {
        return next_parse!(self, self.parse_cover_call_expression_and_async_arrow_head());
      }
      _ => {
        return Err("".to_string());
      }
    }
  }

  fn parse_literal(&mut self) -> ParseResult<Expr> {
    scoped!(|| {
      self.advance();
    });

    match self.cur() {
      Token::Null | Token::True | Token::False => {
        let t = self.cur();
        return Ok(new_node!(self, Literal, t, LiteralValue::None).into());
      }
      Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        let t = self.cur();
        let val = LiteralValue::Number(self.numeric_value());
        return Ok(new_node!(self, Literal, t, val).into());
      }
      Token::StringLiteral => {
        let t = self.cur();
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        return Ok(new_node!(self, Literal, t, val).into());
      }
      _ => return Err("Literal expected".to_string()),
    }
  }

  fn parse_regular_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_array_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr> {
    debug_assert!(self.cur() == Token::LeftBracket);
    let mut array = new_node!(self, StructuralLiteral, StructuralLiteralType::Array);
    array.set_source_position(self.source_position());
    self.advance();
    while !self.cur().one_of(&[Token::RightBracket, Token::Invalid, Token::End]) {
      if self.cur() == Token::Comma {
        let start_pos = self.source_position().clone();
        self.advance();
        match self.cur() {
          Token::Comma | Token::RightBracket => {
            array.push(new_node_with_pos!(self, Elision, start_pos).into());
          }
          _ => {
            let pos = self.source_position().clone();
            array.push(new_node_with_pos!(self, Elision, pos).into());
          }
        }
      } else {
        if self.cur() == Token::Spread {
          let a = next_parse!(self, self.parse_spread_element())?;
          array.push(a);
        } else {
          let mut expr = next_parse!(self, self.parse_assignment_expression())?;
          match expr {
            Expr::Literal(lit) => {
              if lit.is_identifier() {
                expr.set_valid_lhs();
              } else {
                expr.unset_valid_lhs();
              }
            }
            _ => expr.unset_valid_lhs(),
          };
          array.push(expr);
        }
      }
    }
    array.set_end_position(self.source_position());
    expect!(self, self.cur(), Token::RightBracket);
    return Ok(array.into());
  }
  fn parse_element_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_spread_element(&mut self) -> ParseResult<Expr> {
    let start_pos = self.source_position().clone();
    debug_assert!(self.cur() == Token::Spread);
    self.advance();
    let expr = next_parse!(self, self.parse_assignment_expression())?;
    let mut node = new_node_with_pos!(
      self,
      UnaryExpression,
      start_pos,
      UnaryExpressionOperandPosition::Pre,
      Token::Spread,
      expr
    );
    node.set_end_position(expr.source_position());
    return Ok(node.into());
  }
  fn parse_object_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    debug_assert!(self.cur() == Token::LeftBrace);
    self.advance();
    let mut object = new_node_with_pos!(self, StructuralLiteral, start, StructuralLiteralType::Object);
    if self.cur() == Token::RightBrace {
      self.advance();
      return Ok(object.into());
    }

    while !self.cur().one_of(&[Token::RightBrace, Token::Invalid, Token::End]) {
      let prop = next_parse!(self, self.parse_object_literal_property(constraints))?;
      object.push(prop);
      if self.cur() == Token::Comma {
        self.advance();
      }
    }

    object.set_end_position(self.source_position());
    expect!(self, self.cur(), Token::RightBrace);
    return Ok(object.into());
  }
  fn parse_object_literal_property(&mut self, constraints: ParserConstraints) -> ParseResult<Expr> {
    let mut key: ParseResult<Expr> = Err("".to_string());
    let mut is_computed_property_name = false;
    let start = self.source_position().clone();

    if self.cur().one_of(&[
      Token::NumericLiteral,
      Token::ImplicitOctalLiteral,
      Token::Identifier,
      Token::StringLiteral,
      Token::LeftBracket,
    ]) {
      if self.peek() == Token::LeftParen {
        return next_parse!(self, self.parse_method_definition());
      }

      key = if self.cur() == Token::Identifier {
        next_parse!(self, self.parse_identifier_reference())
      } else {
        next_parse!(self, self.parse_property_name())
      };

      if self.cur() == Token::OpAssign {
        if constraints.is_binding_pattern_allowed() || is_computed_property_name {
          return Err("Unexpected '=' detected".to_string());
        }
        let ident = next_parse!(self, self.parse_identifier_reference())?;
        let value = next_parse!(self, self.parse_assignment_expression())?;
        return Ok(
          new_node_with_positions!(
            self,
            ObjectPropertyExpression,
            start,
            value.source_position(),
            ident,
            None,
            Some(value)
          )
          .into(),
        );
      } else if self.cur().one_of(&[Token::Comma, Token::RightBrace]) {
        let key_node = key?;
        return Ok(
          new_node_with_positions!(
            self,
            ObjectPropertyExpression,
            start,
            key_node.source_position().clone(),
            key_node,
            Some(key_node),
            None
          )
          .into(),
        );
      }
    } else {
      return Err(format!("Property name must be one of 'identifier', 'string literal', 'numeric literal' or 'computed property' but got {}", self.cur().symbol()));
    }

    if key.is_err() {
      return Err("Expression expected".to_string());
    }

    let key_node = key.unwrap();

    if self.cur() == Token::Colon {
      match key_node {
        Expr::Literal(lit) => {
          if lit.is_identifier() {
            return Ok(
              new_node_with_positions!(
                self,
                ObjectPropertyExpression,
                start,
                key_node.source_position().clone(),
                key_node,
                Some(key_node),
                None
              )
              .into(),
            );
          }
        }
        _ => {}
      }
    }

    expect!(self, self.cur(), Token::Colon);
    let value = next_parse!(self, self.parse_assignment_expression())?;
    if constraints.is_binding_pattern_allowed() {
      match value {
        Expr::Literal(node) => {
          if !node.is_identifier() {
            return Err("Identifier expected".to_string());
          }
        }
        Expr::StructuralLiteral(_) => {}
        _ => {
          return Err("Identifier expected".to_string());
        }
      }
    }

    return Ok(
      new_node_with_positions!(
        self,
        ObjectPropertyExpression,
        start,
        value.source_position().clone(),
        key_node,
        Some(value),
        None
      )
      .into(),
    );
  }
  fn parse_property_definition_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_property_definition(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_property_name(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::Identifier => {
        return next_parse!(self, self.parse_identifier());
      }
      Token::StringLiteral | Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        return next_parse!(self, self.parse_literal());
      }
      _ => {
        let start = self.source_position().clone();
        expect!(self, self.cur(), Token::LeftBracket);
        let ret = next_parse!(self, self.parse_assignment_expression())?;
        let end = self.source_position().clone();
        expect!(self, self.cur(), Token::RightBracket);
        return Ok(
          new_node_with_positions!(
            self,
            PropertyAccessExpression,
            start,
            end,
            PropertyAccessType::Element,
            CallReceiverType::None,
            None,
            Some(ret)
          )
          .into(),
        );
      }
    };
  }
  fn parse_cover_initialized_name(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_initializer(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_template_literal(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_template_spans(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_template_middle_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_member_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();

    if self.cur() == Token::Super {
      self.advance();
      return next_parse!(
        self,
        self.parse_post_member_expression(
          &start,
          self.empty.into(),
          CallReceiverType::Super,
          ParserConstraints::None,
          true
        )
      );
    }

    if self.cur() == Token::New {
      self.advance();
      if self.cur() == Token::Dot {
        self.advance();
        if self.peek() == Token::Identifier {
          if self.is_value_match_with_str(self.peek_value(), "target") {
            return Ok(
              new_node!(
                self,
                PropertyAccessExpression,
                PropertyAccessType::Dot,
                CallReceiverType::New,
                None,
                None
              )
              .into(),
            );
          }
          return Err(format!(
            "new.target? but got {}",
            Parser::value_to_utf8(self.peek_value())
          ));
        }
        return Err("new.target? identifier 'target' expected".to_string());
      }
      let start_call = self.source_position().clone();
      let m = next_parse!(self, self.parse_member_expression())?;
      if self.cur() == Token::LeftParen {
        let mut expr = new_node_with_pos!(self, NewExpression, &start, m);
        expr.set_end_position(m.source_position());
        return Ok(expr.into());
      }
      let args = next_parse!(self, self.parse_arguments())?;
      let mut call = new_node_with_pos!(
        self,
        CallExpression,
        &start_call,
        CallReceiverType::Expr,
        m,
        Some(Node::<Expressions>::try_from(args).unwrap())
      );
      call.set_end_position(args.source_position());
      let mut expr = new_node_with_pos!(self, NewExpression, &start, call.into());
      expr.set_end_position(args.source_position());
      return next_parse!(
        self,
        self.parse_post_member_expression(
          &start,
          expr.into(),
          CallReceiverType::Expr,
          ParserConstraints::Template,
          false
        )
      );
    }

    let n = next_parse!(self, self.parse_primary_expression())?;
    return next_parse!(
      self,
      self.parse_post_member_expression(&start, n, CallReceiverType::Expr, ParserConstraints::Template, false)
    );
  }
  fn parse_post_member_expression(
    &mut self,
    source_position: &SourcePosition,
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
          let a = next_parse!(self, self.parse_arguments())?;
          current = new_node_with_pos!(
            self,
            CallExpression,
            source_position,
            receiver_type,
            current,
            Some(Node::<Expressions>::try_from(a).unwrap())
          )
          .into();
        }
        Token::LeftBracket => {
          self.advance();
          let n = next_parse!(self, self.parse_expression())?;
          let end = self.source_position().clone();
          expect!(self, self.cur(), Token::RightBracket);
          let mut expr = new_node_with_pos!(
            self,
            PropertyAccessExpression,
            source_position,
            PropertyAccessType::Element,
            receiver_type,
            Some(current),
            Some(n)
          );
          expr.set_end_position(&end);
          current = expr.into();
        }
        Token::Dot => {
          self.advance();
          let ident = next_parse!(self, self.parse_identifier_reference())?;
          let expr = new_node_with_pos!(
            self,
            PropertyAccessExpression,
            source_position,
            PropertyAccessType::Dot,
            receiver_type,
            Some(current),
            Some(ident)
          );
          current = expr.into();
        }
        Token::BackQuote => {
          if !constraints.is_template_allowed() {
            return Err("Unexpected '`' found".to_string());
          }
          let tmpl = next_parse!(self, self.parse_template_literal())?;
          current = new_node_with_pos!(
            self,
            CallExpression,
            source_position,
            CallReceiverType::Template,
            current,
            Some(Node::<Expressions>::try_from(tmpl).unwrap())
          )
          .into();
        }
        _ => {
          if error_if_default {
            return Err(format!("Unexpected {} found", self.cur().symbol()));
          }
          return Ok(current);
        }
      }
    }

    return Ok(current);
  }
  fn parse_super_property(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_new_target(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_new_expression(&mut self) -> ParseResult<Expr> {
    if self.cur() == Token::New {
      let start = self.source_position().clone();
      if self.peek() == Token::New {
        self.advance();
        let callee = next_parse!(self, self.parse_new_expression())?;
        let mut expr = new_node_with_pos!(self, NewExpression, start, callee);
        expr.set_end_position(callee.source_position());
        return Ok(expr.into());
      }
      return next_parse!(self, self.parse_member_expression());
    }
    return next_parse!(self, self.parse_member_expression());
  }
  fn parse_call_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::Super => {
        self.record();
        self.advance();
        if self.cur() == Token::LeftParen {
          self.restore();
          let n = next_parse!(self, self.parse_super_call())?;
          return next_parse!(
            self,
            self.parse_post_member_expression(
              &start,
              n,
              CallReceiverType::Super,
              ParserConstraints::Call | ParserConstraints::Template,
              true
            )
          );
        }
        self.restore();
      }
      _ => {}
    }
    let mut expr = next_parse!(self, self.parse_cover_call_expression_and_async_arrow_head())?;
    if let Ok(exprs) = Node::<Expressions>::try_from(expr) {
      if exprs.is_arguments() {
        expr = new_node!(
          self,
          CallExpression,
          CallReceiverType::Expr,
          *exprs.at(0).unwrap(),
          Some(Node::<Expressions>::try_from(*exprs.at(1).unwrap()).unwrap())
        )
        .into();
      }
    }
    return next_parse!(
      self,
      self.parse_post_member_expression(
        &start,
        expr,
        CallReceiverType::Expr,
        ParserConstraints::Call | ParserConstraints::Template,
        true
      )
    );
  }
  fn parse_cover_call_expression_and_async_arrow_head(&mut self) -> ParseResult<Expr> {
    let n = next_parse!(self, self.parse_member_expression())?;
    if self.cur() == Token::LeftParen {
      let a = next_parse!(self, self.parse_arguments())?;
      let mut exprs = new_node!(self, Expressions);
      exprs.push(n);
      exprs.push(a);
      return Ok(exprs.into());
    }
    return Ok(n);
  }
  fn parse_call_member_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_super_call(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_arguments(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_arguments_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_left_hand_side_expression(&mut self) -> ParseResult<Expr> {
    let set_lhs = |mut node: Expr| {
      node.set_valid_lhs();
      return node;
    };
    match self.cur() {
      Token::New => {
        if self.peek() == Token::Dot {
          self.advance();
          return next_parse!(self, self.parse_call_expression().map(set_lhs));
        }
        return next_parse!(self, self.parse_new_expression().map(set_lhs));
      }
      _ => {
        return next_parse!(self, self.parse_call_expression().map(set_lhs));
      }
    }
  }
  fn parse_update_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::OpIncrement | Token::OpDecrement => {
        let op = self.cur();
        self.advance();
        let n = next_parse!(self, self.parse_left_hand_side_expression())?;
        let mut update = new_node_with_pos!(self, UnaryExpression, start, UnaryExpressionOperandPosition::Pre, op, n);
        update.set_end_position(n.source_position());
        return Ok(update.into());
      }
      _ => {
        let n = next_parse!(self, self.parse_left_hand_side_expression())?;
        match self.cur() {
          Token::OpIncrement | Token::OpDecrement => {
            scoped!(|| {
              self.advance();
            });
            let token = self.cur();
            let sp = self.source_position().clone();
            return Ok(
              new_node_with_positions!(
                self,
                UnaryExpression,
                start,
                sp,
                UnaryExpressionOperandPosition::Post,
                token,
                n
              )
              .into(),
            );
          }
          _ => return Ok(n),
        }
      }
    }
  }
  fn parse_unary_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    match self.cur() {
      Token::Delete | Token::Void | Token::Typeof | Token::OpPlus | Token::OpMinus | Token::OpTilde | Token::OpNot => {
        let op = self.cur();
        self.advance();
        let rhs_exp = next_parse!(self, self.parse_unary_expression())?;
        let mut unary = new_node!(self, UnaryExpression, UnaryExpressionOperandPosition::Pre, op, rhs_exp);
        unary.set_start_position(&start);
        unary.set_end_position(rhs_exp.source_position());
        return Ok(unary.into());
      }
      Token::Await => {
        self.advance();
        return next_parse!(self, self.parse_await_expression());
      }
      _ => {
        return next_parse!(self, self.parse_update_expression());
      }
    }
  }

  fn parse_binary_operator_by_priority(
    &mut self,
    mut prev_ast: Expr,
    priority: OperatorPriority,
    prev_priority: OperatorPriority,
  ) -> ParseResult<Expr> {
    let token = self.cur();
    self.advance();

    let expr = next_parse!(self, self.parse_unary_expression())?;
    let left;
    let right;
    if prev_priority == OperatorPriority::None || prev_priority >= priority {
      left = prev_ast;
      right = expr;
      let mut ret = new_node_with_pos!(
        self,
        BinaryExpression,
        left.source_position().clone(),
        token,
        left,
        right
      );
      ret.set_end_position(right.source_position());
      return Ok(ret.into());
    }

    let mut maybe_bin_ast = prev_ast;
    while match prev_ast {
      Expr::BinaryExpression(_) => true,
      _ => false,
    } {
      match prev_ast {
        Expr::BinaryExpression(node) => {
          let rhs = node.rhs();
          if !match rhs {
            Expr::BinaryExpression(_) => true,
            _ => false,
          } {
            break;
          }
          maybe_bin_ast = node.into();
        }
        _ => break,
      }
    }
    let mut bin_expr = Node::<BinaryExpression>::try_from(maybe_bin_ast).unwrap();
    right = expr;
    left = bin_expr.rhs();
    let mut ret = new_node_with_pos!(
      self,
      BinaryExpression,
      left.source_position().clone(),
      token,
      left,
      right
    );
    ret.set_end_position(expr.source_position());
    bin_expr.set_rhs(ret);
    prev_ast.set_end_position(expr.source_position());
    return Ok(prev_ast);
  }

  fn parse_binary_expression(&mut self) -> ParseResult<Expr> {
    let mut last = next_parse!(self, self.parse_unary_expression())?;
    let mut last_op = OperatorPriority::None;
    loop {
      match self.cur() {
        Token::OpLogicalOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::LogicalOr, last_op)
          )?;
          last_op = OperatorPriority::LogicalOr;
        }
        Token::OpLogicalAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::LogicalAnd, last_op)
          )?;
          last_op = OperatorPriority::LogicalAnd;
        }
        Token::OpOr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::BitwiseOr, last_op)
          )?;
          last_op = OperatorPriority::BitwiseOr;
        }
        Token::OpXor => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::BitwiseXor, last_op)
          )?;
          last_op = OperatorPriority::BitwiseXor;
        }
        Token::OpAnd => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::BitwiseAnd, last_op)
          )?;
          last_op = OperatorPriority::BitwiseAnd;
        }
        Token::OpEq | Token::OpStrictEq | Token::OpNotEq | Token::OpStrictNotEq => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::Equality, last_op)
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
            self.parse_binary_operator_by_priority(last, OperatorPriority::Relational, last_op)
          )?;
          last_op = OperatorPriority::Relational;
        }
        Token::OpShl | Token::OpShr | Token::OpUShr => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::Shift, last_op)
          )?;
          last_op = OperatorPriority::Shift;
        }
        Token::OpPlus | Token::OpMinus => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::Additive, last_op)
          )?;
          last_op = OperatorPriority::Additive;
        }
        Token::OpMul | Token::OpDiv | Token::OpMod => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::Multiplicative, last_op)
          )?;
          last_op = OperatorPriority::Multiplicative;
        }
        Token::OpPow => {
          last = next_parse!(
            self,
            self.parse_binary_operator_by_priority(last, OperatorPriority::Exponentiation, last_op)
          )?;
          last_op = OperatorPriority::Exponentiation;
        }
        _ => return Ok(last),
      }
    }
  }

  fn parse_conditional_expression(&mut self) -> ParseResult<Expr> {
    let bin_expr = next_parse!(self, self.parse_binary_expression())?;
    if self.cur() == Token::Question {
      self.advance();
      let lhs = next_parse!(self, self.parse_assignment_expression())?;
      if self.cur() != Token::Colon {
        return Err("':' expected".to_string());
      }
      self.advance();
      let mut rhs = next_parse!(self, self.parse_assignment_expression())?;
      let rhs_source_pos = rhs.source_position().clone();
      rhs.set_end_position(&rhs_source_pos);
      let result = new_node_with_pos!(
        self,
        ConditionalExpression,
        bin_expr.source_position().clone(),
        bin_expr,
        lhs,
        rhs
      );
      return Ok(result.into());
    }
    return Ok(bin_expr);
  }
  fn parse_assignment_expression(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::Yield => {
        if !self.match_states(&[ParserState::InGeneratorFunction, ParserState::InAsyncGeneratorFunction]) {
          return Err("yield only allowed in generator or async generator.".to_string());
        }
        return next_parse!(self, self.parse_yield_expression());
      }
      Token::New => {
        return next_parse!(self, self.parse_assignment_expression_lhs());
      }
      _ => {
        if self.peek() == Token::ArrowFunctionGlyph {
          return next_parse!(self, self.parse_arrow_function());
        }
        if self.is_value_match_with("async") {
          self.record();
          self.advance();
          let next = self.peek();
          self.restore();
          if next == Token::ArrowFunctionGlyph {
            return self.parse_arrow_function();
          }
        }
        let expr = next_parse!(self, self.parse_conditional_expression())?;
        if !expr.is_valid_lhs() {
          return Err("Invalid left-hand-side-expression".to_string());
        }

        if self.cur().is_assignment_operator() {
          let op = self.cur();
          self.advance();
          let assignment = next_parse!(self, self.parse_assignment_expression())?;
          let end_pos = assignment.source_position().clone();
          let mut binary_expr = new_node_with_pos!(
            self,
            BinaryExpression,
            expr.source_position().clone(),
            op,
            expr,
            assignment
          );
          binary_expr.set_end_position(&end_pos);
          return Ok(binary_expr.into());
        }
        return Ok(expr);
      }
    }
  }
  fn parse_assignment_expression_lhs(&mut self) -> ParseResult<Expr> {
    let lhs = next_parse!(self, self.parse_left_hand_side_expression())?;
    if self.cur().is_assignment_operator() {
      let op = self.cur();
      self.advance();
      let rhs = next_parse!(self, self.parse_assignment_expression())?;
      let binary_expr = new_node!(self, BinaryExpression, op, lhs, rhs);
      return Ok(binary_expr.into());
    }
    return Ok(lhs);
  }
  fn parse_assignment_pattern(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_object_assignment_pattern(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_array_assignment_pattern(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_property_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_element_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_elision_element(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_property(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_element(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_assignment_rest_element(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_destructuring_assignment_target(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_expression(&mut self) -> ParseResult<Expr> {
    let expr = next_parse!(self, self.parse_assignment_expression())?;
    if self.cur() == Token::Comma {
      self.advance();
      let mut exprs = new_node!(self, Expressions);
      exprs.push(expr.into());
      while self.cur() == Token::Comma {
        let assignment_expr = next_parse!(self, self.parse_assignment_expression())?;
        exprs.push(assignment_expr);
      }
      return Ok(exprs.into());
    }

    return Ok(expr);
  }
  fn parse_statement(&mut self) -> ParseResult<Stmt> {
    use Token::*;
    match self.cur() {
      Terminate => {
        self.advance();
        return self.parse_statement();
      }
      LeftBrace => {
        return self.parse_block_statement();
      }
      Var => {
        return self.parse_variable_statement();
      }
      If => {
        return self.parse_if_statement();
      }
      Break => {
        return self.parse_break_statement();
      }
      Return => {
        return self.parse_return_statement();
      }
      With => {
        return self.parse_with_statement();
      }
      Throw => {
        return self.parse_throw_statement();
      }
      Try => {
        return self.parse_try_statement();
      }
      Debugger => {
        return self.parse_debugger_statement();
      }
      _ => {
        if self.peek() == Colon {
          return self.parse_labelled_statement();
        }
        return self.parse_expression_statement();
      }
    }
  }
  fn parse_declaration(&mut self) -> ParseResult<Stmt> {
    let mut is_async = false;
    match self.cur() {
      Token::Identifier => {
        if self.is_value_match_with("let") {
          return self.parse_lexical_declaration();
        }
        is_async = true;
        return next_parse!(self, self.parse_function_declaration(is_async));
      }
      Token::Function => {
        return next_parse!(self, self.parse_function_declaration(is_async));
      }
      _ => unreachable!(),
    };
  }
  fn parse_hoistable_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_breakable_statement(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_block_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_block(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_statement_list<T>(&mut self, condition: fn(Token) -> bool) -> ParseResult<Stmt> {
    let mut statements = new_node!(self, Statements);
    while self.has_more() && condition(self.cur()) {
      let stmt = next_parse!(self, self.parse_statement_list_item())?;
      statements.push(stmt);
    }

    if statements.len() > 0 {
      let last = statements.last().unwrap();
      let sp = last.source_position().clone();
      statements.set_end_position(&sp);
    }

    return Ok(statements.into());
  }
  fn parse_statement_list_item(&mut self) -> ParseResult<Stmt> {
    match self.cur() {
      Token::Class | Token::Const | Token::Function => {
        return next_parse!(self, self.parse_declaration());
      }
      _ => {
        if self.cur() == Token::Identifier {
          let v = self.value();
          if (self.is_value_match_with("async") && self.peek() == Token::Function && !self.has_line_break_before())
            || self.is_value_match_with("let")
          {
            return self.parse_declaration();
          }
        }
      }
    };

    return self.parse_statement();
  }
  fn parse_lexical_declaration(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_lexical_binding(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_variable_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_variable_declaration_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_variable_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_binding_pattern(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }

  fn parse_binding_element(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBracket | Token::LeftBrace => {
        return next_parse!(self, self.parse_binding_pattern());
      }
      _ => return next_parse!(self, self.parse_single_name_binding(ParserConstraints::Initializer)),
    };
  }

  fn parse_single_name_binding(&mut self, constraints: ParserConstraints) -> ParseResult<Expr> {
    let mut identifier: Expr = self.empty.into();
    match self.cur() {
      Token::Identifier | Token::Yield | Token::Await => {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        identifier = new_node!(self, Literal, Token::Identifier, val).into();
      }
      _ => return Err("Identifier expected".to_string()),
    };

    debug_assert!(match identifier {
      Expr::Empty(_) => false,
      _ => true,
    });
    if constraints.is_initializer_allowed() && self.cur() == Token::OpAssign {
      self.advance();
      let expr = next_parse!(self, self.parse_assignment_expression())?;
      return Ok(new_node!(self, BinaryExpression, Token::OpAssign, identifier, expr).into());
    }
    return Ok(identifier);
  }

  fn parse_binding_rest_element(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }

  fn parse_expression_statement(&mut self) -> ParseResult<Stmt> {
    let expr = next_parse!(self, self.parse_expression())?;
    let mut stmt = new_node!(self, Statement, expr);
    let sp = expr.source_position().clone();
    stmt.set_start_position(&sp);
    stmt.set_end_position(&sp);
    return next_parse!(self, self.parse_terminator(stmt.into()));
  }
  fn parse_if_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_iteration_statement(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_for_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_for_binding(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_continue_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_break_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_return_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_with_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_switch_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_case_block(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_case_clauses(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_case_clause(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_default_caluse(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_labelled_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_labelled_item(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_throw_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_try_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_catch(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_finally(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_catch_parameter(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_debugger_statement(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_function_declaration(&mut self, is_async: bool) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_function_expression(&mut self, is_async: bool) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_formal_parameters(&mut self) -> ParseResult<Expr> {
    if self.cur() == Token::Spread {
      return next_parse!(self, self.parse_function_rest_parameter());
    }
    let p = next_parse!(self, self.parse_formal_parameter_list())?;
    let mut exprs = Node::<Expressions>::try_from(p).unwrap();
    if self.cur() == Token::Comma {
      self.advance();
      if self.cur() == Token::Spread {
        let rp = next_parse!(self, self.parse_function_rest_parameter())?;
        exprs.push(rp);
      }
    }
    return Ok(exprs.into());
  }
  fn parse_formal_parameter_list(&mut self) -> ParseResult<Expr> {
    let mut exprs = new_node!(self, Expressions);
    loop {
      match self.cur() {
        Token::Identifier | Token::LeftBrace | Token::LeftBracket => {
          let be = next_parse!(self, self.parse_binding_element())?;
          exprs.push(be);
        }
        Token::Comma => {
          self.advance();
        }
        _ => return Ok(exprs.into()),
      };
    }
    unreachable!();
  }
  fn parse_function_rest_parameter(&mut self) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Spread);
    self.advance();
    match self.cur() {
      Token::LeftBrace | Token::LeftBracket => {
        return next_parse!(self, self.parse_binding_pattern());
      }
      _ => return next_parse!(self, self.parse_single_name_binding(ParserConstraints::Initializer)),
    }
  }
  fn parse_function_body(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_arrow_function(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_arrow_parameter(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_async_concise_body(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_async_arrow_head(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_method_definition(&mut self) -> ParseResult<Expr> {
    let is_getter = self.is_value_match_with("get");
    let is_setter = self.is_value_match_with("set");
    if (is_getter || is_setter) && self.cur().one_of(&[Token::Identifier, Token::LeftBracket]) {
      self.advance();
    }

    match self.cur() {
      Token::OpMul => {
        return next_parse!(self, self.parse_generator_method());
      }
      Token::NumericLiteral
      | Token::ImplicitOctalLiteral
      | Token::StringLiteral
      | Token::LeftBracket
      | Token::Identifier => {
        let mut maybe_name = Err("".to_string());
        let mut formal_parameters = Err("".to_string());

        if self.cur() == Token::Identifier && self.is_value_match_with("async") {
          return next_parse!(self, self.parse_async_method());
        }
        maybe_name = next_parse!(self, self.parse_property_name());

        expect!(self, self.cur(), Token::LeftParen);
        if is_getter {
          if self.cur() != Token::RightParen {
            return Err("Getter must not have any formal parameters".to_string());
          }
          self.advance();
          formal_parameters = Ok(new_node!(self, Expressions).into());
        } else if is_setter {
          if self.cur() == Token::RightParen {
            return Err("Setter must have exactly one formal parameter".to_string());
          }
          formal_parameters = next_parse!(self, self.parse_property_set_parameter_list());
          if self.cur() != Token::RightParen {
            return Err("Setter must have exactly one formal parameter".to_string());
          }
          self.advance();
        } else {
          formal_parameters = next_parse!(self, self.parse_formal_parameters());
        }

        expect!(self, self.cur(), Token::LeftBrace);
        let name = maybe_name?;
        let formal_params = formal_parameters?;
        let start = self.scanner.source_index();
        let _ = next_parse!(self, self.parse_function_body())?;
        let end = self.scanner.source_index();
        expect!(self, self.cur(), Token::RightBrace);
        let accessor_type = if is_getter {
          FunctionAccessor::Getter
        } else if is_setter {
          FunctionAccessor::Setter
        } else {
          FunctionAccessor::None
        };
        let function = new_node!(
          self,
          FunctionExpression,
          Some(Node::<Literal>::try_from(name).unwrap()),
          FunctionType::Scoped,
          accessor_type,
          Node::<Expressions>::try_from(formal_params).unwrap(),
          start,
          end
        );
        return Ok(new_node!(self, ObjectPropertyExpression, name, Some(function.into()), None).into());
      }
      _ => unreachable!(),
    };
  }
  fn parse_property_set_parameter_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_generator_method(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_generator_declaration(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_generator_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_generator_body(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_yield_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_async_method(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_async_function_declaration(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_async_function_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_async_function_body(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_await_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_tail(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_heritage(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_body(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_element_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_class_element(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_module_item(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_import_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_name_space_import(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_named_import(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_export_declaration(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_export_clause(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
  fn parse_named_list(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }
}
