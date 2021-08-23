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
  (@notadvance $self:tt, $n:expr, $token:expr) => {
    if $n != $token {
      return Err(format!("'{}' expected", $token.symbol()));
    }
  };
  ($self:tt, $n:expr, $token:expr) => {
    expect!(@notadvance $self, $n, $token);
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
  next_parse: Vec<&'static str>,
  position: SourcePosition,
  has_error: bool,
  indent: String,
  buffer: String,
}

#[cfg(feature = "print_ast")]
impl ParserDebugger {
  fn new() -> Self {
    ParserDebugger {
      next_parse: Vec::<&'static str>::new(),
      position: SourcePosition::new(),
      has_error: false,
      indent: "".to_string(),
      buffer: String::new(),
    }
  }
  fn enter(&mut self, next_parse: &'static str, current_token: Token, position: &SourcePosition, has_error: bool) {
    self.next_parse.push(next_parse);
    self.position = *position;
    self.has_error = has_error;

    self.buffer = format!(
      "{}\n{}Enter {}: CurrentToken = {} {:?}",
      self.buffer, self.indent, next_parse, current_token, self.position
    );
    #[cfg(feature = "print_ast_when_called")]
    println!(
      "{}Enter {}: CurrentToken = {} {:?}",
      self.indent, next_parse, current_token, self.position
    );
    self.indent = format!("  {}", self.indent);
  }

  fn leave<T>(&mut self, token: Token, result: &ParseResult<T>) {
    self.indent = self.indent.chars().take(self.indent.len() - 2).collect::<String>();
    self.buffer = format!(
      "{}\n{}Exit {} {} CurrentToken = {:?}{} {:?}",
      self.buffer,
      self.indent,
      *self.next_parse.last().unwrap(),
      if result.is_ok() { "Success" } else { "Failure" },
      token,
      if self.has_error { "[Error]" } else { "" },
      self.position
    );
    #[cfg(feature = "print_ast_when_called")]
    println!(
      "{}Exit {} {} CurrentToken = {:?}{} {:?}",
      self.indent,
      *self.next_parse.last().unwrap(),
      if result.is_ok() { "Success" } else { "Failure" },
      token,
      if self.has_error { "[Error]" } else { "" },
      self.position
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
    $self
      .debugger
      .enter(stringify!($call), cur, &source_position, has_pending_error);
    let result = $call;
    let cur = $self.cur();
    $self.debugger.leave(cur, &result);
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
  result: Result<Ast, String>,
  source: FixedU16CodePointArray,
  scanner_record: Option<ScannerRecord>,
  error_reporter: Exotic<ErrorReporter>,
  is_strict_mode: bool,
  debugger: ParserDebugger,
  empty: Node<Empty>,
}

macro_rules! new_node {
  ($self:tt, $name:tt, $($args:expr),+$(,)?) => {{
    let mut node = $name::new(&mut $self.region, $($args),*);
    let p = $self.source_position().clone();
    node.set_source_position(&p);
    node
  }};
  ($self:tt, $name:tt) => {{
    let mut node = $name::new(&mut $self.region);
    let p = $self.source_position().clone();
    node.set_source_position(&p);
    node
  }}
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
      result: Err("Unparsed".to_string()),
      source: FixedU16CodePointArray::from_utf8(context, source),
      scanner_record: None,
      error_reporter: Exotic::new(std::ptr::null_mut()),
      is_strict_mode: false,
      debugger: ParserDebugger::new(),
      empty,
    };
    parser.error_reporter = parser.region.alloc(ErrorReporter::new());
    parser.parser_state = parser.region.alloc(ParserStateStack::new());
    parser.scanner = parser
      .region
      .alloc(Scanner::new(parser.source, parser.parser_state, parser.error_reporter));
    return parser;
  }

  pub fn parse(&mut self, parser_type: ParserType) -> Result<Ast, String> {
    self.scanner.next();
    self.parser_type = parser_type;
    self.parse_program();
    if self.result.is_err() {
      let m = self.result.as_ref().unwrap_err().clone();
      report_syntax_error!(noreturn self, m);
    }
    return self.result.clone();
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
    return value.len() == bytes.len();
  }

  fn value_to_utf8(value: &Vec<u16>) -> String {
    return decode_utf16(value.iter().cloned())
      .map(|r| r.unwrap_or('#'))
      .collect::<String>();
  }

  #[cfg(feature = "print_ast")]
  pub fn print_stack_trace(&self) {
    self.debugger.print_stack_trace();
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
      self.result = Err(self.error_reporter.last_error().unwrap().clone());
      return;
    }
    self.result = if self.parser_type == ParserType::Script {
      self.parse_script()
    } else {
      self.parse_module()
    }
  }

  fn parse_script(&mut self) -> ParseResult<Ast> {
    return next_parse!(self, self.parse_statement_list(|_| true).map(|a| Into::<Ast>::into(a)));
  }

  fn parse_module(&mut self) -> ParseResult<Ast> {
    return next_parse!(self, self.parse_module_body().map(|a| Into::<Ast>::into(a)));
  }

  fn parse_module_body(&mut self) -> ParseResult<Stmt> {
    let mut stmts = new_node!(self, Statements);
    while self.has_more() {
      let item = next_parse!(self, self.parse_module_item())?;
      stmts.push(item);
    }
    return Ok(stmts.into());
  }

  fn parse_terminator<T>(&mut self, expr: T) -> ParseResult<T> {
    if self.cur() == Token::Terminate {
      self.advance();
      return Ok(expr);
    } else if self.cur() != Token::End && self.cur() != Token::RightBrace && !self.has_line_break_after() {
      return Err("';' expected".to_string());
    }

    return Ok(expr);
  }

  fn parse_identifier(&mut self) -> ParseResult<Expr> {
    expect!(@notadvance self, self.cur(), Token::Identifier);
    let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
    let mut node = new_node!(self, Literal, Token::Identifier, val);
    node.set_valid_lhs();
    return Ok(node.into());
  }

  fn parse_identifier_reference(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    let result = match self.cur() {
      Token::Identifier | Token::Yield | Token::Await => {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        let mut node = new_node_with_pos!(self, Literal, &start, Token::Identifier, val);
        node.set_valid_lhs();
        Ok(node.into())
      }
      _ => Err("Literal expected".to_string()),
    };
    self.advance();
    return result;
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
        return next_parse!(self, self.parse_object_literal(ParserConstraints::None));
      }
      Token::Function => {
        if self.peek() == Token::OpMul {
          return next_parse!(self, self.parse_generator_expression());
        }
        return next_parse!(self, self.parse_function_expression(false, false));
      }
      Token::Class => {
        return next_parse!(self, self.parse_class_expression());
      }
      Token::Identifier => {
        if self.is_value_match_with("async") && !self.has_line_break_after() && self.peek() == Token::Function {
          self.advance();
          return next_parse!(self, self.parse_function_expression(true, false));
        }
        return next_parse!(self, self.parse_identifier_reference());
      }
      Token::ImplicitOctalLiteral => {
        if self.is_strict_mode {
          return Err("Implicit octal literal not allowed in strict mode".to_string());
        }
        return next_parse!(self, self.parse_literal());
      }
      Token::NumericLiteral => {
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
        return next_parse!(
          self,
          self.parse_cover_parenthesized_expression_and_arrow_parameter_list()
        );
      }
      _ => {
        return Err("Unexpected token found".to_string());
      }
    }
  }

  fn parse_literal(&mut self) -> ParseResult<Expr> {
    let result = match self.cur() {
      Token::Null | Token::True | Token::False => {
        let t = self.cur();
        Ok(new_node!(self, Literal, t, LiteralValue::None).into())
      }
      Token::NumericLiteral | Token::ImplicitOctalLiteral => {
        let t = self.cur();
        let val = LiteralValue::Number(self.numeric_value());
        Ok(new_node!(self, Literal, t, val).into())
      }
      Token::StringLiteral => {
        let t = self.cur();
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        Ok(new_node!(self, Literal, t, val).into())
      }
      _ => Err("Literal expected".to_string()),
    };
    self.advance();
    return result;
  }

  fn parse_regular_expression(&mut self) -> ParseResult<Expr> {
    return Err("".to_string());
  }

  fn parse_array_literal(&mut self, constraints: ParserConstraints) -> ParseResult<Expr> {
    expect!(@notadvance self, self.cur(), Token::LeftBracket);
    let mut array = new_node!(self, StructuralLiteral, StructuralLiteralType::Array);
    array.set_source_position(self.source_position());
    array.set_valid_lhs();
    self.advance();
    let mut is_spread_seen = false;
    while !self.cur().one_of(&[Token::RightBracket, Token::Invalid, Token::End]) {
      if self.cur() == Token::Comma {
        let start_pos = self.source_position().clone();
        self.advance();
        match self.cur() {
          Token::Comma | Token::RightBracket => {
            array.push(new_node_with_pos!(self, Empty, start_pos).into());
          }
          _ => {
            while self.cur() == Token::Comma {
              let pos = self.source_position().clone();
              array.push(new_node_with_pos!(self, Empty, pos).into());
              self.advance();
            }
          }
        }
      } else {
        if is_spread_seen {
          return Err("Spread must be last element".to_string());
        }
        if self.cur() == Token::Spread {
          is_spread_seen = true;
          let a = next_parse!(self, self.parse_spread_element())?;
          array.push(a);
          array.set_spread();
        } else {
          let expr = next_parse!(self, self.parse_assignment_expression())?;
          match expr {
            Expr::Literal(lit) => {
              if !lit.is_identifier() {
                array.unset_valid_lhs();
              }
            }
            Expr::StructuralLiteral(lit) => {
              if !lit.is_valid_lhs() {
                array.unset_valid_lhs();
              }
            }
            _ => {
              array.unset_valid_lhs();
            }
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
    let binding_tokens = [
      Token::Identifier,
      Token::LeftBracket,
      Token::StringLiteral,
      Token::NumericLiteral,
      Token::ImplicitOctalLiteral,
      Token::OpMul,
    ];

    if self.cur().one_of(&binding_tokens) {
      if self.peek() == Token::LeftParen {
        return next_parse!(self, self.parse_method_definition());
      }

      if self.is_value_match_with("async") && self.peek().one_of(&binding_tokens) && !self.has_line_break_after() {
        let state = if self.peek() == Token::OpMul {
          ParserState::InAsyncGeneratorFunction
        } else {
          ParserState::InAsyncFunction
        };
        let mut this = scoped!(self, |this| {
          this.parser_state.pop_state(state);
        });
        this.parser_state.push_state(state);
        return next_parse!(this, this.parse_method_definition());
      }

      if self.cur() == Token::OpMul && self.peek().one_of(&binding_tokens) {
        let mut this = scoped!(self, |this| {
          this.parser_state.pop_state(ParserState::InGeneratorFunction);
        });
        this.parser_state.push_state(ParserState::InGeneratorFunction);
        return next_parse!(this, this.parse_method_definition());
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
        self.advance();
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
    if self.cur() != Token::Colon {
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

  fn parse_property_name(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::Identifier => {
        let i = next_parse!(self, self.parse_identifier());
        self.advance();
        return i;
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

  fn parse_template_literal(&mut self) -> ParseResult<Expr> {
    let mut template_literal = new_node_with_pos!(self, TemplateLiteral, self.source_position().clone());
    expect!(self, self.cur(), Token::BackQuote);
    while self.cur() != Token::Template {
      match self.cur() {
        Token::StringLiteral => {
          let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
          template_literal
            .push(new_node_with_pos!(self, Literal, self.source_position().clone(), Token::StringLiteral, val).into());
          self.advance();
        }
        Token::TemplateSubstitution => {
          self.parser_state.push_state(ParserState::InTemplateInterpolation);
          self.advance();
          let expr = next_parse!(self, self.parse_expression())?;
          template_literal.push(expr.into());
          self.parser_state.pop_state(ParserState::InTemplateInterpolation);
          expect!(self, self.cur(), Token::RightBrace);
        }
        _ => {
          self.advance();
        }
      };
    }

    template_literal.set_end_position(self.source_position());
    self.parser_state.pop_state(ParserState::InTemplateLiteral);
    self.advance();
    return Ok(template_literal.into());
  }

  fn parse_cover_parenthesized_expression_and_arrow_parameter_list(&mut self) -> ParseResult<Expr> {
    let mut start = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    let mut exprs = new_node_with_pos!(self, Expressions, start);
    if self.cur() == Token::RightParen {
      self.advance();
      return Ok(exprs.into());
    }

    loop {
      if self.cur() == Token::Spread {
        start = self.source_position().clone();
        self.advance();
        let e;
        if self.cur().one_of(&[Token::LeftBracket, Token::LeftBrace]) {
          e = next_parse!(self, self.parse_binding_pattern())?;
        } else {
          e = next_parse!(self, self.parse_single_name_binding(ParserConstraints::None))?;
        }
        let u = new_node_with_positions!(
          self,
          UnaryExpression,
          start,
          e.source_position().clone(),
          UnaryExpressionOperandPosition::Pre,
          Token::Spread,
          e
        );
        exprs.push(u.into());
      } else {
        let n = next_parse!(self, self.parse_expression())?;
        exprs.push(n.into());
      }

      if self.cur() != Token::Comma {
        break;
      } else {
        self.advance();
        if self.peek() == Token::RightParen {
          break;
        }
      }
    }

    exprs.set_end_position(self.source_position());
    expect!(self, self.cur(), Token::RightParen);
    return Ok(exprs.into());
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
      if self.cur() != Token::LeftParen {
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
        Some(m),
        Some(args)
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
            Some(current),
            Some(a.into())
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
          let mut expr = new_node_with_pos!(
            self,
            PropertyAccessExpression,
            source_position,
            PropertyAccessType::Dot,
            receiver_type,
            Some(current),
            Some(ident)
          );
          expr.set_end_position(ident.source_position());
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
            Some(current),
            Some(tmpl)
          )
          .into();
          current.set_end_position(tmpl.source_position());
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

  fn parse_super_call(&mut self) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Super);
    let args = next_parse!(self, self.parse_arguments())?;
    return Ok(new_node!(self, CallExpression, CallReceiverType::Super, None, Some(args)).into());
  }

  fn parse_import_call(&mut self) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Import);
    expect!(self, self.cur(), Token::LeftParen);
    let expr = next_parse!(self, self.parse_assignment_expression())?;
    expect!(self, self.cur(), Token::RightParen);
    return Ok(new_node!(self, CallExpression, CallReceiverType::Super, None, Some(expr)).into());
  }

  fn parse_arguments(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    let mut exprs = new_node_with_pos!(self, Expressions, start);
    exprs.mark_as_arguments();
    loop {
      if self.cur() == Token::Spread {
        self.advance();
        let expr = next_parse!(self, self.parse_assignment_expression())?;
        let u = new_node!(
          self,
          UnaryExpression,
          UnaryExpressionOperandPosition::Pre,
          Token::Spread,
          expr
        );
        exprs.push(u.into());
      } else if self.cur() == Token::RightParen {
        break;
      } else {
        let expr = next_parse!(self, self.parse_assignment_expression())?;
        exprs.push(expr);
      }

      if self.cur() == Token::Comma {
        if self.peek() == Token::LeftParen {
          return Err("Extra ',' found".to_string());
        }
        self.advance();
      } else {
        break;
      }
    }

    let end = self.source_position().clone();
    expect!(self, self.cur(), Token::RightParen);
    exprs.set_end_position(&end);
    return Ok(exprs.into());
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
          return next_parse!(self, self.parse_member_expression().map(set_lhs));
        }
        return next_parse!(self, self.parse_member_expression());
      }
      Token::Super => {
        if self.peek() == Token::LeftParen {
          return next_parse!(self, self.parse_super_call());
        }
        return next_parse!(self, self.parse_member_expression());
      }
      Token::Import => {
        return next_parse!(self, self.parse_import_call());
      }
      _ => {
        let expr = next_parse!(self, self.parse_member_expression())?;
        if self.cur() == Token::LeftParen {
          let a = next_parse!(self, self.parse_arguments())?;
          let pos = self.source_position().clone();
          let n = new_node_with_positions!(
            self,
            CallExpression,
            expr.source_position().clone(),
            pos,
            CallReceiverType::Expr,
            Some(expr),
            Some(a)
          );
          return next_parse!(
            self,
            self.parse_post_member_expression(
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
        let result = match self.cur() {
          Token::OpIncrement | Token::OpDecrement => {
            let token = self.cur();
            let sp = self.source_position().clone();
            self.advance();
            Ok(
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
            )
          }
          _ => Ok(n),
        };
        return result;
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
        if !self
          .parser_state
          .match_states(&[ParserState::InAsyncFunction, ParserState::InAsyncGeneratorFunction])
        {
          return Err("await is not allowed outside of async function.".to_string());
        }
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
      let rhs = next_parse!(self, self.parse_assignment_expression())?;
      let rhs_source_pos = rhs.source_position().clone();
      let mut result = new_node_with_pos!(
        self,
        ConditionalExpression,
        bin_expr.source_position().clone(),
        bin_expr,
        lhs,
        rhs
      );
      result.set_end_position(&rhs_source_pos);
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
        if self.cur().one_of(&[Token::Identifier, Token::Await, Token::Yield])
          && self.peek() == Token::ArrowFunctionGlyph
        {
          return next_parse!(self, self.parse_arrow_function(false));
        }

        let expr = next_parse!(self, self.parse_conditional_expression())?;
        if self.cur() == Token::ArrowFunctionGlyph {
          let pos = self.source_position().clone();
          self.advance();
          match expr {
            Expr::CallExpression(call_node) => {
              if match call_node.receiver() {
                CallReceiverType::New | CallReceiverType::Super => true,
                _ => false,
              } {
                return Err("Unexpected token found".to_string());
              }
              if let Some(callee) = call_node.callee() {
                match callee {
                  Expr::Literal(literal_node) => {
                    if literal_node.literal_type() == Token::Identifier {
                      match literal_node.value() {
                        LiteralValue::String(array) => {
                          if array.to_utf8().eq("async") {
                            if let Some(args) = call_node.parameters() {
                              return next_parse!(self, self.parse_concise_body(true, args));
                            } else {
                              let mut exprs = new_node_with_pos!(self, Expressions, pos);
                              exprs.push(literal_node.into());
                              exprs.mark_as_arguments();
                              return next_parse!(self, self.parse_concise_body(false, exprs.into()));
                            };
                          }
                        }
                        _ => {
                          return Err("Identifier expected".to_string());
                        }
                      }
                    }
                  }
                  _ => {
                    return Err("Identifier expected".to_string());
                  }
                }
              }
            }
            Expr::Expressions(exprs) => {
              return next_parse!(self, self.parse_concise_body(false, exprs.into()));
            }
            _ => {
              return Err("Unexpected token found".to_string());
            }
          };
        }

        if self.cur().is_assignment_operator() {
          if !expr.is_valid_lhs() {
            return Err("Invalid left-hand-side-expression".to_string());
          }
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
        return next_parse!(self, self.parse_statement());
      }
      LeftBrace => {
        return next_parse!(self, self.parse_block_statement());
      }
      Var => {
        return next_parse!(self, self.parse_variable_statement());
      }
      If => {
        return next_parse!(self, self.parse_if_statement());
      }
      Break => {
        return next_parse!(self, self.parse_break_statement());
      }
      Return => {
        return next_parse!(self, self.parse_return_statement());
      }
      With => {
        return next_parse!(self, self.parse_with_statement());
      }
      Throw => {
        return next_parse!(self, self.parse_throw_statement());
      }
      Try => {
        return next_parse!(self, self.parse_try_statement());
      }
      Debugger => {
        return next_parse!(self, self.parse_debugger_statement());
      }
      _ => {
        if self.peek() == Colon {
          return next_parse!(self, self.parse_labelled_statement());
        }
        return next_parse!(self, self.parse_expression_statement());
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
        return next_parse!(self, self.parse_function_declaration(is_async, false));
      }
      Token::Function => {
        return next_parse!(self, self.parse_function_declaration(is_async, false));
      }
      _ => unreachable!(),
    };
  }
  fn parse_hoistable_declaration(&mut self) -> ParseResult<Stmt> {
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
  fn parse_statement_list<T: Fn(Token) -> bool>(&mut self, condition: T) -> ParseResult<Stmt> {
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
          if (self.is_value_match_with("async") && self.peek() == Token::Function && !self.has_line_break_after())
            || self.is_value_match_with("let")
          {
            return next_parse!(self, self.parse_declaration());
          }
        }
      }
    };

    return next_parse!(self, self.parse_statement());
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
  fn parse_variable_declaration_list(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_variable_declaration(&mut self) -> ParseResult<Stmt> {
    return Err("".to_string());
  }
  fn parse_binding_pattern(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftBrace => {
        return next_parse!(self, self.parse_object_literal(ParserConstraints::BindingPattern));
      }
      _ => {
        debug_assert!(self.cur() == Token::LeftBracket);
        return next_parse!(self, self.parse_array_literal(ParserConstraints::BindingPattern));
      }
    }
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
  fn parse_for_declaration(&mut self) -> ParseResult<Stmt> {
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
  fn parse_function_declaration(&mut self, is_async: bool, is_default: bool) -> ParseResult<Stmt> {
    let start = self.source_position().clone();
    if is_async {
      self.advance();
    }

    expect!(self, self.cur(), Token::Function);
    let mut identifier: Option<Node<Literal>> = None;

    if self.cur() == Token::Identifier {
      identifier = Some(Node::<Literal>::try_from(next_parse!(self, self.parse_identifier())?).unwrap());
      self.advance();
    } else if !is_default {
      return Err("Identifier expected".to_string());
    }

    let formal_parameter_position = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftParen);
    let mut params = next_parse!(self, self.parse_formal_parameters())?;
    params.set_start_position(&formal_parameter_position);
    params.set_end_position(self.source_position());
    expect!(self, self.cur(), Token::RightParen);

    let body_start_position = self.source_position().clone();
    expect!(self, self.cur(), Token::LeftBrace);
    let mut body = next_parse!(self, self.parse_function_body())?;
    expect!(self, self.cur(), Token::RightBrace);
    body.set_start_position(&body_start_position);
    body.set_end_position(self.source_position());

    let function_type = if self
      .parser_state
      .is_in_states(&[ParserState::InGeneratorFunction, ParserState::InAsyncGeneratorFunction])
    {
      FunctionType::Generator
    } else {
      FunctionType::Scoped
    };

    let mut function = new_node!(
      self,
      FunctionExpression,
      is_async,
      identifier,
      function_type,
      FunctionAccessor::None,
      Node::<Expressions>::try_from(params).unwrap(),
      body.into()
    );
    function.set_start_position(&start);
    function.set_end_position(self.prev_source_position());
    let mut stmt = new_node!(self, Statement, function.into());
    stmt.set_start_position(&start);
    stmt.set_end_position(self.prev_source_position());
    return Ok(stmt.into());
  }
  fn parse_function_expression(&mut self, is_async: bool, is_default: bool) -> ParseResult<Expr> {
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
  fn parse_function_body(&mut self) -> ParseResult<Stmt> {
    return next_parse!(self, self.parse_statement_list(|t| t != Token::RightBrace));
  }

  fn parse_arrow_function(&mut self, is_async: bool) -> ParseResult<Expr> {
    let p = next_parse!(self, self.parse_arrow_parameter())?;
    if self.cur() != Token::Terminate || self.has_line_break_before() {
      expect!(self, self.cur(), Token::ArrowFunctionGlyph);
      return next_parse!(self, self.parse_concise_body(is_async, p));
    }
    return Ok(p);
  }

  fn parse_arrow_parameter(&mut self) -> ParseResult<Expr> {
    match self.cur() {
      Token::LeftParen => {
        return next_parse!(
          self,
          self.parse_cover_parenthesized_expression_and_arrow_parameter_list()
        )
      }
      _ => {
        let mut exprs = new_node!(self, Expressions);
        let binding = next_parse!(self, self.parse_single_name_binding(ParserConstraints::None))?;
        exprs.push(binding);
        return Ok(binding.into());
      }
    }
  }

  fn parse_concise_body(&mut self, is_async: bool, args: Expr) -> ParseResult<Expr> {
    let has_brace = self.cur() == Token::LeftBrace;
    let body = if has_brace {
      self.advance();
      next_parse!(self, self.parse_statement_list(|t| t == Token::RightBrace)).map(|t| t.into())
    } else {
      next_parse!(self, self.parse_assignment_expression()).map(|t| t.into())
    }?;
    if has_brace {
      self.advance();
    }
    return Ok(
      new_node!(
        self,
        FunctionExpression,
        is_async,
        None,
        FunctionType::NonScoped,
        FunctionAccessor::None,
        Node::<Expressions>::try_from(args).unwrap(),
        body
      )
      .into(),
    );
  }
  fn parse_method_definition(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    let is_getter = self.is_value_match_with("get");
    let is_setter = self.is_value_match_with("set");
    let is_generator = self.cur() == Token::OpMul;
    if ((is_getter || is_setter) && self.cur().one_of(&[Token::Identifier, Token::LeftBracket])) || is_generator {
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
        let mut is_async = self.cur() == Token::Identifier && self.is_value_match_with("async");
        if is_async {
          self.advance();
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
          expect!(self, self.cur(), Token::RightParen);
        }

        expect!(self, self.cur(), Token::LeftBrace);
        let name = maybe_name?;
        let formal_params = formal_parameters?;
        let body = next_parse!(self, self.parse_function_body())?;
        expect!(self, self.cur(), Token::RightBrace);
        let accessor_type = if is_getter {
          FunctionAccessor::Getter
        } else if is_setter {
          FunctionAccessor::Setter
        } else {
          FunctionAccessor::None
        };
        let function = new_node_with_positions!(
          self,
          FunctionExpression,
          start,
          self.source_position().clone(),
          is_async,
          Some(Node::<Literal>::try_from(name).unwrap()),
          if is_generator {
            FunctionType::Generator
          } else {
            FunctionType::Scoped
          },
          accessor_type,
          Node::<Expressions>::try_from(formal_params).unwrap(),
          body.into()
        );
        return Ok(
          new_node_with_positions!(
            self,
            ObjectPropertyExpression,
            start,
            self.source_position().clone(),
            name,
            Some(function.into()),
            None
          )
          .into(),
        );
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
    let pos = self.source_position().clone();
    expect!(self, self.cur(), Token::Yield);
    let has_termination = self.cur() == Token::Terminate;
    println!("aAAAaaaaaaaaa = {} {:?}", self.has_line_break_before(), self.cur());
    if has_termination || self.has_line_break_before() {
      if has_termination {
        self.advance();
      }
      return Ok(
        new_node_with_pos!(
          self,
          UnaryExpression,
          pos,
          UnaryExpressionOperandPosition::Pre,
          Token::Yield,
          self.empty.into()
        )
        .into(),
      );
    }

    if self.cur() == Token::OpMul {
      self.advance();
      let expr = next_parse!(self, self.parse_assignment_expression())?;
      return Ok(
        new_node_with_positions!(
          self,
          UnaryExpression,
          pos,
          expr.source_position().clone(),
          UnaryExpressionOperandPosition::Pre,
          Token::YieldAggregator,
          expr
        )
        .into(),
      );
    }

    let expr = next_parse!(self, self.parse_assignment_expression())?;
    return Ok(
      new_node_with_positions!(
        self,
        UnaryExpression,
        pos,
        expr.source_position().clone(),
        UnaryExpressionOperandPosition::Pre,
        Token::Yield,
        expr
      )
      .into(),
    );
  }
  fn parse_await_expression(&mut self) -> ParseResult<Expr> {
    let start = self.source_position().clone();
    expect!(self, self.cur(), Token::Await);
    let expr = next_parse!(self, self.parse_unary_expression())?;
    let mut unary = new_node_with_pos!(
      self,
      UnaryExpression,
      start,
      UnaryExpressionOperandPosition::Pre,
      Token::Await,
      expr
    );
    unary.set_end_position(expr.source_position());
    return Ok(unary.into());
  }
  fn parse_class_declaration(&mut self) -> ParseResult<Stmt> {
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
    match self.cur() {
      Token::Import => return next_parse!(self, self.parse_import_declaration()),
      Token::Export => return next_parse!(self, self.parse_export_declaration()),
      _ => return next_parse!(self, self.parse_statement_list_item()),
    }
  }
  fn parse_import_declaration(&mut self) -> ParseResult<Stmt> {
    debug_assert!(self.cur() == Token::Import);
    let mut start = self.source_position().clone();
    self.advance();
    if self.cur() == Token::StringLiteral {
      let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
      let t = self.cur();
      let str = new_node_with_pos!(self, Literal, start, t, val);
      return Ok(new_node_with_pos!(self, ImportDeclaration, start, None, str.into()).into());
    }

    let mut default_binding_node = None;
    start = self.source_position().clone();
    if self.cur() == Token::Identifier {
      let default_binding = next_parse!(self, self.parse_identifier())?;
      self.advance();
      default_binding_node = Some(default_binding);
    }
    let mut ib = new_node_with_pos!(self, ImportBinding, start, default_binding_node, None, None);
    match self.cur() {
      Token::LeftBrace => {
        ib.set_named_import_list(Some(next_parse!(self, self.parse_named_import())?));
      }
      Token::OpMul => {
        ib.set_namespace_import(Some(next_parse!(self, self.parse_name_space_import())?));
      }
      _ => return Err("Unexpected token".to_string()),
    }

    if self.cur() == Token::Identifier && self.is_value_match_with("from") {
      self.advance();
      if self.cur() == Token::StringLiteral {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        let t = self.cur();
        let str = new_node!(self, Literal, t, val);
        if self.cur() == Token::Terminate {
          self.advance();
        } else if !self.has_line_break_before() {
          return Err("';' expected".to_string());
        }
        return Ok(new_node!(self, ImportDeclaration, Some(ib.into()), str.into()).into());
      }
    }

    return Err("Module specifier expected".to_string());
  }

  fn parse_name_space_import(&mut self) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::Export);
    if self.cur() == Token::Identifier && self.is_value_match_with("as") {
      return Ok(new_node!(self, ImportSpecifier, true, None, None).into());
    }
    return Err("'as' expected".to_string());
  }

  fn parse_named_import(&mut self) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list());
  }

  fn parse_export_declaration(&mut self) -> ParseResult<Stmt> {
    expect!(self, self.cur(), Token::Export);
    let mut export_type = ExportDeclarationType::NamespaceExport;
    let mut export_clause: Option<Ast> = None;
    if self.cur() == Token::Default {
      export_type = ExportDeclarationType::DefaultExport;
      self.advance();
      match self.cur() {
        Token::Class => {
          export_clause = Some(next_parse!(self, self.parse_class_declaration())?.into());
        }
        Token::Function => {
          export_clause = Some(next_parse!(self, self.parse_function_declaration(false, true))?.into());
        }
        Token::Identifier => {
          export_clause = Some(next_parse!(self, self.parse_assignment_expression())?.into());
        }
        _ => {
          export_clause = Some(next_parse!(self, self.parse_assignment_expression())?.into());
        }
      }

      return Ok(new_node!(self, ExportDeclaration, export_type, export_clause, None).into());
    }

    match self.cur() {
      Token::Var => {
        export_clause = Some(next_parse!(self, self.parse_variable_statement())?.into());
      }
      Token::Const | Token::Function | Token::Class => {
        export_clause = Some(next_parse!(self, self.parse_declaration())?.into());
      }
      Token::Identifier => {
        if self.is_value_match_with("let") {
          export_clause = Some(next_parse!(self, self.parse_declaration())?.into());
        }
        return Err("Unexpected token found".to_string());
      }
      _ => {
        export_clause = Some(next_parse!(self, self.parse_export_clause())?.into());
      }
    }

    let mut from_clause: Option<Ast> = None;
    if self.cur() == Token::Identifier && self.is_value_match_with("from") {
      self.advance();
      if self.cur() == Token::StringLiteral {
        let val = LiteralValue::String(FixedU16CodePointArray::from_u16_vec(self.context, self.value()));
        from_clause = Some(new_node!(self, Literal, Token::StringLiteral, val).into());
      } else {
        return Err("Module specifier expected".to_string());
      }
    }

    if self.cur() == Token::Terminate {
      self.advance();
    } else if self.has_line_break_after() {
      return Err("';' expected".to_string());
    }

    return Ok(new_node!(self, ExportDeclaration, export_type, export_clause, from_clause).into());
  }
  fn parse_export_clause(&mut self) -> ParseResult<Expr> {
    return next_parse!(self, self.parse_named_list());
  }

  fn parse_named_list(&mut self) -> ParseResult<Expr> {
    expect!(self, self.cur(), Token::LeftBrace);
    let mut list = new_node!(self, NamedImportList);

    while self.has_more() && self.cur() != Token::RightBrace {
      let identifier = next_parse!(self, self.parse_identifier_reference())?;
      if self.cur() == Token::Identifier && self.is_value_match_with("as") {
        self.advance();
        let value_ref = next_parse!(self, self.parse_identifier())?;
        list.push(new_node!(self, ImportSpecifier, false, Some(identifier), Some(value_ref)).into());
      } else {
        list.push(new_node!(self, ImportSpecifier, false, Some(identifier), None).into());
      }

      if self.cur() == Token::Comma {
        self.advance();
      } else if self.cur() != Token::RightBrace {
        return Err("'}' expected".to_string());
      }
    }

    return Ok(list.into());
  }
}
