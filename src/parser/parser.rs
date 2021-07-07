use super::super::structs::FixedU16CodePointArray;
use super::ast::*;
use super::error_reporter::*;
use super::parser_def::ParserDef;
use super::parser_state::{ParserState, ParserStateStack};
use super::scanner::*;
use super::source_position::*;
use super::token::Token;
use crate::context::{Context, LuxContext, ObjectRecordsInitializedContext};
use crate::utility::*;

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
}

macro_rules! new_node {
  ($name:tt, $($args:expr),+$(,)?) => {
    $name::new(&mut self.region, $(args),*)
  }
}

impl Parser {
  pub fn new(context: impl ObjectRecordsInitializedContext, source: &str) -> Self {
    let mut parser = Parser {
      context: LuxContext::from_allocation_only_context(context),
      parser_type: ParserType::Script,
      parser_state: Exotic::new(std::ptr::null_mut()),
      scanner: Exotic::new(std::ptr::null_mut()),
      region: Region::new(),
      result: None,
      source: FixedU16CodePointArray::from_utf8(context, source),
      scanner_record: None,
    };
    parser.parser_state = parser.region.alloc(ParserStateStack::new());
    parser.scanner = parser
      .region
      .alloc(Scanner::new(parser.source, parser.parser_state.copy_unchecked()));
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
}
