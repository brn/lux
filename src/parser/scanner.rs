use super::super::structs::FixedU16CodePointArray;
use super::super::unicode::chars;
use super::error_reporter::*;
use super::parser_state::{ParserState, ParserStateStack};
use super::source::Source;
use super::source_position::SourcePosition;
use super::token::Token;
use crate::utility::*;
use std::cmp::{Eq, PartialEq};
use std::rc::Rc;
use std::vec::Vec;

enum ScannerState {
  HasLinebreakBefore = 1,
  HasLinebreakAfter,
}

#[derive(Property)]
struct Uc32 {
  #[property(get(type = "copy"))]
  code: u32,

  #[property(get(type = "copy"))]
  is_surrogate_pair: bool,
}
impl Uc32 {
  fn new(code: u16) -> Uc32 {
    return Uc32 {
      code: code as u32,
      is_surrogate_pair: false,
    };
  }

  fn from_u32(code: u32) -> Uc32 {
    return Uc32 {
      code,
      is_surrogate_pair: chars::is_surrogate_pair(code as u16),
    };
  }

  fn surrogate_pair(code: u32) -> Uc32 {
    return Uc32 {
      code,
      is_surrogate_pair: true,
    };
  }
}

#[derive(Clone)]
struct SourceCursor {
  source: FixedU16CodePointArray,
  index: isize,
}

const INVALID: u16 = 0xFFFF;
const INVALID_CHAR: char = chars::ch(INVALID);
impl SourceCursor {
  fn new(source: FixedU16CodePointArray) -> SourceCursor {
    return SourceCursor { source, index: -1 };
  }

  fn uc32(&mut self) -> Uc32 {
    if chars::is_high_surrogate(self.val()) {
      let high = self.val();
      if let Some(low) = self.peek() {
        let uc32 = chars::join_surrogate_pair(high, low);
        self.next();
        return Uc32::surrogate_pair(uc32);
      } else {
        return Uc32::new(INVALID);
      }
    }
    return Uc32::new(self.val());
  }

  #[inline]
  fn index(&self) -> isize {
    return self.index;
  }

  fn back(&mut self) -> Option<u16> {
    if self.source.len() == 0 {
      return None;
    }
    if self.index == 0 {
      self.index = -1;
      return None;
    }
    if self.index > 0 {
      self.index -= 1;
      return Some(self.source[self.index as usize]);
    }
    return Some(self.source[0]);
  }

  #[inline]
  fn pos(&self) -> isize {
    return self.index;
  }

  #[inline]
  fn val(&self) -> u16 {
    return self.get_val(self.index).cloned().unwrap_or(INVALID);
  }

  #[inline]
  fn val_ref(&self) -> &u16 {
    return self.get_val(self.index).unwrap_or(&INVALID);
  }

  #[inline]
  fn as_char(&self) -> char {
    return chars::ch(self.val());
  }

  #[inline]
  fn peek(&self) -> Option<u16> {
    return self.get_val(self.index + 1).cloned();
  }

  #[inline]
  fn peek_as_char(&self) -> Option<char> {
    return match self.get_val(self.index + 1).cloned() {
      Some(uc) => Some(chars::ch(uc)),
      _ => None,
    };
  }

  #[inline]
  fn get_val(&self, i: isize) -> Option<&u16> {
    return if i >= 0 && self.source.len() as isize > i {
      Some(&self.source[i as usize])
    } else {
      None
    };
  }
}

impl Iterator for SourceCursor {
  type Item = u16;
  fn next(&mut self) -> Option<Self::Item> {
    if self.index < (self.source.len() as isize) {
      self.index += 1;
    }
    return if self.source.len() as isize > self.index {
      return Some(self.source[self.index as usize]);
    } else {
      None
    };
  }
}

impl std::ops::Deref for SourceCursor {
  type Target = u16;
  fn deref(&self) -> &Self::Target {
    return self.val_ref();
  }
}

pub struct ScannerRecord {
  iter: SourceCursor,
  token: Token,
  position: SourcePosition,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Mode {
  Current = 0,
  Lookahead,
}

pub struct Scanner {
  token: Token,
  lookahead_token: Token,
  iter: SourceCursor,
  region: Region,
  source: Rc<Source>,
  contextual_keywords: [Token; 2],

  numeric_value: [f64; 2],

  literal_buffer: [Vec<u16>; 2],

  scanner_state: [Bitset<u8>; 2],

  parser_state_stack: Exotic<ParserStateStack>,

  previous_position: SourcePosition,
  position: [SourcePosition; 2],

  error_reporter: Exotic<ErrorReporter>,

  mode: Mode,

  skipped: u32,
}

impl ReportSyntaxError for Scanner {
  fn error_reporter(&mut self) -> &mut ErrorReporter {
    return &mut self.error_reporter;
  }

  fn source_position(&self) -> &SourcePosition {
    return self.current_position();
  }
}

impl Scanner {
  pub fn new(
    source: Rc<Source>,
    parser_state_stack: Exotic<ParserStateStack>,
    error_reporter: Exotic<ErrorReporter>,
  ) -> Scanner {
    let mut scanner = Scanner {
      token: Token::Invalid,
      lookahead_token: Token::Invalid,
      region: Region::new(),
      source: source.clone(),
      iter: SourceCursor::new(source.source_code()),
      contextual_keywords: [Token::Invalid, Token::Invalid],
      literal_buffer: [Vec::<u16>::new(), Vec::<u16>::new()],

      scanner_state: [Bitset::<u8>::new(), Bitset::<u8>::new()],

      numeric_value: [0.0_f64, 0.0_f64],

      parser_state_stack,

      previous_position: SourcePosition::new(),
      position: [SourcePosition::new(), SourcePosition::new()],

      error_reporter,

      mode: Mode::Current,

      skipped: 0,
    };
    scanner.advance();
    scanner.current_position_mut().set_end_col(0_u32);
    return scanner;
  }

  pub fn record(&self) -> ScannerRecord {
    return ScannerRecord {
      iter: self.iter.clone(),
      token: self.lookahead_token,
      position: self.position[0].clone(),
    };
  }

  pub fn contextual_keyword(&self) -> Token {
    return self.contextual_keywords[Mode::Current as usize];
  }

  pub fn peek_contextual_keyword(&self) -> Token {
    return self.contextual_keywords[Mode::Lookahead as usize];
  }

  pub fn restore(&mut self, record: &ScannerRecord) {
    self.iter = record.iter.clone();
    self.lookahead_token = record.token;
    self.position[Mode::Current as usize] = record.position.clone();
  }

  #[inline(always)]
  pub fn has_line_break_before(&self) -> bool {
    return self
      .current_scanner_state()
      .get(ScannerState::HasLinebreakBefore as usize);
  }

  #[inline(always)]
  pub fn has_line_break_after(&self) -> bool {
    return self
      .current_scanner_state()
      .get(ScannerState::HasLinebreakAfter as usize);
  }

  #[inline(always)]
  pub fn has_more(&self) -> bool {
    return *self.iter != INVALID && !self.error_reporter.has_pending_error();
  }

  #[inline(always)]
  pub fn cur(&self) -> Token {
    return self.token;
  }

  #[inline(always)]
  pub fn peek_token(&self) -> Token {
    return self.lookahead_token;
  }

  #[inline(always)]
  pub fn value(&self) -> &Vec<u16> {
    return &self.literal_buffer[Mode::Current as usize];
  }

  #[inline(always)]
  pub fn source_index(&self) -> isize {
    return self.iter.index();
  }

  #[inline(always)]
  pub fn numeric_value(&self) -> f64 {
    return self.numeric_value[Mode::Current as usize];
  }

  #[inline(always)]
  pub fn peek_value(&self) -> &Vec<u16> {
    return &self.literal_buffer[Mode::Lookahead as usize];
  }

  #[inline(always)]
  pub fn prev_source_position(&self) -> &SourcePosition {
    return &self.previous_position;
  }

  pub fn clear_peek(&mut self) {
    self.lookahead_token = Token::Invalid;
  }

  fn record_position(&mut self) -> SourcePosition {
    return self.current_position().clone();
  }

  pub fn next(&mut self) -> Token {
    self.mode = Mode::Current;
    if !self.has_more() {
      self.position[Mode::Lookahead as usize] = self.position[Mode::Current as usize].clone();
      let end_col = self.current_position().end_col();
      let end_line = self.current_position().end_line_number();
      self.current_position_mut().set_start_col(end_col);
      self.current_position_mut().set_start_line_number(end_line);
      self.token = Token::End;
      self.lookahead_token = Token::End;
      return self.lookahead_token;
    }

    self.previous_position = self.position[Mode::Current as usize].clone();
    if self.lookahead_token != Token::Invalid {
      self.contextual_keywords[self.mode as usize] = self.peek_contextual_keyword();
      self.token = self.lookahead_token;
      self.lookahead_token = Token::Invalid;
      self.literal_buffer[Mode::Current as usize] = self.literal_buffer[Mode::Lookahead as usize].clone();
      self.position[Mode::Current as usize] = self.position[Mode::Lookahead as usize].clone();
      self.numeric_value[Mode::Current as usize] = self.numeric_value[Mode::Lookahead as usize];
      self.scanner_state[Mode::Current as usize] = self.scanner_state[Mode::Lookahead as usize];
      return self.token;
    }

    self.prologue();
    let token = self.tokenize();
    self.token = token;
    self.epilogue();
    return self.token;
  }

  pub fn peek(&mut self) -> Token {
    let mut this = scoped!(self, |this| {
      this.mode = Mode::Current;
    });
    this.mode = Mode::Lookahead;
    if !this.has_more() {
      this.position[Mode::Lookahead as usize] = this.position[Mode::Current as usize].clone();
      this.lookahead_token = Token::End;
      return this.lookahead_token;
    }
    if this.lookahead_token != Token::Invalid {
      return this.lookahead_token;
    }
    this.position[Mode::Lookahead as usize] = this.position[Mode::Current as usize].clone();
    this.prologue();
    this.lookahead_token = this.tokenize();
    this.epilogue();
    return this.lookahead_token;
  }

  fn is_succeeding(&mut self, value: u16) -> bool {
    if let Some(next) = self.iter.peek() {
      return next == value;
    }
    return false;
  }

  fn decode_hex_escape(&mut self, len: u32) -> Result<u32, ()> {
    let unicode_hex_start = self.iter.as_char();
    let mut ret: u32 = 0;
    if unicode_hex_start == '{' {
      self.advance();
      while self.iter.as_char() != '}' && *self.iter != INVALID {
        if let Ok(hex) = chars::to_hex(*self.iter) {
          ret = ret * 16 + hex;
        } else {
          return Err(());
        }
        self.advance();
      }
      if self.iter.as_char() != '}' {
        return Err(());
      } else {
        self.advance();
      }
    } else {
      for _ in 0..len {
        if let Ok(hex) = chars::to_hex(*self.iter) {
          ret = ret * 16 + hex;
        } else {
          return Err(());
        }
        self.advance();
      }
    }

    return Ok(ret);
  }

  fn decode_ascii_escape(&mut self) -> Result<u16, ()> {
    let u = self.decode_hex_escape(2)?;
    if u > 127 {
      return Err(());
    }
    return Ok(u as u16);
  }

  fn decode_escape_sequence(&mut self) -> Result<u32, ()> {
    self.advance();

    if chars::is_start_unicode_escape_sequence(*self.iter) {
      self.advance();
      return self.decode_hex_escape(4);
    }

    if chars::is_start_ascii_escape_sequence(*self.iter) {
      self.advance();
      return match self.decode_ascii_escape() {
        Ok(u) => Ok(u as u32),
        _ => Err(()),
      };
    }
    return Err(());
  }

  fn prologue(&mut self) {
    self.contextual_keywords[self.mode as usize] = Token::Invalid;
    self.unset_linebreak_before();
    {
      let cur = self.current_position_mut();
      cur.set_start_col(cur.end_col());
    }
    loop {
      if self.skip_line_break() {
        {
          let skipped = self.skipped;
          let end_line_number = self.current_position().end_line_number();
          self
            .current_position_mut()
            .set_start_line_number(end_line_number + skipped);
          self.current_position_mut().set_end_col(0_u32);
          self.current_position_mut().set_start_col(0_u32);
          self.current_position_mut().set_end_col(0_u32);
        }
        self.set_linebreak_before();
      } else if self.skip_whitespace() {
        {
          let skipped = self.skipped;
          self.current_position_mut().add_start_col(skipped);
          let start_col = self.current_position().start_col();
          self.current_position_mut().set_end_col(start_col);
        }
        self.unset_linebreak_before();
      } else {
        break;
      }
    }
  }

  #[inline(always)]
  pub fn current_position(&self) -> &SourcePosition {
    return &self.position[self.mode as usize];
  }

  #[inline(always)]
  fn current_position_mut(&mut self) -> &mut SourcePosition {
    return &mut self.position[self.mode as usize];
  }

  #[inline(always)]
  pub fn current_literal_buffer(&self) -> &Vec<u16> {
    return &self.literal_buffer[self.mode as usize];
  }

  #[inline(always)]
  fn current_literal_buffer_mut(&mut self) -> &mut Vec<u16> {
    return &mut self.literal_buffer[self.mode as usize];
  }

  #[inline(always)]
  pub fn current_numeric_value(&self) -> f64 {
    return self.numeric_value[self.mode as usize];
  }

  #[inline(always)]
  pub fn set_current_numeric_value(&mut self, value: f64) {
    self.numeric_value[self.mode as usize] = value;
  }

  #[inline(always)]
  fn current_scanner_state_mut(&mut self) -> &mut Bitset<u8> {
    return &mut self.scanner_state[self.mode as usize];
  }

  #[inline(always)]
  fn current_scanner_state(&self) -> &Bitset<u8> {
    return &self.scanner_state[self.mode as usize];
  }

  fn epilogue(&mut self) {
    if chars::is_cr_or_lf(*self.iter) {
      self.set_linebreak_after();
    }
  }

  fn tokenize(&mut self) -> Token {
    if self.parser_state_stack.match_state(ParserState::InTemplateLiteral) {
      return self.tokenize_template_literal_characters();
    } else if self.parser_state_stack.is_in_state(ParserState::RegexpExpected) {
      return self.tokenize_regexp_characters();
    }

    use Token::*;
    match self.iter.as_char() {
      '(' => {
        self.advance();
        return LeftParen;
      }
      ')' => {
        self.advance();
        return RightParen;
      }
      '{' => {
        self.advance();
        return LeftBrace;
      }
      '}' => {
        self.advance();
        return RightBrace;
      }
      '[' => {
        self.advance();
        return LeftBracket;
      }
      ']' => {
        self.advance();
        return RightBracket;
      }
      ',' => {
        self.advance();
        return Comma;
      }
      '*' => {
        self.advance();
        if self.iter.as_char() == '*' {
          self.advance();
          if self.iter.as_char() == '=' {
            self.advance();
            return OpPowAssign;
          }
          return OpPow;
        }
        if self.iter.as_char() == '=' {
          self.advance();
          return OpMulAssign;
        }
        return OpMul;
      }
      '/' => {
        self.advance();
        if self.iter.as_char() == '/' {
          self.advance();
          self.skip_signleline_comment();
          return self.tokenize();
        }
        if self.iter.as_char() == '*' {
          self.advance();
          self.skip_multiline_comment();
          return self.tokenize();
        }

        if self.iter.as_char() == '=' {
          self.advance();
          return OpDivAssign;
        }

        return OpDiv;
      }
      '-' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpMinusAssign;
        }
        if self.iter.as_char() == '-' {
          self.advance();
          return OpDecrement;
        }
        return OpMinus;
      }
      '+' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpPlusAssign;
        }
        if self.iter.as_char() == '+' {
          self.advance();
          return OpIncrement;
        }
        return OpPlus;
      }
      '%' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpModAssign;
        }
        return OpMod;
      }
      '<' => {
        self.advance();
        if self.iter.as_char() == '<' {
          self.advance();
          if self.iter.as_char() == '=' {
            self.advance();
            return OpShlAssign;
          }
          return OpShl;
        }
        if self.iter.as_char() == '=' {
          self.advance();
          return OpLessThanOrEq;
        }
        return OpLessThan;
      }
      '>' => {
        self.advance();
        if self.iter.as_char() == '>' {
          self.advance();
          if self.iter.as_char() == '=' {
            self.advance();
            return OpShrAssign;
          }
          if self.iter.as_char() == '>' {
            self.advance();
            if self.iter.as_char() == '=' {
              self.advance();
              return OpUShrAssign;
            }
            return OpUShr;
          }
          return OpShr;
        }
        if self.iter.as_char() == '=' {
          self.advance();
          return OpGreaterThanOrEq;
        }
        return OpGreaterThan;
      }
      '=' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          if self.iter.as_char() == '=' {
            self.advance();
            return OpStrictEq;
          }
          return OpEq;
        }
        if self.iter.as_char() == '>' {
          self.advance();
          return ArrowFunctionGlyph;
        }
        return OpAssign;
      }
      '|' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpOrAssign;
        }
        if self.iter.as_char() == '|' {
          self.advance();
          return OpLogicalOr;
        }
        return OpOr;
      }
      '&' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpAndAssign;
        }
        if self.iter.as_char() == '&' {
          self.advance();
          return OpLogicalAnd;
        }
        return OpAnd;
      }
      '~' => {
        self.advance();
        return OpTilde;
      }
      '^' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          return OpXorAssign;
        }
        return OpXor;
      }
      '.' => {
        if let Some(p) = self.iter.peek() {
          if chars::is_decimal_digits(p) {
            return self.tokenize_numeric_literal(true);
          }
        }
        self.advance();
        if self.iter.as_char() == '.' {
          self.advance();
          if self.iter.as_char() == '.' {
            self.advance();
            return Spread;
          }
          return Invalid;
        }
        return Dot;
      }
      '!' => {
        self.advance();
        if self.iter.as_char() == '=' {
          self.advance();
          if self.iter.as_char() == '=' {
            self.advance();
            return OpStrictNotEq;
          }
          return OpNotEq;
        }
        return OpNot;
      }
      chars::LT_CHAR | chars::PS_CHAR | ';' => {
        self.advance();
        return Terminate;
      }
      ':' => {
        self.advance();
        return Colon;
      }
      '?' => {
        self.advance();
        if self.iter.as_char() == '.' {
          self.advance();
          return OpOptionalChaining;
        }
        if self.iter.as_char() == '?' {
          self.advance();
          return OpNullCoalescing;
        }
        return Question;
      }
      '`' => {
        self.advance();
        self.parser_state_stack.push_state(ParserState::InTemplateLiteral);
        return BackQuote;
      }
      '\'' | '"' => {
        return self.tokenize_string_literal();
      }
      '$' => {
        if self.parser_state_stack.is_in_state(ParserState::InTemplateLiteral) {
          if let Some(peek) = self.iter.peek() {
            if chars::ch(peek) == '{' {
              self.advance();
              return TemplateSubstitution;
            }
          }
        }
        return self.tokenize_identifier();
      }
      _ => {
        if chars::is_decimal_digits(*self.iter) {
          return self.tokenize_numeric_literal(false);
        }
        if chars::is_identifier_start(self.iter.uc32().code()) {
          return self.tokenize_identifier();
        }
        return if self.has_more() { Invalid } else { End };
      }
    }
  }

  fn tokenize_string_literal(&mut self) -> Token {
    self.current_literal_buffer_mut().clear();
    let mut value = *self.iter;
    let start = value;
    self.advance();

    let mut is_escaped = false;
    while self.has_more() {
      value = *self.iter;
      match self.iter.as_char() {
        '\\' => {
          if !is_escaped {
            if let Some(lookahead) = self.iter.peek() {
              if chars::is_start_escape_sequence(lookahead) {
                let escape_sequence_start_pos = self.record_position();
                if let Ok(ret) = self.decode_escape_sequence() {
                  if let Ok((hi, low)) = chars::uc32_to_uc16(ret) {
                    self.current_literal_buffer_mut().push(hi);
                    if low != 0 {
                      self.current_literal_buffer_mut().push(low);
                    }
                  } else {
                    report_error!(
                      self,
                      "Invalid escape sequence",
                      &pos_range!(escape_sequence_start_pos, self.current_position()),
                      Token::Invalid
                    );
                  }
                } else {
                  report_error!(
                    self,
                    "Invalid escape sequence",
                    &pos_range!(escape_sequence_start_pos, self.current_position()),
                    Token::Invalid
                  );
                }
              } else {
                is_escaped = true;
                self.advance();
              }
            } else {
              let start = self.record_position();
              self.advance();
              report_error!(
                self,
                "Unterminated string literal",
                &pos_range!(start, start),
                Token::Invalid
              );
            }
          } else {
            self.advance_and_push_buffer();
            is_escaped = false;
          }
        }
        INVALID_CHAR => {
          report_error!(self, "Unexpected token found", self.source_position(), Token::Invalid);
        }
        _ => {
          if chars::is_cr_or_lf(*self.iter) {
            if !is_escaped {
              let start = self.record_position();
              self.advance();
              report_error!(
                self,
                "Unterminated string literal",
                &pos_range!(start, start),
                Token::Invalid
              );
            }
            self.collect_line_break();
            continue;
          }
          if value == start {
            if !is_escaped {
              self.advance();
              return Token::StringLiteral;
            }
          }
          if is_escaped {
            is_escaped = false;
          }
          self.current_literal_buffer_mut().push(value);
          self.advance();
        }
      }
    }
    report_error!(
      self,
      "Unterminated string literal",
      self.current_position(),
      Token::Invalid
    );
  }

  fn tokenize_identifier(&mut self) -> Token {
    let mut value = self.iter.uc32();
    self.current_literal_buffer_mut().clear();
    debug_assert!(chars::is_identifier_start(value.code()));

    while self.has_more() && chars::is_identifier_continue(value.code(), false) {
      if !value.is_surrogate_pair() && chars::ch(value.code() as u16) == '\\' {
        self.advance();
        let unicode_keyword = self.iter.uc32();
        if !unicode_keyword.is_surrogate_pair() && chars::ch(unicode_keyword.code() as u16) == 'u' {
          let before_decode_pos = self.record_position();
          self.advance();
          if let Ok(u) = self.decode_hex_escape(4) {
            if !chars::is_identifier_continue(u, false) {
              self.current_literal_buffer_mut().clear();
              report_error!(
                self,
                "Invalid unicode escape sequence",
                &pos_range!(before_decode_pos, self.current_position()),
                Token::Invalid
              );
            }
            if let Ok((hi, low)) = chars::uc32_to_uc16(u) {
              self.current_literal_buffer_mut().push(hi);
              if low != 0 {
                self.current_literal_buffer_mut().push(low);
              }
            } else {
              report_error!(
                self,
                "Invalid unicode escape sequence",
                &pos_range!(before_decode_pos, self.current_position()),
                Token::Invalid
              );
            }
          } else {
            self.current_literal_buffer_mut().clear();
            report_error!(
              self,
              "Invalid unicode escape sequence",
              &pos_range!(before_decode_pos, self.current_position()),
              Token::Invalid
            );
          }
        } else {
          self.advance_and_push_buffer();
        }
      } else {
        self.advance_and_push_buffer();
      }
      value = self.iter.uc32();
    }

    let token = self.get_identifier_type();
    if token.is_contextual_keyword() {
      self.contextual_keywords[self.mode as usize] = token;
      return Token::Identifier;
    }
    return token;
  }

  fn get_identifier_type(&mut self) -> Token {
    use Token::*;
    let buf = self.current_literal_buffer();
    if buf.len() == 0 {
      report_error!(self, "Unexpected end of input", self.source_position(), Token::Invalid);
    }
    if buf.len() < 2 || buf.len() > 10 {
      return Token::Identifier;
    }

    macro_rules! _keyword_unroll_check {
      ($buf:expr, $keyword:expr, $token:tt) => {
        {
          let k = $keyword;
          if $buf.len() == k.len() {
            let b = k.as_bytes();
            if $buf[1] as u8 == b[1] &&
              (k.len() <= 2 || $buf[2] as u8 == b[2]) &&
              (k.len() <= 3 || $buf[3] as u8 == b[3]) &&
              (k.len() <= 4 || $buf[4] as u8 == b[4]) &&
              (k.len() <= 5 || $buf[5] as u8 == b[5]) &&
              (k.len() <= 6 || $buf[6] as u8 == b[6]) &&
              (k.len() <= 7 || $buf[7] as u8 == b[7]) &&
              (k.len() <= 8 || $buf[8] as u8 == b[8]) &&
              (k.len() <= 9 || $buf[9] as u8 == b[9]) {
                return $token;
              }
          }
        }
      };
      ($buf:expr, $($group:tt => {$($keyword:tt : $token:tt,)+},)+) => {
        match chars::ch($buf[0]) {
          $(
            $group => {
              $(
                _keyword_unroll_check!($buf, $keyword, $token);
              )*
            }
          )*,
          _ => {
            return Identifier;
          }
        }
      }
    }

    _keyword_unroll_check!(buf, 'a' => {
      "arguments": Arguments,
      "as": As,
      "async": Async,
      "await": Await,
    }, 'b' => {
      "break": Break,
    }, 'c' => {
      "case": Case,
      "catch": Catch,
      "class": Class,
      "const": Const,
      "continue": Continue,
    }, 'd' => {
      "debugger": Debugger,
      "default": Default,
      "delete": Delete,
      "do": Do,
    }, 'e' => {
      "else": Else,
      "enum": Enum,
      "eval": Eval,
      "export": Export,
      "extends": Extends,
    }, 'f' => {
      "false": False,
      "finally": Finally,
      "for": For,
      "from": From,
      "function": Function,
    }, 'g' => {
      "get": Get,
    }, 'i' => {
      "if": If,
      "implements": Implements,
      "import": Import,
      "in": In,
      "instanceof": Instanceof,
      "interface": Interface,
    }, 'l' => {
      "let": Let,
    }, 'n' => {
      "new": New,
      "null": Null,
    }, 'p' => {
      "package": Package,
      "private": Private,
      "protected": Protected,
      "public": Public,
    }, 'r' => {
      "return": Return,
    }, 's' => {
      "set": Set,
      "static": Static,
      "super": Super,
      "switch": Switch,
    }, 't' => {
      "target": Target,
      "this": This,
      "throw": Throw,
      "true": True,
      "try": Try,
      "typeof": Typeof,
    }, 'v' => {
      "var": Var,
      "void": Void,
    }, 'w' => {
      "while": While,
      "with": With,
    }, 'y' => {
      "yield": Yield,
    },);

    return Identifier;
  }

  fn tokenize_numeric_literal(&mut self, is_period_seen: bool) -> Token {
    self.iter.back();
    self.set_current_numeric_value(0.0);
    let pos = self.iter.pos();
    let mut clone = self.iter.clone();
    let result = chars::parse_numeric_value(self.iter.by_ref(), &mut clone, is_period_seen, true);
    if pos != self.iter.pos() {
      self.iter.back();
    }
    let next_pos = if self.iter.pos() >= 0 {
      self.iter.pos() as u32
    } else {
      0_u32
    };
    self.current_position_mut().set_end_col(next_pos);
    self.advance();
    if let Ok((value, kind)) = result {
      self.set_current_numeric_value(value);
      if kind == chars::NumericValueKind::ImplicitOctal {
        return Token::ImplicitOctalLiteral;
      }
      return Token::NumericLiteral;
    }
    let err = result.unwrap_err();
    match err {
      chars::NumericConvertionError::UnexpectedTokenFound | chars::NumericConvertionError::NotANumber => {
        report_error!(self, "Unexpected token found", self.source_position(), Token::Invalid);
      }
      chars::NumericConvertionError::ExponentsExpectedNumber => {
        report_error!(
          self,
          "Number expected after exponents",
          self.source_position(),
          Token::Invalid
        );
      }
      chars::NumericConvertionError::UnexpectedEndOfInput => {
        report_error!(self, "Unexpected end of input", self.source_position(), Token::Invalid);
      }
    };
  }

  fn tokenize_regexp_characters(&mut self) -> Token {
    let value = *self.iter;
    let mut is_escaped = false;
    loop {
      if let Some(lookahead) = self.iter.peek() {
        match chars::ch(lookahead) {
          '\\' => is_escaped = !is_escaped,
          '/' => {
            if !is_escaped {
              return Token::RegExp;
            }
          }
          _ => {
            unreachable!();
          }
        }
        self.advance();
      } else {
        report_error!(self, "Unexpected end of input", self.source_position(), Token::Invalid);
      }
    }
  }

  fn tokenize_template_literal_characters(&mut self) -> Token {
    self.current_literal_buffer_mut().clear();
    let mut is_escaped = false;
    if self.iter.as_char() == '{' {
      self.advance();
    }
    while self.has_more() {
      match self.iter.as_char() {
        '\\' => {
          let mut value = *self.iter;
          if !is_escaped {
            if let Some(lookahead) = self.iter.peek() {
              let escape_sequence_start_pos = self.record_position();
              if chars::is_start_escape_sequence(lookahead) {
                if let Ok(ret) = self.decode_hex_escape(4) {
                  if let Ok((hi, low)) = chars::uc32_to_uc16(ret) {
                    self.current_literal_buffer_mut().push(hi);
                    if low != 0 {
                      self.current_literal_buffer_mut().push(low);
                    }
                  } else {
                    report_error!(
                      self,
                      "Invalid unicode escape sequence",
                      &pos_range!(escape_sequence_start_pos, self.current_position()),
                      Token::Invalid
                    );
                  }
                } else {
                  report_error!(
                    self,
                    "Invalid unicode escape sequence",
                    &pos_range!(escape_sequence_start_pos, self.current_position()),
                    Token::Invalid
                  );
                }
              } else {
                is_escaped = true;
                self.advance();
              }
            } else {
              report_error!(
                self,
                "Unexpected end of input.",
                self.current_position(),
                Token::Invalid
              );
            }
          } else {
            is_escaped = !is_escaped;
          }
        }
        '$' => {
          if !is_escaped {
            if let Some(lookahead) = self.iter.peek() {
              if chars::ch(lookahead) == '{' {
                if self.current_literal_buffer().len() > 0 {
                  return Token::StringLiteral;
                }
                self.advance();
                self.advance();
                return Token::TemplateSubstitution;
              }
            } else {
              report_error!(
                self,
                "Unexpected end of input.",
                self.current_position(),
                Token::Invalid
              );
            }
          } else {
            is_escaped = false;
            self.advance_and_push_buffer();
          }
        }
        '`' => {
          if !is_escaped {
            if self.current_literal_buffer().len() > 0 {
              return Token::StringLiteral;
            }
            self.advance();
            return Token::Template;
          } else {
            is_escaped = false;
            self.advance_and_push_buffer();
          }
        }
        chars::CR_CHAR => {
          if let Some(next) = self.iter.peek() {
            if chars::is_lf(next) {
              self.advance_and_push_buffer();
            }
            self.advance_and_push_buffer();
            self.current_position_mut().set_end_col(0_u32);
            self.current_position_mut().inc_end_line_number();
          }
        }
        chars::LF_CHAR => {
          self.advance_and_push_buffer();
          self.current_position_mut().set_end_col(0_u32);
          self.current_position_mut().inc_end_line_number();
        }
        _ => {
          self.advance_and_push_buffer();
        }
      }
    }

    report_error!(
      self,
      "Unterminated template literal.",
      self.source_position(),
      Token::Invalid
    );
  }

  fn collect_line_break(&mut self) {
    if chars::is_cr(*self.iter) {
      self.advance_and_push_buffer();
      if chars::is_lf(*self.iter) {
        self.advance_and_push_buffer();
      }
    }
  }

  fn advance_and_push_buffer(&mut self) -> u16 {
    let value = *self.iter;
    self.current_literal_buffer_mut().push(value);
    return self.advance();
  }

  fn advance(&mut self) -> u16 {
    self.iter.next();
    self.current_position_mut().inc_end_col();
    return *self.iter;
  }

  fn skip_line_break(&mut self) -> bool {
    self.skipped = 0;
    if chars::is_cr(*self.iter) {
      self.advance();
      self.skipped = 1;
      if chars::is_lf(*self.iter) {
        self.advance();
        self.skipped = 2;
      }
      return true;
    }
    return false;
  }

  fn skip_whitespace(&mut self) -> bool {
    self.skipped = 0;
    let mut whitespace_seen = false;
    while chars::is_whitespace(*self.iter) {
      self.advance();
      self.skipped += 1;
      whitespace_seen = true;
    }
    return whitespace_seen;
  }

  fn skip_signleline_comment(&mut self) {
    while self.has_more() {
      if chars::is_cr(*self.iter) {
        self.advance();
        if chars::is_lf(*self.iter) {
          self.advance();
          return;
        }
      }
      if chars::is_lf(*self.iter) {
        self.advance();
        return;
      }
      self.advance();
    }
  }

  fn skip_multiline_comment(&mut self) {
    loop {
      if chars::ch(*self.iter) == '*' {
        if let Some(next) = self.iter.peek() {
          if chars::ch(next) == '/' {
            self.advance();
            self.advance();
            return;
          }
        } else {
          return;
        }
      }
      self.advance();
    }
  }

  #[inline(always)]
  fn unset_linebreak_before(&mut self) {
    self.unset_flag(ScannerState::HasLinebreakBefore);
  }

  #[inline(always)]
  fn set_linebreak_before(&mut self) {
    self.set_flag(ScannerState::HasLinebreakBefore);
  }

  #[inline(always)]
  fn unset_linebreak_after(&mut self) {
    self.unset_flag(ScannerState::HasLinebreakAfter);
  }

  #[inline(always)]
  fn set_linebreak_after(&mut self) {
    self.set_flag(ScannerState::HasLinebreakAfter);
  }

  #[inline(always)]
  fn set_flag(&mut self, state: ScannerState) {
    self.current_scanner_state_mut().set(state as usize);
  }

  #[inline(always)]
  fn unset_flag(&mut self, state: ScannerState) {
    self.current_scanner_state_mut().unset(state as usize);
  }

  #[inline(always)]
  fn get_flag(&self, state: ScannerState) -> bool {
    return self.current_scanner_state().get(state as usize);
  }
}
