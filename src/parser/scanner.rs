use super::super::structs::{FixedU16CodePointArray, FixedU16CodePointArrayIterator};
use super::super::unicode::chars;
use super::error_reporter::*;
use super::source_position::SourcePosition;
use crate::context::ObjectRecordsInitializedContext;
use crate::utility::*;
use std::boxed::Box;
use std::cmp::{Eq, PartialEq};
use std::iter::Peekable;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::vec::Vec;

#[derive(PartialEq, Eq)]
pub enum Token {
  ArrowFunctionGlyph,
  BackQuote,
  Colon,
  Comma,
  False,
  LeftBrace,
  LeftBracket,
  LeftParen,
  OpAnd,
  OpAndAssign,
  OpAssign,
  OpDecrement,
  OpDiv,
  OpDivAssign,
  OpEq,
  OpGreaterThan,
  OpGreaterThanOrEq,
  OpIncrement,
  OpLessThan,
  OpLessThanOrEq,
  OpLogicalAnd,
  OpLogicalOr,
  OpMinus,
  OpMinusAssign,
  OpMod,
  OpModAssign,
  OpMul,
  OpMulAssign,
  OpNot,
  OpNotEq,
  OpOr,
  OpOrAssign,
  OpPlus,
  OpPlusAssign,
  OpPow,
  OpPowAssign,
  OpShl,
  OpShlAssign,
  OpShr,
  OpShrAssign,
  OpStrictEq,
  OpStrictNotEq,
  OpTilde,
  OpUShr,
  OpUShrAssign,
  OpXor,
  OpXorAssign,
  Question,
  RightBrace,
  RightBracket,
  RightParen,
  Spread,
  Terminate,
  True,
  Identifier,
  NumericLiteral,
  StringLiteral,
  Template,
  End,
  Invalid,
}

pub enum ScannerState {
  ImplicitOctal,
  HasLinebreakBefore,
  HasLinebreakAfter,
  InTemplateLiteral,
  RegexpExpected,
}

#[derive(Copy, Clone)]
pub struct IterVal {
  value: u16,
  iter: Peekable<FixedU16CodePointArrayIterator>,
}

const INVALID: u16 = 0xFFFF;
impl IterVal {
  pub fn new(iter: Peekable<FixedU16CodePointArrayIterator>) -> IterVal {
    return IterVal { value: INVALID, iter };
  }

  pub fn next(&mut self) -> u16 {
    self.value = self.iter.next().unwrap_or(INVALID);
    return self.value;
  }

  pub fn peek(&mut self) -> Option<u16> {
    return self.iter.peek();
  }

  pub fn as_char(&self) -> char {
    return self.value as char;
  }
}

impl std::ops::Deref for IterVal {
  type Target = u16;
  fn deref(&self) -> &Self::Target {
    return &self.value;
  }
}

pub struct ScannerRecord {
  iter: IterVal,
  token: Token,
  position: SourcePosition,
}

pub struct Scanner<'a> {
  token: Token,
  lookahead_token: Token,
  iter: IterVal,
  source: FixedU16CodePointArray,

  literal_buffer: Vec<u16>,
  lookahead_literal_buffer: Vec<u16>,
  current_literal_buffer: &'a mut Vec<u16>,

  state: Bitset<u8>,
  lookahead_state: Bitset<u8>,
  current_state: &'a mut Bitset<u8>,

  previous_position: SourcePosition,
  position: SourcePosition,
  lookahead_position: SourcePosition,
  current_position: &'a mut SourcePosition,

  error_reporter: &'a mut ErrorReporter,

  skipped: u32,
  _pinned: PhantomPinned,
}

impl<'a> ReportSyntaxError for Scanner<'a> {
  fn error_reporter(&mut self) -> &mut ErrorReporter {
    return self.error_reporter;
  }
}

impl<'a> Scanner<'a> {
  pub fn new(
    context: impl ObjectRecordsInitializedContext,
    source: &str,
    error_reporter: &'a mut ErrorReporter,
  ) -> Pin<Box<Scanner<'a>>> {
    let source = FixedU16CodePointArray::from_utf8(context, source);
    let literal_buffer = Vec::<u16>::new();
    let position = SourcePosition::new();
    let state = Bitset::<u8>::new();
    return Box::pin(Scanner {
      token: Token::Invalid,
      lookahead_token: Token::Invalid,
      source,
      iter: IterVal::new(source.into_iter().peekable()),

      literal_buffer,
      lookahead_literal_buffer: Vec::<u16>::new(),
      current_literal_buffer: &mut literal_buffer,

      state,
      lookahead_state: Bitset::<u8>::new(),
      current_state: &mut state,

      previous_position: position,
      position,
      lookahead_position: SourcePosition::new(),
      current_position: &mut position,

      error_reporter,

      skipped: 0,
      _pinned: PhantomPinned,
    });
  }

  pub fn record(&self) -> ScannerRecord {
    return ScannerRecord {
      iter: self.iter.clone(),
      token: self.lookahead_token,
      position: self.position,
    };
  }

  pub fn restore(&mut self, record: &'a ScannerRecord) {
    self.iter = record.iter;
    self.lookahead_token = record.token;
    self.position = record.position;
  }

  pub fn next(&'a mut self) -> Token {
    self.current_literal_buffer = &mut self.literal_buffer;
    self.current_position = &mut self.position;
    self.current_state = &mut self.state;
    if self.iter.peek().is_none() {
      self.lookahead_position = self.position;
      self.lookahead_token = Token::End;
      return self.lookahead_token;
    }
    self.previous_position = self.position;
    if self.lookahead_token != Token::Invalid {
      self.token = self.lookahead_token;
      self.lookahead_token = Token::Invalid;
      self.literal_buffer = self.lookahead_literal_buffer;
      self.position = self.lookahead_position;
      self.state = self.lookahead_state;
      return self.token;
    }
    self.prologue();
    self.token = self.tokenize();
    self.epilogue();
    return self.token;
  }

  pub fn peek(&'a mut self) -> Token {
    self.current_literal_buffer = &mut self.lookahead_literal_buffer;
    self.current_position = &mut self.lookahead_position;
    self.current_state = &mut self.lookahead_state;
    if self.iter.peek().is_none() {
      self.lookahead_position = self.position;
      self.lookahead_token = Token::End;
      return self.lookahead_token;
    }
    if self.lookahead_token != Token::Invalid {
      return self.lookahead_token;
    }
    self.lookahead_position = self.position;
    self.prologue();
    self.lookahead_token = self.tokenize();
    self.epilogue();
    return self.lookahead_token;
  }

  fn is_succeeding(&mut self, value: u16) -> bool {
    if let Some(next) = self.iter.peek() {
      return next == value;
    }
    return false;
  }

  fn decode_hex_escape(&mut self, len: u32) -> Result<u16, ()> {
    let unicode_hex_start = self.iter.as_char();
    let mut ret: u16 = 0;
    if unicode_hex_start == '{' {
      self.advance();
      while self.iter.as_char() == '}' {
        ret = ret * 16 + chars::to_hex(*self.iter) as u16;
        self.advance();
      }

      if self.iter.as_char() != '}' {
        report_syntax_error!(noreturn self, "'}' expected");
      } else {
        self.advance();
      }
    } else {
      for i in 0..len {
        if chars::is_hex_digits(*self.iter) {
          ret = ret * 16 + chars::to_hex(*self.iter) as u16;
        } else {
          report_syntax_error!(self, "Unrecognized hex token", Err(()));
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
    return Ok(u);
  }

  fn prologue(&mut self) {
    self.current_position.set_start_col(self.current_position.end_col());
    self.unset_flag(ScannerState::ImplicitOctal);
    self.unset_linebreak_before();
    loop {
      if self.skip_line_break() {
        self
          .current_position
          .set_start_line_number(self.current_position.end_line_number() + self.skipped);
        self.current_position.set_end_col(0_u32);
        self.current_position.set_start_col(0_u32);
        self.current_position.set_end_col(0_u32);
        self.set_linebreak_before();
      } else if self.skip_whitespace() {
        self.current_position.add_start_col(self.skipped);
        self.current_position.set_end_col(self.current_position.start_col());
        self.unset_linebreak_before();
      } else {
        break;
      }
    }
  }

  fn epilogue(&mut self) {
    if chars::is_cr_or_lf(*self.iter) {
      self.set_linebreak_after();
    }
  }

  fn tokenize(&self) -> Token {
    if self.get_flag(ScannerState::InTemplateLiteral) {
      return self.tokenize_template_literal_characters();
    } else if self.get_flag(ScannerState::RegexpExpected) {
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
    }
  }

  fn advance(&mut self) -> u16 {
    self.iter.next();
    self.current_position.inc_end_col();
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
    let whitespace_seen = false;
    while chars::is_whitespace(*self.iter) {
      self.advance();
      self.skipped += 1;
      whitespace_seen = true;
    }
    return whitespace_seen;
  }

  fn skip_signleline_comment(&mut self) {
    while self.iter.peek().is_some() {
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
        let next = self.iter.peek().unwrap_or(INVALID);
        if chars::ch(next) == '/' {
          self.advance();
          return;
        }
      } else if *self.iter == INVALID {
        return;
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
    self.state.set(state as usize);
  }

  #[inline(always)]
  fn get_flag(&mut self, state: ScannerState) -> bool {
    return self.state.get(state as usize);
  }

  #[inline(always)]
  fn unset_flag(&mut self, state: ScannerState) {
    self.state.set(state as usize);
  }
}
