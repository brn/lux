use super::super::structs::{FixedU16CodePointArray, FixedU16CodePointArrayIterator};
use super::super::unicode::chars;
use super::error_reporter::*;
use super::parser_state::{ParserState, ParserStateStack};
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
  Await,
  BackQuote,
  Break,
  Case,
  Catch,
  Class,
  Colon,
  Comma,
  Const,
  Continue,
  Debugger,
  Default,
  Delete,
  Do,
  Dot,
  Else,
  Enum,
  Export,
  Extends,
  False,
  Finally,
  For,
  Function,
  If,
  Import,
  In,
  Instanceof,
  LeftBrace,
  LeftBracket,
  LeftParen,
  New,
  Null,
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
  OpNullCoalescing,
  OpOptionalChaining,
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
  Return,
  RightBrace,
  RightBracket,
  RightParen,
  Spread,
  Super,
  Switch,
  Terminate,
  This,
  Throw,
  True,
  Try,
  Typeof,
  Var,
  Void,
  While,
  With,
  Yield,
  Identifier,
  NumericLiteral,
  StringLiteral,
  Template,
  TemplateSubstitution,
  End,
  Invalid,
}

enum ScannerState {
  ImplicitOctal,
  HasLinebreakBefore,
  HasLinebreakAfter,
}

#[derive(Copy, Clone)]
pub struct IterVal {
  value: u16,
  iter: Peekable<FixedU16CodePointArrayIterator>,
}

const INVALID: u16 = 0xFFFF;
const INVALID_CHAR: char = chars::ch(INVALID);
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
    return chars::ch(self.value);
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

  scanner_state: Bitset<u8>,
  lookahead_scanner_state: Bitset<u8>,
  current_scanner_state: &'a mut Bitset<u8>,

  parser_state_stack: &'a mut ParserStateStack,

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
    parser_state_stack: &'a mut ParserStateStack,
  ) -> Pin<Box<Scanner<'a>>> {
    let source = FixedU16CodePointArray::from_utf8(context, source);
    let literal_buffer = Vec::<u16>::new();
    let position = SourcePosition::new();
    let scanner_state = Bitset::<u8>::new();
    return Box::pin(Scanner {
      token: Token::Invalid,
      lookahead_token: Token::Invalid,
      source,
      iter: IterVal::new(source.into_iter().peekable()),

      literal_buffer,
      lookahead_literal_buffer: Vec::<u16>::new(),
      current_literal_buffer: &mut literal_buffer,

      scanner_state,
      lookahead_scanner_state: Bitset::<u8>::new(),
      current_scanner_state: &mut scanner_state,

      parser_state_stack,

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
    self.current_scanner_state = &mut self.scanner_state;
    if self.has_more() {
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
      self.scanner_state = self.lookahead_scanner_state;
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
    self.current_scanner_state = &mut self.lookahead_scanner_state;
    if !self.has_more() {
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

  fn decode_escape_sequence(&mut self) -> Result<u16, ()> {
    self.advance();
    let mut result = INVALID;
    if chars::is_start_unicode_escape_sequence(*self.iter) {
      self.advance();
      return self.decode_hex_escape(4);
    }

    if chars::is_start_ascii_escape_sequence(*self.iter) {
      self.advance();
      return self.decode_ascii_escape();
    }
    return Err(());
  }

  fn prologue(&'a mut self) {
    self.current_position.set_start_col(self.current_position.end_col());
    self.current_scanner_state = &mut self.scanner_state;
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
    if self.parser_state_stack.is_in_state(ParserState::InTemplateLiteral) {
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
        self.advance();
        if chars::is_decimal_digits(*self.iter) {
          return self.tokenize_numeric_literal(true);
        }
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
    }
  }

  fn tokenize_string_literal(&mut self) -> Token {
    self.current_literal_buffer.clear();
    let mut value = *self.iter;
    let start = value;
    self.advance();

    let is_escaped = false;
    while self.has_more() {
      value = *self.iter;
      match self.iter.as_char() {
        '\\' => {
          if !is_escaped {
            if let Some(lookahead) = self.iter.peek() {
              if chars::is_start_escape_sequence(lookahead) {
                let mut ok = true;
                if let Ok(ret) = self.decode_escape_sequence() {
                  value = ret;
                } else {
                  report_syntax_error!(self, "Invalid unicode escape sequence found", Token::Invalid);
                }
              } else {
                is_escaped = true;
                self.advance();
                break;
              }
            }
          } else {
            self.advance();
          }
          is_escaped = false;
          self.current_literal_buffer.push(value);
          break;
        }
        INVALID_CHAR => {
          return Token::Invalid;
        }
        _ => {
          if chars::is_cr_or_lf(*self.iter) {
            if !is_escaped {
              report_syntax_error!(self, "Unterminated string literal", Token::Invalid);
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
          self.current_literal_buffer.push(value);
          self.advance();
        }
      }
    }
    unreachable!();
  }

  fn tokenize_identifier(&mut self) -> Token {
    self.current_literal_buffer.clear();
    let mut value = *self.iter;
    debug_assert!(chars::is_identifier_start(value));
    while self.has_more() && chars::is_identifier_continue(value, false) {
      if chars::ch(value) == '\\' {
        let v = Vec::<u16>::new();
        self.advance();
        let unicode_keyword = *self.iter;
        if chars::ch(unicode_keyword) == 'u' {
          self.advance();
          let mut ok = true;
          if let Ok(u) = self.decode_hex_escape(4) {
            value = u;
          } else {
            return Token::Invalid;
          }
        }
      }
      self.advance_and_push_buffer();
      value = *self.iter;
    }

    return self.get_identifier_type();
  }

  fn get_identifier_type(&self) -> Token {
    use Token::*;
    let buf = self.current_literal_buffer;
    if buf.len() == 0 {
      return Token::Invalid;
    }
    if buf.len() < 2 || buf.len() >= 10 {
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
      "export": Export,
      "extends": Extends,
    }, 'f' => {
      "finally": Finally,
      "for": For,
      "function": Function,
    }, 'i' => {
      "if": If,
      "import": Import,
      "in": In,
      "instanceof": Instanceof,
    }, 'n' => {
      "new": New,
      "null": Null,
    }, 'r' => {
      "return": Return,
    }, 's' => {
      "super": Super,
      "switch": Switch,
    }, 't' => {
      "this": This,
      "throw": Throw,
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
    if is_period_seen {
      return Token::NumericLiteral;
    }
    return Token::NumericLiteral;
  }

  fn tokenize_regexp_characters(&mut self) -> Token {
    return Token::NumericLiteral;
  }

  fn tokenize_template_literal_characters(&mut self) -> Token {
    return Token::NumericLiteral;
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
    self.current_literal_buffer.push(*self.iter);
    return self.advance();
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
  fn has_more(&self) -> bool {
    return *self.iter != INVALID && !self.error_reporter.has_pending_error();
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
    self.current_scanner_state.set(state as usize);
  }

  #[inline(always)]
  fn unset_flag(&mut self, state: ScannerState) {
    self.current_scanner_state.set(state as usize);
  }

  #[inline(always)]
  fn get_flag(&self, state: ScannerState) -> bool {
    return self.current_scanner_state.get(state as usize);
  }
}
