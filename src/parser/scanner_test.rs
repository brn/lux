#[cfg(test)]
mod scanner_test {
  use super::super::error_reporter::{ErrorReporter, ReportSyntaxError};
  use super::super::parser_state::{ParserState, ParserStateStack};
  use super::super::scanner::*;
  use super::super::source::*;
  use super::super::source_position::SourcePosition;
  use super::super::token::*;
  use crate::context::LuxContext;
  use crate::structs::FixedU16CodePointArray;
  use crate::unicode::chars;
  use crate::utility::*;
  use itertools::Itertools;
  use std::char::decode_utf16;
  use std::rc::Rc;
  use std::vec::Vec;

  fn init_scanner<T>(
    source: &str,
    opt_parser_state_stack: Option<ParserStateStack>,
    mut cb: impl FnMut(Scanner) -> T,
  ) -> T {
    let context = LuxContext::new_until_internal_object_records();
    let mut parser_state_stack = Box::new(if opt_parser_state_stack.is_some() {
      opt_parser_state_stack.unwrap()
    } else {
      ParserStateStack::new()
    });
    let source = Source::new(context, "anonymouse", &source);
    let error_reporter = Exotic::from(Box::new(ErrorReporter::new(source.clone())));
    let mut scanner = Scanner::new(Region::new(), source.clone(), parser_state_stack.into(), error_reporter);
    return cb(scanner);
  }

  fn scan(source: &str) -> Vec<Token> {
    return init_scanner(source, None, |mut scanner| {
      let mut vec = Vec::new();
      loop {
        let token = scanner.next();
        vec.push(token);
        if token == Token::End || token == Token::Invalid {
          return vec;
        }
      }
    });
  }

  fn scan_test(token: Token) {
    scan_test_with_symbol(token.symbol(), token);
  }

  fn scan_test_with_symbol(symbol: &str, token: Token) {
    let ret = scan(symbol);
    assert_eq!(ret.len(), 2);
    assert_eq!(ret[0], token);
    assert_eq!(ret[1], Token::End);
  }

  #[test]
  fn scanner_all_token_test() {
    for token in Token::values().filter(|token| !token.is_pseudo_token() && !token.is_contextual_keyword()) {
      scan_test(*token);
    }
  }

  #[test]
  fn scanner_all_token_once_test() {
    let base_iter = Token::values()
      .filter(|token| !token.is_pseudo_token() && !token.is_contextual_keyword() && **token != Token::BackQuote);
    let source: String = base_iter.clone().map(|token| token.symbol()).intersperse(" ").collect();
    let results = scan(&source);
    let mut expected = base_iter.clone().collect::<Vec<_>>();
    expected.push(&Token::End);
    for (i, token) in results.iter().enumerate() {
      assert_eq!(token, expected[i]);
    }
  }

  #[test]
  fn scanner_scan_identifier_test() {
    init_scanner("$identifier_", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "$identifier_");
    });

    init_scanner("_identifier_", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "_identifier_");
    });

    init_scanner("$__identifier_", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "$__identifier_");
    });

    init_scanner("IDENTIFIER", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "IDENTIFIER");
    });
  }

  #[test]
  fn scanner_scan_contextual_keyword_test() {
    init_scanner("eval", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Eval);
    });

    init_scanner("arguments", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Arguments);
    });

    init_scanner("async", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Async);
    });

    init_scanner("let", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Let);
    });

    init_scanner("target", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Target);
    });

    init_scanner("get", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Get);
    });

    init_scanner("set", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Set);
    });

    init_scanner("from", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::From);
    });

    init_scanner("as", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::As);
    });

    init_scanner("implements", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Implements);
    });

    init_scanner("interface", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Interface);
    });

    init_scanner("package", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Package);
    });

    init_scanner("private", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Private);
    });

    init_scanner("protected", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Protected);
    });

    init_scanner("public", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Public);
    });

    init_scanner("static", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      assert_eq!(scanner.contextual_keyword(), Token::Static);
    });
  }

  #[test]
  fn scanner_scan_unicode_escape_sequence_test() {
    init_scanner("\\u0061\\u0062\\u0063\\u0064", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Identifier);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "abcd");
    });

    init_scanner("\\u0011", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().has_pending_error());
    });
  }

  #[test]
  fn scanner_scan_unicode_escape_sequence_with_surrogate_pair_test() {
    init_scanner(
      "\\u0061\\u{2A6D6}\\u0062\\u{2A700}\\u0063\\u{2B740}\\u0064\\u{2B746}",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::Identifier);
        let value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "a\u{2A6D6}b\u{2A700}c\u{2B740}d\u{2B746}");
        assert_eq!(scanner.current_literal_buffer().len(), 12);
      },
    );
  }

  #[test]
  fn scanner_scan_string_literal() {
    init_scanner("'test string'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "test string");
    });
  }

  #[test]
  fn scanner_scan_escaped_string_literal() {
    init_scanner("'test \\'string'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "test \'string");
    });
  }

  #[test]
  fn scanner_scan_double_escaped_string_literal() {
    init_scanner("'test \\\\'string'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "test \\");
    });
  }

  #[test]
  fn scanner_scan_unterminated_string_literal() {
    init_scanner("'test", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(0), Some(5), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_unicode_escaped_string_literal() {
    init_scanner("'\\u0061_foo_\\u0062_bar_\\u0063_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "a_foo_b_bar_c_baz");
    });
  }

  #[test]
  fn scanner_scan_unicode_escaped_string_literal2() {
    init_scanner(
      "'foo_\\u0061\\u{2A6D6}\\u0062\\u{2A700}\\u0063\\u{2B740}\\u0064_bar_\\u{2B746}'",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::StringLiteral);
        let value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "foo_a\u{2A6D6}b\u{2A700}c\u{2B740}d_bar_\u{2B746}");
      },
    );
  }

  #[test]
  fn scanner_scan_octal_escaped_string_literal() {
    init_scanner("'foo_\\141_bar_\\142_baz_\\143'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "foo_a_bar_b_baz_c");
    });
  }

  #[test]
  fn scanner_scan_invalid_unicode_escaped_string_literal() {
    init_scanner("'\\u006_foo_\\u0062_bar_\\u0063_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
    });
  }

  #[test]
  fn scanner_scan_invalid_unicode_escaped_string_literal2() {
    init_scanner("'\\u0061_foo_\\u062_bar_\\u0063_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
    });
  }

  #[test]
  fn scanner_scan_invalid_unicode_escaped_string_literal3() {
    init_scanner("'\\ux0061_foo_\\u0062_bar_\\u0-063_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
    });
  }

  #[test]
  fn scanner_scan_ascii_escaped_string_literal() {
    init_scanner("'\\x61_foo_\\x62_bar_\\x63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "a_foo_b_bar_c_baz");
    });
  }

  #[test]
  fn scanner_scan_invalid_ascii_escaped_string_literal() {
    init_scanner("'\\x6_foo_\\x62_bar_\\x63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(1), Some(5), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_invalid_ascii_escaped_string_literal2() {
    init_scanner("'\\x61_foo_\\x2_bar_\\x63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(10), Some(14), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_invalid_ascii_escaped_string_literal3() {
    init_scanner("'\\x61_foo_\\x62_bar_\\x-63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(19), Some(22), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_invalid_escaped_string_literal4() {
    init_scanner("'a\\", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(2), Some(3), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_invalid_escaped_string_literal5() {
    init_scanner("'a\n", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
      assert!(scanner.error_reporter().last_error().is_some());
      assert_eq!(
        *scanner.error_reporter().last_error().unwrap().source_position(),
        SourcePosition::with(Some(2), Some(3), Some(0), Some(0))
      );
    });
  }

  #[test]
  fn scanner_scan_digit_double() {
    init_scanner(".3032", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 0.3032);
    });
  }

  #[test]
  fn scanner_scan_digit_hex() {
    init_scanner("0xFEFEFE", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 0xFEFEFE as f64);
    });
  }

  #[test]
  fn scanner_scan_int() {
    init_scanner("1349075", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1349075.0);
    });
  }

  #[test]
  fn scanner_scan_double2() {
    init_scanner("1349.075", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1349.075);
    });
  }

  #[test]
  fn scanner_scan_digit_exponents() {
    init_scanner("1349e+2", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1349e+2 as f64);
    });
  }

  #[test]
  fn scanner_scan_digit_exponents2() {
    init_scanner("1349e-2", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1349e-2 as f64);
    });
  }

  #[test]
  fn scanner_scan_digit_exponents3() {
    init_scanner("1349e20", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1349e20 as f64);
    });
  }

  #[test]
  fn scanner_scan_digit_double_exponents3() {
    init_scanner("1.3e+1", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 1.3e+1 as f64);
    });
  }

  #[test]
  fn scanner_scan_digit_octal() {
    init_scanner("0o72377", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::NumericLiteral);
      assert_eq!(scanner.current_numeric_value(), 0o72377 as f64);
    });
  }

  #[test]
  fn scanner_scan_digit_implicit_octal() {
    init_scanner("072377", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::ImplicitOctalLiteral);
      assert_eq!(scanner.current_numeric_value(), 0o72377 as f64);
    });
  }

  #[test]
  fn scanner_scan_single_line_comment() {
    init_scanner(
      "//abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()-_|\\`~{}[]\"\':;/?.<>,",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::End);
      },
    );
  }

  #[test]
  fn scanner_scan_single_line_comment_with_line_feed() {
    init_scanner(
      "foo//abcdefghijklmnopqrstuvwxyz1234567890!@#$%^&*()-_|\\`~{}[]\"\':;/?.<>,\naaa",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::Identifier);
        let mut value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "foo");
        assert_eq!(scanner.next(), Token::Identifier);
        value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "aaa");
        assert_eq!(scanner.next(), Token::End);
      },
    );
  }

  #[test]
  fn scanner_scan_multiline_comments() {
    init_scanner(
      "foo/*aaaaaaaa\nbbbbbbbbbb\ncccccccccccc\nddddddddddddd*/aaa",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::Identifier);
        let mut value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "foo");
        assert_eq!(scanner.next(), Token::Identifier);
        value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "aaa");
        assert_eq!(scanner.next(), Token::End);
      },
    );
  }

  #[test]
  fn scanner_scan_multiline_comments_2() {
    init_scanner(
      "foo/*aaaaaaaa\r\nbbbbbbbbbb\r\ncccccccccccc\r\nddddddddddddd*/aaa",
      None,
      |mut scanner| {
        assert_eq!(scanner.next(), Token::Identifier);
        let mut value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "foo");
        assert_eq!(scanner.next(), Token::Identifier);
        value = chars::to_utf8(scanner.current_literal_buffer());
        assert_eq!(&value, "aaa");
        assert_eq!(scanner.next(), Token::End);
      },
    );
  }

  #[test]
  fn scanner_scan_template_literal() {
    let mut state = ParserStateStack::new();
    state.push_state(ParserState::InTemplateLiteral);
    init_scanner("abcdefg`", Some(state), |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let mut value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "abcdefg");
      assert_eq!(scanner.next(), Token::Template);
    });
  }

  #[test]
  fn scanner_scan_template_literal_substitution() {
    let mut state = ParserStateStack::new();
    state.push_state(ParserState::InTemplateLiteral);
    init_scanner("abcdefg${`", Some(state), |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let mut value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "abcdefg");
      assert_eq!(scanner.next(), Token::TemplateSubstitution);
    });
  }
}
