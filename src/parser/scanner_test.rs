#[cfg(test)]
mod scanner_test {
  use super::super::error_reporter::ReportSyntaxError;
  use super::super::parser_state::ParserStateStack;
  use super::super::scanner::*;
  use crate::context::LuxContext;
  use crate::unicode::chars;
  use itertools::Itertools;
  use std::char::decode_utf16;
  use std::vec::Vec;

  fn init_scanner<'a, T>(
    source: &str,
    opt_parser_state_stack: Option<ParserStateStack>,
    mut cb: impl FnMut(Scanner) -> T,
  ) -> T {
    let context = LuxContext::new_until_internal_object_records();
    let mut parser_state_stack = if opt_parser_state_stack.is_some() {
      opt_parser_state_stack.unwrap()
    } else {
      ParserStateStack::new()
    };
    let mut scanner = Scanner::new(context, source, &parser_state_stack);
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
    for token in Token::values().filter(|token| !token.is_pseudo_token()) {
      scan_test(*token);
    }
  }

  #[test]
  fn scanner_all_token_once_test() {
    let base_iter = Token::values().filter(|token| !token.is_pseudo_token());
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
      assert_eq!(&value, "test \\'string");
    });
  }

  #[test]
  fn scanner_scan_double_escaped_string_literal() {
    init_scanner("'test \\\\'string'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::StringLiteral);
      let value = chars::to_utf8(scanner.current_literal_buffer());
      assert_eq!(&value, "test \\\\");
    });
  }

  #[test]
  fn scanner_scan_unterminated_string_literal() {
    init_scanner("'test", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
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
    });
  }

  #[test]
  fn scanner_scan_invalid_ascii_escaped_string_literal2() {
    init_scanner("'\\x61_foo_\\x2_bar_\\x63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
    });
  }

  #[test]
  fn scanner_scan_invalid_ascii_escaped_string_literal3() {
    init_scanner("'\\x61_foo_\\x62_bar_\\x-63_baz'", None, |mut scanner| {
      assert_eq!(scanner.next(), Token::Invalid);
    });
  }
}
