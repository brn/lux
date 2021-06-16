#[cfg(test)]
mod scanner_test {
  use super::super::error_reporter::*;
  use super::super::parser_state::ParserStateStack;
  use super::super::scanner::*;
  use crate::context::LuxContext;
  use itertools::Itertools;
  use std::vec::Vec;

  fn scan(source: &str) -> Vec<Token> {
    let mut error_reporter = ErrorReporter::new();
    let context = LuxContext::new_until_internal_object_records();
    let mut parser_state_stack = ParserStateStack::new();
    let mut scanner = Scanner::new(context, source, &mut error_reporter, &mut parser_state_stack);
    let mut vec = Vec::new();
    loop {
      let token = scanner.next();
      vec.push(token);
      if token == Token::End || token == Token::Invalid {
        return vec;
      }
    }
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
    let base_iter = Token::values().filter(|token| !token.is_pseudo_token() && !token.is_state_affected());
    let source: String = base_iter.clone().map(|token| token.symbol()).intersperse(" ").collect();
    let results = scan(&source);
    let mut expected = base_iter.clone().collect::<Vec<_>>();
    expected.push(&Token::End);
    for (i, token) in results.iter().enumerate() {
      assert_eq!(token, expected[i]);
    }
  }
}
