use super::ast::*;
use super::parser::*;
use super::parser_range::*;
use super::parser_test_utils::*;
use super::source::*;
use super::source_position::*;
use crate::context::*;

bitflags! {
  struct SkipParserTestOption: u8 {
    const NONE = 0;
    const ASYNC = 1;
    const GENERATOR = 2;
  }
}

fn parse_nested(source: &str, expectations: TestableAst, test_opt: SkipParserTestOption) {
  let context = LuxContext::new_until_internal_object_records();
  let wrapped_source = format!(
    "{}function {}root() {{{}}}",
    if test_opt.intersects(SkipParserTestOption::ASYNC) {
      "async "
    } else {
      ""
    },
    if test_opt.intersects(SkipParserTestOption::GENERATOR) {
      "*"
    } else {
      ""
    },
    source
  );
  let source = Source::new(context, "anonymous", &wrapped_source);
  let mut parser = Parser::new(context, source.clone(), Default::default());

  match parser.parse() {
    Ok(ast) => match Node::<Function>::try_from(Stmt::try_from(ast).unwrap()) {
      Ok(function) => {
        parse_inner_body(context, function, source, &(expectations.to_string()));
      }
      Err(e) => {
        panic!("Failed to try_from for ast {:?}", ast);
      }
    },
    Err(e) => {
      panic!("{}", e.error_message());
    }
  }
}

fn parse_inner_body(context: impl Context, function: Node<Function>, source: Source, expectations: &str) {
  let mut parser = Parser::new(
    context,
    source.clone(),
    ParserOptionBuilder {
      parser_range: ParserRange::new(function.function_body_start(), function.function_body_end()),
      ..Default::default()
    }
    .build(),
  );
  match parser.parse() {
    Ok(ast) => {
      if let Ok(function) = Node::<Function>::try_from(Stmt::try_from(ast).unwrap()) {
        parse_inner_body(context, function, source, expectations);
      } else {
        assert_eq!(ast.to_string_tree(), expectations);
      }
    }
    Err(e) => {
      parser.print_stack_trace();
      panic!(e.error_message());
    }
  };
}

#[test]
fn parse_simple_expr() {
  parse_nested(
    "a + b",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::NONE,
  );
}

#[test]
fn parse_with_async() {
  parse_nested(
    "a + b",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC,
  );
}

#[test]
fn parse_with_generator() {
  parse_nested(
    "a + b",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::GENERATOR,
  );
}

#[test]
fn parse_with_async_generator() {
  parse_nested(
    "a + b",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC | SkipParserTestOption::GENERATOR,
  );
}

#[test]
fn parse_multi_level_inner_function() {
  parse_nested(
    "function innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::NONE,
  );
}

#[test]
fn parse_multi_level_inner_function_with_outer_async() {
  parse_nested(
    "function innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC,
  );
}

#[test]
fn parse_multi_level_inner_async_function_with_outer_async() {
  parse_nested(
    "async function innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC,
  );
}

#[test]
fn parse_multi_level_inner_function_with_outer_generator_async() {
  parse_nested(
    "function innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC | SkipParserTestOption::GENERATOR,
  );
}

#[test]
fn parse_multi_level_inner_async_function_with_outer_async_generator() {
  parse_nested(
    "async function innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC | SkipParserTestOption::GENERATOR,
  );
}

#[test]
fn parse_multi_level_inner_async_generator_function_with_outer_async_generator() {
  parse_nested(
    "async function *innerRoot(){a + b}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC | SkipParserTestOption::GENERATOR,
  );
}

#[test]
fn parse_multi_level2_inner_async_generator_function_with_outer_async_generator() {
  parse_nested(
    "async function *innerRoot(){function innerInner(){a + b}}",
    stmt!(
      pos!(0, 0),
      binary!("OpPlus", pos!(0, 0), ident!("a", pos!(0, 0)), ident!("b", pos!(4, 0)))
    ),
    SkipParserTestOption::ASYNC | SkipParserTestOption::GENERATOR,
  );
}
