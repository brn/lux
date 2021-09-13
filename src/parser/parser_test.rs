#[cfg(test)]
mod parser_test {
  use super::super::ast::*;
  use super::super::error_reporter::*;
  use super::super::parser::*;
  use super::super::source::*;
  use super::super::source_position::*;
  use super::super::token::Token;
  use super::*;
  use crate::context::*;
  use crate::utility::*;
  use paste::paste;
  use std::rc::Rc;

  struct TestableAst {
    name: String,
    attr: String,
    pos: RuntimeSourcePosition,
    children: Vec<Box<TestableAst>>,
  }

  impl TestableAst {
    fn ast(name: &str, attr: &str, pos: RuntimeSourcePosition) -> Self {
      return TestableAst {
        name: name.to_string(),
        attr: attr.to_string(),
        pos,
        children: Vec::new(),
      };
    }

    fn push(&mut self, ast: TestableAst) {
      self.children.push(Box::new(ast));
    }

    fn to_string(&self) -> String {
      let mut indent = "".to_string();
      return self.to_string_internal(&mut indent);
    }

    fn to_string_internal(&self, indent: &mut String) -> String {
      if self.name.len() == 0 {
        return "".to_string();
      }

      let has_attr = self.attr.len() > 0;
      let mut buf = format!("{}[{}", indent, self.name);
      if has_attr {
        let n = format!("{} {}", buf, self.attr);
        buf = n;
      }
      let n = format!("{} {}]\n", buf, self.pos.to_string());
      buf = n;
      let mut ni = format!("  {}", indent);
      for child in self.children.iter() {
        let n = format!("{}{}", buf, child.to_string_internal(&mut ni));
        buf = n;
      }
      return buf;
    }
  }

  impl std::fmt::Debug for TestableAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      return write!(f, "{}", self.to_string());
    }
  }

  macro_rules! s_pos {
    ($start_col:expr, $end_col:expr, $start_line:expr, $end_line:expr) => {
      SourcePosition::with(Some($start_col), Some($end_col), Some($start_line), Some($end_line))
    };
  }

  macro_rules! pos {
    ($col:expr, $line:expr) => {
      RuntimeSourcePosition::new($col, $line)
    };
  }

  macro_rules! ast {
    ($name:expr, $attr:expr, $pos:expr) => {
      TestableAst::ast($name, $attr, $pos)
    };
  }

  macro_rules! ast_with_children {
    ($name:expr, $attr:expr, $pos:expr, $($asts:expr),*$(,)*) => {{
      let mut ast = TestableAst::ast($name, $attr, $pos);
      $(
        ast.push($asts);
      )*
      ast
    }};
  }

  macro_rules! void {
    () => {
      ast!("", "", pos!(0, 0));
    };
  }

  macro_rules! stmts {
    ($pos:expr, $($asts:expr),*$(,)*) => {
      ast_with_children!("Statements", "", $pos, $($asts,)*)
    };
    ($pos:expr) => {
      ast!("Statements", "", $pos)
    }
  }

  macro_rules! stmt {
    ($pos:expr, $expr:expr) => {
      ast_with_children!("Statement", "", $pos, $expr)
    };
  }

  macro_rules! exprs {
    ($pos:expr, $($asts:expr),*$(,)*) => {
      ast_with_children!("Expressions", "", $pos, $($asts,)*)
    };
    ($pos:expr) => {
      ast!("Expressions", "", $pos)
    }
  }

  macro_rules! scope {
    (@opaque @strict $children_scope_len:expr, $has_parent:expr) => {
      scope!("opaque", true, $children_scope_len, $has_parent)
    };
    (@transparent @strict $children_scope_len:expr, $has_parent:expr) => {
      scope!("transparent", true, $children_scope_len, $has_parent)
    };
    (@opaque $children_scope_len:expr, $has_parent:expr) => {
      scope!("opaque", false, $children_scope_len, $has_parent)
    };
    (@transparent $children_scope_len:expr, $has_parent:expr) => {
      scope!("transparent", false, $children_scope_len, $has_parent)
    };
    (@opaque $is_strict:expr, $children_scope_len:expr, $has_parent:expr) => {
      scope!("opaque", $is_strict, $children_scope_len, $has_parent)
    };
    (@transparent $is_strict:expr, $children_scope_len:expr, $has_parent:expr) => {
      scope!("transparent", $is_strict, $children_scope_len, $has_parent)
    };
    ($scope_type:expr, $is_strict:expr, $children_scope_len:expr, $has_parent:expr) => {
      format!(
        "Scope {{ type = {} strict_mode = {} children = {} has_parent = {} }}",
        $scope_type, $is_strict, $children_scope_len, $has_parent
      )
    };
  }

  macro_rules! fnexpr {
    ($pos:expr, $type:expr, $start:expr, $end:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {} body_start = {} body_end = {} {}", $type, $start, $end, $scope);
      ast_with_children!("FunctionExpression", &attr, $pos, $($asts,)*)
    }}
  }

  macro_rules! afnexpr {
    ($pos:expr, $type:expr, $start:expr, $end:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {} async body_start = {} body_end = {} {}", $type, $start, $end, $scope);
      ast_with_children!("FunctionExpression", &attr, $pos, $($asts,)*)
    }}
  }

  macro_rules! getset_fnexpr {
    ($pos:expr, $type:expr, $accessor:expr, $start:expr, $end:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {} {} body_start = {} body_end = {} {}", $type, $accessor, $start, $end, $scope);
      ast_with_children!("FunctionExpression", &attr, $pos, $($asts,)*)
    }}
  }

  macro_rules! unary {
    ($operand:expr, $position:expr, $pos:expr, $expr:expr) => {{
      let attr = format!("operand = {} position = {}", $operand, $position);
      ast_with_children!("UnaryExpression", &attr, $pos, $expr)
    }};
  }

  macro_rules! binary {
    ($operand:expr, $pos:expr, $left:expr, $right:expr) => {{
      let attr = format!("operand = {}", $operand);
      ast_with_children!("BinaryExpression", &attr, $pos, $left, $right)
    }};
  }

  macro_rules! callexpr {
    ($receiver:expr, $pos:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("receiver = {}", $receiver);
      ast_with_children!("CallExpression", &attr, $pos, $($asts,)*)
    }};
  }

  macro_rules! newexpr {
    ($pos:expr, $callee:expr) => {{
      ast_with_children!("NewExpression", "", $pos, $callee)
    }};
  }

  macro_rules! prop {
    ($type:expr, $pos:expr, $callee:expr, $prop:expr) => {{
      let attr = format!("property_access = {}", $type);
      ast_with_children!("PropertyAccessExpression", &attr, $pos, $callee, $prop)
    }};
  }

  macro_rules! lit {
    ($attr:expr, $pos:expr) => {{
      ast!("Literal", $attr, $pos)
    }};
  }

  macro_rules! arraylit {
    ($has_spread:expr, $pos:expr, $($asts:expr),*$(,)*) => {{
      let attr = if $has_spread {
        "type = ArrayLiteral spread = true"
      } else {
        "type = ArrayLiteral"
      };
      ast_with_children!("StructuralLiteral", attr, $pos, $($asts,)*)
    }};
  }

  bitflags! {
    struct ObjectLitType: u8 {
      const NONE = 0;
      const HAS_ACCESSOR = 1;
      const HAS_GENERATOR = 2;
      const HAS_SPREAD = 4;
    }
  }
  macro_rules! objectlit {
    ($lit_type:expr, $pos:expr, $($asts:expr),*$(,)*) => {{
      let mut buf = "type = ObjectLiteral".to_string();
      if $lit_type.contains(ObjectLitType::HAS_ACCESSOR) {
        buf = format!("{} accessor = true", buf);
      }
      if $lit_type.contains(ObjectLitType::HAS_GENERATOR) {
        buf = format!("{} generator = true", buf);
      }
      if $lit_type.contains(ObjectLitType::HAS_SPREAD) {
        buf = format!("{} spread = true", buf);
      }
      ast_with_children!("StructuralLiteral", &buf, $pos, $($asts,)*)
    }};
  }

  macro_rules! object_props {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("ObjectPropertyExpression", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! number {
    ($val:expr, $pos:expr) => {{
      let attr = format!("type = NumericLiteral value = {}", $val);
      ast!("Literal", &attr, $pos)
    }};
  }

  macro_rules! octal {
    ($val:expr, $pos:expr) => {{
      let attr = format!("type = ImplicitOctalLiteral value = {}", $val);
      ast!("Literal", &attr, $pos)
    }};
  }

  macro_rules! str {
    ($val:expr, $pos:expr) => {{
      let attr = format!("type = StringLiteral value = {}", $val);
      ast!("Literal", &attr, $pos)
    }};
  }

  macro_rules! tmpl {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("TemplateLiteral", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! ident {
    ($val:expr, $pos:expr) => {{
      let attr = format!("type = Identifier value = {}", $val);
      ast!("Literal", &attr, $pos)
    }};
  }

  macro_rules! cond {
    ($pos:expr, $cond:expr, $then:expr, $else:expr) => {{
      ast_with_children!("ConditionalExpression", "", $pos, $cond, $then, $else)
    }};
  }

  macro_rules! empty {
    ($pos:expr) => {{
      ast!("Empty", "", $pos)
    }};
  }

  type Expectations<'a> = [&'a str; 3];

  fn parse_test<'a>(
    env_list: &[(Option<&'a str>, &'a str); 3],
    code: &str,
    expectations: &'a Expectations,
    parser_option: ParserOption,
  ) {
    for i in 0..3 {
      let env = env_list[i];
      if env.0.is_none() {
        continue;
      }
      let str = format!("{}{}{};PARSER_SENTINEL", env.0.unwrap(), code, env.1);
      let context = LuxContext::new_until_internal_object_records();
      let source = Source::new(context, "anonymouse", &str);
      let mut parser = Parser::new(context, source.clone(), parser_option.clone());
      match parser.parse(ParserType::Script) {
        Ok(ast) => {
          let tree = ast.to_string_tree();
          match compare_node(&str, &tree, expectations[i]) {
            Err(em) => {
              println!("{}", em);
              parser.print_stack_trace();
            }
            _ => {}
          }
        }
        Err(err) => {
          parser.print_stack_trace();
          println!("code is {}", str);
          panic!(err.error_message());
        }
      }
    }
  }

  fn syntax_error_test<'a>(
    env_list: &[(Option<&str>, &str); 3],
    code: &str,
    source_positions: &[&SourcePosition],
    show_error: bool,
  ) {
    let parser_options = [ParserOption::new(), ParserOption::with(true)];
    for opt in (&parser_options).iter() {
      for i in 0..3 {
        let env = env_list[i];
        if env.0.is_some() {
          let str = format!("{}{}{};PARSER_SENTINEL", env.0.unwrap(), code, env.1);
          let context = LuxContext::new_until_internal_object_records();
          let source = Source::new(context, "anonymouse", &str);
          let mut parser = Parser::new(context, source.clone(), opt.clone());
          let ast = parser.parse(ParserType::Script);
          let m = format!("Code {} not generate error", code);
          if !ast.is_err() {
            parser.print_stack_trace();
            println!("code is {}", str);
            assert!(ast.is_err(), m);
          }
          assert!(parser.error_reporter().has_pending_error());
          for ed in parser.error_reporter().pending_errors().iter() {
            match compare_position(ed.source_position(), source_positions[i]) {
              Err(err) => {
                let e = ed.clone();
                parser.print_stack_trace();
                println!("error is {:?}", *e);
                println!("code is {}", str);
                panic!(err);
              }
              _ => {}
            };
          }
          if show_error {
            parser.error_reporter().print_errors();
          }
        }
      }
    }
  }

  fn wrap_with_function_expr<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    expr_size: u64,
    ast_builder: F,
    scope_count: u32,
    parser_option: ParserOption,
    mut before_line_break_col_count: u64,
  ) -> (F, String) {
    let buf = String::new();
    let original_blb_cc = before_line_break_col_count;
    if before_line_break_col_count > 0 {
      before_line_break_col_count += 15;
    }

    let base_position = 10;
    let func_exit = (14 + expr_size) - before_line_break_col_count + 2;
    let sentinel_start = if original_blb_cc > 0 {
      (expr_size - original_blb_cc) + 2
    } else {
      func_exit + 1
    };

    let ast = stmts!(
      pos!(0, 0),
      stmt!(
        pos!(0, 0),
        unary!(
          "OpNot",
          "Pre",
          pos!(0, 0),
          fnexpr!(
            pos!(1, 0),
            "Function",
            0,
            0,
            scope!(@opaque scope_count, true),
            ident!("X", pos!(base_position, 0)),
            exprs!(pos!(base_position + 1, 0)),
            stmts!(
              pos!(base_position + 4, 0),
              stmt!(
                pos!(base_position + 5, 0),
                ast_builder((base_position + 5, 0, false, !parser_option.disable_skip_parser()))
              )
            ),
          )
        )
      ),
      stmt!(
        pos!(sentinel_start, 0),
        ident!("PARSER_SENTINEL", pos!(sentinel_start, 0))
      )
    );

    return (ast_builder, ast.to_string());
  }

  fn single_expression_test<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str) {
    let ast_b = single_expression_test_with_options(ast_builder, value, ParserOption::new(), 0, false, 0);
    single_expression_test_with_options(ast_b, value, ParserOption::with(true), 0, false, 0);
  }

  fn single_expression_test_with_scope<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    scope_count: u32,
  ) {
    let ast_b = single_expression_test_with_options(ast_builder, value, ParserOption::new(), scope_count, false, 0);
    single_expression_test_with_options(ast_b, value, ParserOption::with(true), scope_count, false, 0);
  }

  fn single_expression_test_with_options<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    parser_option: ParserOption,
    scope_count: u32,
    is_skip_strict_mode: bool,
    before_line_break_count: u64,
  ) -> F {
    let env: [(Option<&str>, &str); 3] = [
      (Some(""), ""),
      (
        if is_skip_strict_mode {
          None
        } else {
          Some("'use strict';")
        },
        "",
      ),
      (Some("!function X() {"), "}"),
    ];

    let size = value.len() as u64;
    let product1 = ast_builder((0, 0, false, !parser_option.disable_skip_parser()));
    let (ast_b, f) = wrap_with_function_expr(
      size,
      ast_builder,
      scope_count,
      parser_option.clone(),
      before_line_break_count,
    );
    let normal = stmts!(
      pos!(0, 0),
      stmt!(pos!(0, 0), product1),
      stmt!(
        pos!((size + 1) - before_line_break_count, 0),
        ident!("PARSER_SENTINEL", pos!((size + 1) - before_line_break_count, 0))
      )
    )
    .to_string();

    let sentinel_start_col = if before_line_break_count > 0 {
      size - before_line_break_count + 1
    } else {
      (size + 14) - before_line_break_count
    };
    let product2 = ast_b((13, 0, true, !parser_option.disable_skip_parser()));
    let strict = stmts!(
      pos!(13, 0),
      stmt!(pos!(13, 0), product2),
      stmt!(
        pos!(sentinel_start_col, 0),
        ident!("PARSER_SENTINEL", pos!(sentinel_start_col, 0))
      )
    )
    .to_string();

    let expected: Expectations = [&normal, &strict, &f];
    parse_test(&env, value, &expected, parser_option);
    return ast_b;
  }

  const BASIC_ENV: [(Option<&str>, &str); 3] = [
    (Some(""), ""),
    (Some("'use strict';"), ""),
    (Some("!function X() {"), ""),
  ];

  #[test]
  fn parse_single_decimal_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("1", pos!(col, line));
      },
      "1",
    );
  }

  #[test]
  fn parse_multi_decimal_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("1024", pos!(col, line));
      },
      "1024",
    );
  }

  #[test]
  fn parse_multi_decimal_exponent_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("130000000000", pos!(col, line));
      },
      "13e+10",
    );
  }

  #[test]
  fn parse_float_leading_zero_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("0.12", pos!(col, line));
      },
      "0.12",
    );
  }

  #[test]
  fn parse_float_not_leading_zero_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("0.12", pos!(col, line));
      },
      ".12",
    );
  }

  #[test]
  fn parse_hex_decimal_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("12379813812177893000", pos!(col, line));
      },
      "0xabcdef1234567890",
    );
  }

  #[test]
  fn parse_binary_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("21", pos!(col, line));
      },
      "0b010101",
    );
  }

  #[test]
  fn parse_octal_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("511", pos!(col, line));
      },
      "0o777",
    );
  }

  #[test]
  fn parse_implicit_octal_literal_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return octal!("511", pos!(col, line));
      },
      "0777",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_decimal_leading_zero_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return number!("7778", pos!(col, line));
      },
      "07778",
    );
  }

  #[test]
  fn numeric_literal_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "0x_",
      &[&s_pos!(0, 3, 0, 0), &s_pos!(13, 16, 0, 0), &s_pos!(15, 18, 0, 0)],
      false,
    );

    syntax_error_test(
      &BASIC_ENV,
      "0b_",
      &[&s_pos!(0, 3, 0, 0), &s_pos!(13, 16, 0, 0), &s_pos!(15, 18, 0, 0)],
      false,
    );

    syntax_error_test(
      &BASIC_ENV,
      "0o_",
      &[&s_pos!(0, 3, 0, 0), &s_pos!(13, 16, 0, 0), &s_pos!(15, 18, 0, 0)],
      false,
    );

    syntax_error_test(
      &BASIC_ENV,
      "13e",
      &[&s_pos!(0, 3, 0, 0), &s_pos!(13, 16, 0, 0), &s_pos!(15, 18, 0, 0)],
      false,
    );

    syntax_error_test(
      &BASIC_ENV,
      "13e+",
      &[&s_pos!(0, 4, 0, 0), &s_pos!(13, 17, 0, 0), &s_pos!(15, 19, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_implicit_octal_error_test() {
    let env = [(None, ""), (None, ""), (Some("'use strict';"), "")];
    syntax_error_test(
      &env,
      "0777",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_single_quote_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test", pos!(col, line));
      },
      "'test'",
    );
  }

  #[test]
  fn parse_double_quote_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test", pos!(col, line));
      },
      "\"test\"",
    );
  }

  #[test]
  fn parse_single_quote_escaped_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test 'value", pos!(col, line));
      },
      "'test \\'value'",
    );
  }

  #[test]
  fn parse_double_quote_escaped_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test \"value", pos!(col, line));
      },
      "\"test \\\"value\"",
    );
  }

  #[test]
  fn parse_single_quote_backslash_escaped_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test\\ value", pos!(col, line));
      },
      "'test\\\\ value'",
    );
  }

  #[test]
  fn parse_double_quote_backslash_escaped_string_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("test\\ value", pos!(col, line));
      },
      "\"test\\\\ value\"",
    );
  }

  #[test]
  fn parse_string_literal_unicode_escape_sequence_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("A_B_C_D", pos!(col, line));
      },
      "'\\u0041_\\u0042_\\u0043_\\u0044'",
    );
  }

  #[test]
  fn parse_string_literal_ascii_escape_sequence_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return str!("A_B_C_D", pos!(col, line));
      },
      "'\\x41_\\x42_\\x43_\\x44'",
    );
  }

  #[test]
  fn parse_valid_octal_like_escape_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(pos!(col, line), str!("a_\0", pos!(col + 1, line)));
      },
      "`a_\\0`",
    );
  }

  #[test]
  fn parse_template_literal_without_interpolation_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(pos!(col, line), str!("test", pos!(col + 1, line)));
      },
      "`test`",
    );
  }

  #[test]
  fn parse_template_literal_escaped_without_interpolation_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(pos!(col, line), str!("test${aaa}", pos!(col + 1, line)));
      },
      "`test\\${aaa}`",
    );
  }

  #[test]
  fn parse_template_literal_linebreak_without_interpolation_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return tmpl!(pos!(col, line), str!("test\ntest", pos!(col + 1, line)));
      },
      "`test\ntest`",
      ParserOption::new(),
      0,
      false,
      6,
    );
  }

  #[test]
  fn parse_template_literal_with_empty_suffix_interpolation_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(
          pos!(col, line),
          str!("test", pos!(col + 1, line)),
          ident!("test", pos!(col + 7, line))
        );
      },
      "`test${test}`",
    );
  }

  #[test]
  fn parse_template_literal_with_suffix_interpolation_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(
          pos!(col, line),
          str!("foo", pos!(col + 1, line)),
          ident!("bar", pos!(col + 6, line)),
          str!("baz", pos!(col + 10, line)),
        );
      },
      "`foo${bar}baz`",
    );
  }

  #[test]
  fn parse_template_literal_with_many_suffix_interpolation_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(
          pos!(col, line),
          str!("foo", pos!(col + 1, line)),
          ident!("bar", pos!(col + 6, line)),
          str!("baz", pos!(col + 10, line)),
          number!("100", pos!(col + 15, line)),
          unary!(
            "OpPlus",
            "Pre",
            pos!(col + 21, line),
            ident!("foo", pos!(col + 22, line))
          )
        );
      },
      "`foo${bar}baz${100}${+foo}`",
    );
  }

  #[test]
  fn parse_nested_template_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return tmpl!(
          pos!(col, line),
          str!("foo", pos!(col + 1, line)),
          tmpl!(
            pos!(col + 6, line),
            str!("foo", pos!(col + 7, line)),
            ident!("bar", pos!(col + 12, line)),
          ),
        );
      },
      "`foo${`foo${bar}`}`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_hex_escape_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\xzzz", pos!(col + 2, line)))
        );
      },
      "a`\\xzzz`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\uzzz", pos!(col + 2, line)))
        );
      },
      "a`\\uzzz`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\u{xxx}", pos!(col + 2, line)))
        );
      },
      "a`\\u{xxx}`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test_3() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\u{xxx", pos!(col + 2, line)))
        );
      },
      "a`\\u{xxx`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test_4() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\u6", pos!(col + 2, line)))
        );
      },
      "a`\\u6`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test_5() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\u66", pos!(col + 2, line)))
        );
      },
      "a`\\u66`",
    );
  }

  #[test]
  fn parse_tagged_template_literal_special_valid_unicode_escape_test_6() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          tmpl!(pos!(col + 1, line), str!("\\u665", pos!(col + 2, line)))
        );
      },
      "a`\\u665`",
    );
  }

  #[test]
  fn parse_unterminated_string_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "'test",
      &[&s_pos!(0, 21, 0, 0), &s_pos!(13, 34, 0, 0), &s_pos!(15, 36, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_unterminated_string_error_with_linebreak_test() {
    syntax_error_test(
      &BASIC_ENV,
      "'test\\n",
      &[&s_pos!(0, 23, 0, 0), &s_pos!(13, 36, 0, 0), &s_pos!(15, 38, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_invalid_unicode_sequence_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "'\\u0041_\\u0042_\\u043_\\u0044'",
      &[&s_pos!(15, 21, 0, 0), &s_pos!(28, 34, 0, 0), &s_pos!(30, 36, 0, 0)],
      false,
    )
  }

  #[test]
  fn template_literal_not_allow_octal_escape_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "`\\071`",
      &[&s_pos!(1, 4, 0, 0), &s_pos!(14, 17, 0, 0), &s_pos!(16, 19, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_unary_expression_plus_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpPlus", "Pre", pos!(col, line), number!("1", pos!(col + 1, line)));
      },
      "+1",
    );
  }

  #[test]
  fn parse_unary_expression_minus_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpMinus", "Pre", pos!(col, line), number!("1", pos!(col + 1, line)));
      },
      "-1",
    );
  }

  #[test]
  fn parse_unary_expression_not_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpNot", "Pre", pos!(col, line), number!("1", pos!(col + 1, line)));
      },
      "!1",
    );
  }

  #[test]
  fn parse_unary_expression_tilde_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpTilde", "Pre", pos!(col, line), number!("1", pos!(col + 1, line)));
      },
      "~1",
    );
  }

  #[test]
  fn parse_unary_expression_delete_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("Delete", "Pre", pos!(col, line), number!("1", pos!(col + 7, line)));
      },
      "delete 1",
    );
  }

  #[test]
  fn parse_unary_expression_typeof_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("Typeof", "Pre", pos!(col, line), number!("1", pos!(col + 7, line)));
      },
      "typeof 1",
    );
  }

  #[test]
  fn parse_unary_expression_void_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("Void", "Pre", pos!(col, line), number!("1", pos!(col + 5, line)));
      },
      "void 1",
    );
  }

  #[test]
  fn parse_unary_expression_increments_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpIncrement", "Pre", pos!(col, line), ident!("a", pos!(col + 2, line)));
      },
      "++a",
    );
  }

  #[test]
  fn parse_unary_expression_increments_pre_with_eval_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!(
          "OpIncrement",
          "Pre",
          pos!(col, line),
          ident!("eval", pos!(col + 2, line))
        );
      },
      "++eval",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_increments_pre_with_arguments_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!(
          "OpIncrement",
          "Pre",
          pos!(col, line),
          ident!("arguments", pos!(col + 2, line))
        );
      },
      "++arguments",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_pre_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Pre", pos!(col, line), ident!("a", pos!(col + 2, line)));
      },
      "--a",
    );
  }

  #[test]
  fn parse_unary_expression_decrements_pre_with_yield_keyword_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!(
          "OpDecrement",
          "Pre",
          pos!(col, line),
          ident!("yield", pos!(col + 2, line))
        );
      },
      "--yield",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_pre_with_await_keyword_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!(
          "OpDecrement",
          "Pre",
          pos!(col, line),
          ident!("await", pos!(col + 2, line))
        );
      },
      "--await",
    );
  }

  #[test]
  fn parse_unary_expression_increments_post_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpIncrement", "Post", pos!(col, line), ident!("a", pos!(col, line)));
      },
      "a++",
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Post", pos!(col, line), ident!("a", pos!(col, line)));
      },
      "a--",
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_with_yield_keyword_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Post", pos!(col, line), ident!("yield", pos!(col, line)));
      },
      "yield--",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_with_await_keyword_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Post", pos!(col, line), ident!("await", pos!(col, line)));
      },
      "await--",
    );
  }

  #[test]
  fn parse_unary_expression_increments_post_with_eval_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!("OpIncrement", "Post", pos!(col, line), ident!("eval", pos!(col, line)));
      },
      "eval++",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_with_arguments_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!(
          "OpDecrement",
          "Post",
          pos!(col, line),
          ident!("arguments", pos!(col, line))
        );
      },
      "arguments--",
      ParserOption::new(),
      0,
      true,
      0,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_early_error_test() {
    let env = [(None, ""), (None, ""), (Some("'use strict';"), "")];
    syntax_error_test(
      &env,
      "eval++",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );

    syntax_error_test(
      &env,
      "arguments++",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 22, 0, 0)],
      false,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_yield_early_error_test() {
    let env = [(Some("!function *x() {"), "}"), (None, ""), (None, "")];
    syntax_error_test(
      &env,
      "++yield",
      &[&s_pos!(18, 23, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_await_early_error_test() {
    let env = [(Some("!async function x() {"), "}"), (None, ""), (None, "")];
    syntax_error_test(
      &env,
      "++await",
      &[&s_pos!(23, 28, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_new_expression_no_args_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(pos!(col, line), ident!("X", pos!(col + 4, line)));
      },
      "new X",
    );
  }

  #[test]
  fn parse_new_expression_with_args_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(
          pos!(col, line),
          callexpr!(
            "Expr",
            pos!(col + 4, line),
            ident!("X", pos!(col + 4, line)),
            exprs!(pos!(col + 5, line), number!("1", pos!(col + 6, line)))
          )
        );
      },
      "new X(1)",
    );
  }

  #[test]
  fn parse_new_expression_with_props_call_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(
          pos!(col, line),
          callexpr!(
            "Expr",
            pos!(col + 4, line),
            prop!(
              "dot",
              pos!(col + 4, line),
              ident!("X", pos!(col + 4, line)),
              ident!("a", pos!(col + 6, line))
            ),
            exprs!(pos!(col + 7, line), number!("1", pos!(col + 8, line)))
          )
        );
      },
      "new X.a(1)",
    );
  }

  #[test]
  fn parse_new_expression_with_element_call_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(
          pos!(col, line),
          callexpr!(
            "Expr",
            pos!(col + 4, line),
            prop!(
              "element",
              pos!(col + 4, line),
              ident!("X", pos!(col + 4, line)),
              str!("a", pos!(col + 6, line))
            ),
            exprs!(pos!(col + 10, line), number!("1", pos!(col + 11, line)))
          )
        );
      },
      "new X['a'](1)",
    );
  }

  #[test]
  fn parse_new_expression_with_props_chain_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(
          pos!(col, line),
          callexpr!(
            "Expr",
            pos!(col + 4, line),
            prop!(
              "dot",
              pos!(col + 4, line),
              prop!(
                "dot",
                pos!(col + 4, line),
                ident!("a", pos!(col + 4, line)),
                ident!("b", pos!(col + 6, line))
              ),
              ident!("c", pos!(col + 8, line))
            ),
            exprs!(pos!(col + 9, line),)
          )
        );
      },
      "new a.b.c()",
    );
  }

  #[test]
  fn template_literal_not_allowed_after_op_chain_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "a?.`a`",
      &[&s_pos!(3, 4, 0, 0), &s_pos!(16, 17, 0, 0), &s_pos!(18, 19, 0, 0)],
      false,
    );
  }

  #[test]
  fn template_literal_not_allowed_after_op_chain_early_error_test_2() {
    syntax_error_test(
      &BASIC_ENV,
      "a?.a?.`a`",
      &[&s_pos!(6, 7, 0, 0), &s_pos!(19, 20, 0, 0), &s_pos!(21, 22, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_new_expression_with_props_and_element_chain_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return newexpr!(
          pos!(col, line),
          callexpr!(
            "Expr",
            pos!(col + 4, line),
            prop!(
              "dot",
              pos!(col + 4, line),
              prop!(
                "element",
                pos!(col + 4, line),
                ident!("a", pos!(col + 4, line)),
                str!("b", pos!(col + 6, line))
              ),
              ident!("c", pos!(col + 11, line))
            ),
            exprs!(pos!(col + 12, line),)
          )
        );
      },
      "new a['b'].c()",
    );
  }

  #[test]
  fn parse_new_expression_with_tagged_template_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Template",
          pos!(col, line),
          prop!(
            "dot",
            pos!(col, line),
            newexpr!(
              pos!(col, line),
              callexpr!(
                "Expr",
                pos!(col + 4, line),
                ident!("X", pos!(col + 4, line)),
                exprs!(pos!(col + 5, line))
              )
            ),
            ident!("a", pos!(col + 8, line))
          ),
          tmpl!(pos!(col + 9, line), str!("test", pos!(col + 10, line)))
        );
      },
      "new X().a`test`",
    );
  }

  #[test]
  fn parse_new_expression_with_coalesce_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return callexpr!(
          "Expr",
          pos!(col, line),
          prop!(
            "op_chaining",
            pos!(col, line),
            newexpr!(
              pos!(col, line),
              callexpr!(
                "Expr",
                pos!(col + 4, line),
                ident!("X", pos!(col + 4, line)),
                exprs!(pos!(col + 5, line))
              )
            ),
            ident!("a", pos!(col + 9, line))
          ),
          exprs!(pos!(col + 10, line))
        );
      },
      "new X()?.a()",
    );
  }

  #[test]
  fn parse_exponentiation_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpPow",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 ** 1",
    );
  }

  #[test]
  fn parse_multiplicative_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpMul",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 * 1",
    );
  }

  #[test]
  fn parse_division_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpDiv",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 / 1",
    );
  }

  #[test]
  fn parse_addition_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpPlus",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 + 1",
    );
  }

  #[test]
  fn parse_subtraction_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpMinus",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 - 1",
    );
  }

  #[test]
  fn parse_shift_left_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpShl",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 << 1",
    );
  }

  #[test]
  fn parse_shift_right_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpShr",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 >> 1",
    );
  }

  #[test]
  fn parse_u_shift_right_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpUShr",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 6, line))
        );
      },
      "1 >>> 1",
    );
  }

  #[test]
  fn parse_in_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "In",
          pos!(col, line),
          str!("a", pos!(col, line)),
          ident!("v", pos!(col + 7, line))
        );
      },
      "'a' in v",
    );
  }

  #[test]
  fn parse_instanceof_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "Instanceof",
          pos!(col, line),
          ident!("a", pos!(col, line)),
          ident!("v", pos!(col + 13, line))
        );
      },
      "a instanceof v",
    );
  }

  #[test]
  fn parse_greater_than_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpGreaterThan",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("0", pos!(col + 4, line))
        );
      },
      "1 > 0",
    );
  }

  #[test]
  fn parse_greater_than_or_eq_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpGreaterThanOrEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("0", pos!(col + 5, line))
        );
      },
      "1 >= 0",
    );
  }

  #[test]
  fn parse_less_than_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpLessThan",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("0", pos!(col + 4, line))
        );
      },
      "1 < 0",
    );
  }

  #[test]
  fn parse_less_than_or_eq_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpLessThanOrEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("0", pos!(col + 5, line))
        );
      },
      "1 <= 0",
    );
  }

  #[test]
  fn parse_equal_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 == 1",
    );
  }

  #[test]
  fn parse_strict_equal_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpStrictEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 6, line))
        );
      },
      "1 === 1",
    );
  }

  #[test]
  fn parse_not_equal_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpNotEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 != 1",
    );
  }

  #[test]
  fn parse_strict_not_equal_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpStrictNotEq",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 6, line))
        );
      },
      "1 !== 1",
    );
  }

  #[test]
  fn parse_bitwise_and_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAnd",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 & 1",
    );
  }

  #[test]
  fn parse_bitwise_or_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpOr",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 | 1",
    );
  }

  #[test]
  fn parse_bitwise_xor_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpXor",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 4, line))
        );
      },
      "1 ^ 1",
    );
  }

  #[test]
  fn parse_logical_and_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpLogicalAnd",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 && 1",
    );
  }

  #[test]
  fn parse_logical_or_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpLogicalOr",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 || 1",
    );
  }

  #[test]
  fn parse_null_coalesce_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpNullCoalescing",
          pos!(col, line),
          number!("1", pos!(col, line)),
          number!("1", pos!(col + 5, line))
        );
      },
      "1 ?? 1",
    );
  }

  #[test]
  fn parser_operator_priority_test_1() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpOr",
          pos!(col, line),
          binary!(
            "OpMinus",
            pos!(col, line),
            ident!("a", pos!(col, line)),
            binary!(
              "OpMul",
              pos!(col + 4, line),
              ident!("b", pos!(col + 4, line)),
              ident!("c", pos!(col + 8, line))
            )
          ),
          binary!(
            "OpShl",
            pos!(col + 12, line),
            ident!("d", pos!(col + 12, line)),
            binary!(
              "OpPow",
              pos!(col + 17, line),
              ident!("e", pos!(col + 17, line)),
              ident!("f", pos!(col + 22, line))
            )
          )
        );
      },
      "a - b * c | d << e ** f",
    );
  }

  #[test]
  fn parser_operator_priority_test_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpOr",
          pos!(col, line),
          binary!(
            "OpShr",
            pos!(col, line),
            binary!(
              "OpPlus",
              pos!(col, line),
              binary!(
                "OpMul",
                pos!(col, line),
                ident!("a", pos!(col, line)),
                ident!("b", pos!(col + 4, line))
              ),
              ident!("c", pos!(col + 8, line))
            ),
            ident!("d", pos!(col + 13, line))
          ),
          binary!(
            "OpDiv",
            pos!(col + 17, line),
            ident!("e", pos!(col + 17, line)),
            ident!("f", pos!(col + 21, line))
          )
        );
      },
      "a * b + c >> d | e / f",
    );
  }

  #[test]
  fn parser_operator_priority_test_3() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpMul",
          pos!(col + 2, line),
          binary!(
            "OpPlus",
            pos!(col + 2, line),
            binary!(
              "OpPlus",
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              ident!("b", pos!(col + 6, line))
            ),
            binary!(
              "OpPlus",
              pos!(col + 12, line),
              ident!("c", pos!(col + 12, line)),
              ident!("d", pos!(col + 16, line))
            )
          ),
          binary!(
            "OpPlus",
            pos!(col + 23, line),
            number!("1", pos!(col + 23, line)),
            number!("2", pos!(col + 27, line))
          )
        );
      },
      "((a + b) + (c + d)) * (1 + 2)",
    );
  }

  #[test]
  fn parser_conditional_expression_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return cond!(
          pos!(col, line),
          ident!("x", pos!(col, line)),
          number!("1", pos!(col + 3, line)),
          number!("0", pos!(col + 6, line))
        );
      },
      "x? 1: 0",
    );
  }

  #[test]
  fn parser_conditional_expression_test_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return cond!(
          pos!(col, line),
          binary!(
            "OpPlus",
            pos!(col, line),
            ident!("x", pos!(col, line)),
            number!("1", pos!(col + 4, line))
          ),
          newexpr!(
            pos!(col + 7, line),
            callexpr!(
              "Expr",
              pos!(col + 11, line),
              ident!("X", pos!(col + 11, line)),
              exprs!(pos!(col + 12, line))
            )
          ),
          binary!(
            "OpMinus",
            pos!(col + 16, line),
            ident!("y", pos!(col + 16, line)),
            number!("3", pos!(col + 20, line))
          )
        );
      },
      "x + 1? new X(): y - 3",
    );
  }

  macro_rules! _make_assignment_test {
    ($token:tt) => {
      paste! {
        #[test]
        fn [<parser_parse_ $token _test>]() {
          use Token::*;
          single_expression_test(
            |(col, line, _, _)| {
              return binary!(stringify!($token),
                             pos!(col, line),
                             ident!("x", pos!(col, line)),
                                    number!("1", pos!(col + ($token.symbol().len() as u64) + 3, line)));
            },
            &format!("x {} 1", $token.symbol())
          );
        }
      }
    };
  }

  _make_assignment_test!(OpLogicalOrAssign);
  _make_assignment_test!(OpLogicalAndAssign);
  _make_assignment_test!(OpMulAssign);
  _make_assignment_test!(OpDivAssign);
  _make_assignment_test!(OpModAssign);
  _make_assignment_test!(OpPlusAssign);
  _make_assignment_test!(OpMinusAssign);
  _make_assignment_test!(OpAndAssign);
  _make_assignment_test!(OpOrAssign);
  _make_assignment_test!(OpXorAssign);
  _make_assignment_test!(OpAssign);
  _make_assignment_test!(OpShlAssign);
  _make_assignment_test!(OpShrAssign);
  _make_assignment_test!(OpUShrAssign);
  _make_assignment_test!(OpPowAssign);

  #[test]
  fn identifier_reference_strict_mode_early_error_test() {
    let env = [(None, ""), (Some("'use strict';"), ""), (None, "")];
    syntax_error_test(
      &env,
      "eval = 1",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    );
    syntax_error_test(
      &env,
      "arguments = 1",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(13, 22, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    )
  }

  #[test]
  fn parser_parse_array_literal_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return arraylit!(
          false,
          pos!(col, line),
          number!("1", pos!(col + 1, line)),
          number!("2", pos!(col + 3, line))
        );
      },
      "[1,2]",
    )
  }

  #[test]
  fn parser_parse_array_literal_spread_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return arraylit!(
          true,
          pos!(col, line),
          number!("1", pos!(col + 1, line)),
          unary!("Spread", "Pre", pos!(col + 3, line), ident!("x", pos!(col + 6, line)))
        );
      },
      "[1,...x]",
    )
  }

  #[test]
  fn parser_parse_array_literal_empty_test() {
    single_expression_test(
      |(col, line, _, is_skip_parser)| {
        return arraylit!(
          false,
          pos!(col, line),
          number!("1", pos!(col + 1, line)),
          empty!(pos!(col + 2, line))
        );
      },
      "[1,]",
    )
  }

  #[test]
  fn parser_parse_array_literal_empty_test_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return arraylit!(
          false,
          pos!(col, line),
          number!("1", pos!(col + 1, line)),
          empty!(pos!(col + 2, line)),
          empty!(pos!(col + 3, line)),
          empty!(pos!(col + 4, line))
        );
      },
      "[1,,,]",
    )
  }

  #[test]
  fn parser_parse_array_assignment_pattern() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          arraylit!(
            false,
            pos!(col + 1, line),
            ident!("a", pos!(col + 2, line)),
            ident!("b", pos!(col + 4, line)),
            ident!("c", pos!(col + 6, line)),
          ),
          ident!("y", pos!(col + 11, line))
        );
      },
      "([a,b,c] = y)",
    )
  }

  #[test]
  fn parser_parse_array_assignment_pattern_with_nested_array() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          arraylit!(
            false,
            pos!(col + 1, line),
            arraylit!(
              false,
              pos!(col + 2, line),
              arraylit!(false, pos!(col + 3, line), ident!("a", pos!(col + 4, line)))
            )
          ),
          ident!("y", pos!(col + 11, line))
        );
      },
      "([[[a]]] = y)",
    )
  }

  #[test]
  fn parser_parse_array_assignment_pattern_with_spread() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          arraylit!(
            false,
            pos!(col + 1, line),
            arraylit!(
              false,
              pos!(col + 2, line),
              arraylit!(
                true,
                pos!(col + 3, line),
                unary!("Spread", "Pre", pos!(col + 4, line), ident!("a", pos!(col + 7, line)))
              )
            )
          ),
          ident!("y", pos!(col + 14, line))
        );
      },
      "([[[...a]]] = y)",
    )
  }

  #[test]
  fn parser_parse_object_literal() {
    single_expression_test(
      |(col, line, _, _)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 2, line)),
            number!("1", pos!(col + 5, line))
          ),
          object_props!(
            pos!(col + 8, line),
            ident!("b", pos!(col + 8, line)),
            number!("2", pos!(col + 11, line))
          ),
          object_props!(
            pos!(col + 14, line),
            ident!("c", pos!(col + 14, line)),
            number!("3", pos!(col + 17, line))
          )
        );
      },
      "({a: 1, b: 2, c: 3})",
    )
  }

  #[test]
  fn parser_parse_object_literal_computed_property() {
    single_expression_test(
      |(col, line, _, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            str!("a", pos!(col + 3, line)),
            number!("1", pos!(col + 9, line))
          ),
          object_props!(
            pos!(col + 12, line),
            ident!("b", pos!(col + 12, line)),
            number!("2", pos!(col + 15, line))
          ),
        );
      },
      "({['a']: 1, b: 2})",
    )
  }

  #[test]
  fn parser_parse_object_literal_computed_property_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            callexpr!(
              "Expr",
              pos!(col + 3, line),
              ident!("a", pos!(col + 3, line)),
              exprs!(pos!(col + 4, line))
            ),
            number!("1", pos!(col + 9, line))
          ),
          object_props!(
            pos!(col + 12, line),
            ident!("b", pos!(col + 12, line)),
            number!("2", pos!(col + 15, line))
          ),
        );
      },
      "({[a()]: 1, b: 2})",
    )
  }

  #[test]
  fn parser_parse_object_literal_number_property() {
    single_expression_test(
      |(col, line, _, _)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            number!("0", pos!(col + 2, line)),
            number!("1", pos!(col + 5, line))
          ),
          object_props!(
            pos!(col + 8, line),
            number!("1", pos!(col + 8, line)),
            number!("2", pos!(col + 11, line))
          ),
        );
      },
      "({0: 1, 1: 2})",
    )
  }

  #[test]
  fn parser_parse_key_only_object_literal() {
    single_expression_test(
      |(col, line, _, _)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(pos!(col + 2, line), ident!("a", pos!(col + 2, line)),),
          object_props!(pos!(col + 5, line), ident!("b", pos!(col + 5, line)),),
        );
      },
      "({a, b})",
    )
  }

  #[test]
  fn parser_parse_method_object_literal() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 2, line)),
            fnexpr!(
              pos!(col + 2, line),
              "Function",
              if is_skip_parser { col + 8 } else { 0 },
              if is_skip_parser { col + 8 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("a", pos!(col + 2, line)),
              exprs!(pos!(col + 4, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 7, line))
              }
            )
          ),
          object_props!(
            pos!(col + 10, line),
            ident!("b", pos!(col + 10, line)),
            fnexpr!(
              pos!(col + 10, line),
              "Function",
              if is_skip_parser { col + 16 } else { 0 },
              if is_skip_parser { col + 16 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("b", pos!(col + 10, line)),
              exprs!(pos!(col + 12, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 15, line))
              }
            )
          ),
        );
      },
      "({a() {}, b() {}})",
      2,
    )
  }

  #[test]
  fn parser_parse_get_set_method_object_literal() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 6, line)),
            getset_fnexpr!(
              pos!(col + 2, line),
              "Function",
              "get",
              if is_skip_parser { col + 12 } else { 0 },
              if is_skip_parser { col + 12 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("a", pos!(col + 6, line)),
              exprs!(pos!(col + 7, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 11, line))
              }
            )
          ),
          object_props!(
            pos!(col + 14, line),
            ident!("b", pos!(col + 18, line)),
            getset_fnexpr!(
              pos!(col + 14, line),
              "Function",
              "set",
              if is_skip_parser { col + 25 } else { 0 },
              if is_skip_parser { col + 25 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("b", pos!(col + 18, line)),
              exprs!(pos!(col + 20, line), ident!("x", pos!(col + 20, line))),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 24, line))
              }
            )
          ),
        );
      },
      "({get a() {}, set b(x) {}})",
      2,
    )
  }

  #[test]
  fn parser_parse_object_literal_function_expr_value() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 2, line)),
            fnexpr!(
              pos!(col + 5, line),
              "ArrowFunction",
              if is_skip_parser { col + 13 } else { 0 },
              if is_skip_parser { col + 13 } else { 0 },
              scope!(@transparent is_strict, 0, true),
              exprs!(pos!(col + 5, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 12, line))
              }
            )
          ),
        );
      },
      "({a: () => {}})",
      1,
    )
  }

  #[test]
  fn parser_parse_async_method_object_literal() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 8, line)),
            afnexpr!(
              pos!(col + 2, line),
              "Function",
              if is_skip_parser { col + 14 } else { 0 },
              if is_skip_parser { col + 14 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 8, line)),
              exprs!(pos!(col + 10, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 13, line))
              }
            )
          ),
          object_props!(
            pos!(col + 16, line),
            ident!("b", pos!(col + 16, line)),
            fnexpr!(
              pos!(col + 16, line),
              "Function",
              if is_skip_parser { col + 22 } else { 0 },
              if is_skip_parser { col + 22 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("b", pos!(col + 16, line)),
              exprs!(pos!(col + 18, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(pos!(col + 21, line))
              }
            )
          ),
        );
      },
      "({async a() {}, b() {}})",
      2,
    )
  }

  #[test]
  fn parser_parse_async_method_object_literal_with_await_expression_test() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 8, line)),
            afnexpr!(
              pos!(col + 2, line),
              "Function",
              if is_skip_parser { col + 18 } else { 0 },
              if is_skip_parser { col + 23 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 8, line)),
              exprs!(pos!(col + 10, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(
                  pos!(col + 13, line),
                  stmt!(
                    pos!(col + 13, line),
                    unary!(
                      "Await",
                      "Pre",
                      pos!(col + 13, line),
                      callexpr!(
                        "Expr",
                        pos!(col + 19, line),
                        ident!("x", pos!(col + 19, line)),
                        exprs!(pos!(col + 20, line))
                      )
                    )
                  )
                )
              }
            )
          )
        );
      },
      "({async a() {await x()}})",
      1,
    )
  }

  #[test]
  fn parser_parse_generator_method_object_literal_with_yield_expression_test() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 3, line)),
            fnexpr!(
              pos!(col + 2, line),
              "Generator",
              if is_skip_parser { col + 13 } else { 0 },
              if is_skip_parser { col + 18 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 3, line)),
              exprs!(pos!(col + 5, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(
                  pos!(col + 8, line),
                  stmt!(
                    pos!(col + 8, line),
                    unary!(
                      "Yield",
                      "Pre",
                      pos!(col + 8, line),
                      callexpr!(
                        "Expr",
                        pos!(col + 14, line),
                        ident!("x", pos!(col + 14, line)),
                        exprs!(pos!(col + 15, line))
                      )
                    )
                  )
                )
              }
            )
          )
        );
      },
      "({*a() {yield x()}})",
      1,
    )
  }

  #[test]
  fn parser_parse_async_generator_method_object_literal_with_await_expression_test() {
    single_expression_test_with_scope(
      |(col, line, is_strict, is_skip_parser)| {
        return objectlit!(
          ObjectLitType::NONE,
          pos!(col + 1, line),
          object_props!(
            pos!(col + 2, line),
            ident!("a", pos!(col + 9, line)),
            afnexpr!(
              pos!(col + 2, line),
              "Generator",
              if is_skip_parser { col + 19 } else { 0 },
              if is_skip_parser { col + 36 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 9, line)),
              exprs!(pos!(col + 11, line)),
              if is_skip_parser {
                void!()
              } else {
                stmts!(
                  pos!(col + 14, line),
                  stmt!(
                    pos!(col + 14, line),
                    unary!(
                      "Await",
                      "Pre",
                      pos!(col + 14, line),
                      callexpr!(
                        "Expr",
                        pos!(col + 20, line),
                        ident!("x", pos!(col + 20, line)),
                        exprs!(pos!(col + 21, line))
                      )
                    )
                  ),
                  stmt!(
                    pos!(col + 25, line),
                    unary!(
                      "Yield",
                      "Pre",
                      pos!(col + 25, line),
                      callexpr!(
                        "Expr",
                        pos!(col + 31, line),
                        ident!("y", pos!(col + 31, line)),
                        exprs!(pos!(col + 32, line))
                      )
                    )
                  )
                )
              }
            )
          )
        );
      },
      "({async *a() {await x(); yield y();}})",
      1,
    )
  }

  #[test]
  fn parse_simple_object_pattern() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(pos!(col + 2, line), ident!("a", pos!(col + 2, line)),),
          ),
          ident!("X", pos!(col + 7, line))
        );
      },
      "({a} = X)",
    )
  }

  #[test]
  fn parse_simple_object_pattern_2() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(pos!(col + 2, line), ident!("a", pos!(col + 2, line)),),
            object_props!(pos!(col + 5, line), ident!("b", pos!(col + 5, line)),),
          ),
          ident!("X", pos!(col + 10, line))
        );
      },
      "({a, b} = X)",
    )
  }

  #[test]
  fn parse_object_pattern_with_initializer() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              number!("2", pos!(col + 6, line))
            ),
            object_props!(pos!(col + 9, line), ident!("b", pos!(col + 9, line)),),
          ),
          ident!("X", pos!(col + 14, line))
        );
      },
      "({a = 2, b} = X)",
    )
  }

  #[test]
  fn parse_object_pattern_with_value_and_initializer() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              binary!(
                "OpAssign",
                pos!(col + 5, line),
                ident!("b", pos!(col + 5, line)),
                number!("1", pos!(col + 9, line))
              )
            ),
          ),
          ident!("X", pos!(col + 14, line))
        );
      },
      "({a: b = 1} = X)",
    )
  }

  #[test]
  fn parse_object_pattern_with_spread() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              unary!("Spread", "Pre", pos!(col + 2, line), ident!("a", pos!(col + 5, line))),
            ),
          ),
          ident!("X", pos!(col + 10, line))
        );
      },
      "({...a} = X)",
    )
  }

  #[test]
  fn parse_object_pattern_with_nested() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + 5, line),
                object_props!(
                  pos!(col + 6, line),
                  ident!("b", pos!(col + 6, line)),
                  objectlit!(
                    ObjectLitType::NONE,
                    pos!(col + 9, line),
                    object_props!(pos!(col + 10, line), ident!("c", pos!(col + 10, line)))
                  )
                )
              ),
            ),
          ),
          ident!("X", pos!(col + 17, line))
        );
      },
      "({a: {b: {c}}} = X)",
    )
  }

  #[test]
  fn parse_object_pattern_and_array_pattern() {
    single_expression_test(
      |(col, line, _, _)| {
        return binary!(
          "OpAssign",
          pos!(col + 1, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + 5, line),
                object_props!(
                  pos!(col + 6, line),
                  ident!("b", pos!(col + 6, line)),
                  arraylit!(false, pos!(col + 9, line), ident!("c", pos!(col + 10, line)))
                )
              ),
            ),
            object_props!(
              pos!(col + 15, line),
              ident!("d", pos!(col + 15, line)),
              arraylit!(false, pos!(col + 18, line), ident!("e", pos!(col + 19, line)))
            )
          ),
          ident!("X", pos!(col + 25, line))
        );
      },
      "({a: {b: [c]}, d: [e]} = X)",
    )
  }

  #[test]
  fn object_pattern_spread_is_not_last_element_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "([...a, b] = x)",
      &[&s_pos!(2, 6, 0, 0), &s_pos!(15, 19, 0, 0), &s_pos!(17, 21, 0, 0)],
      false,
    );
  }

  #[test]
  fn function_has_duplicated_param_in_strict_mode_early_error_test() {
    let env = [(None, ""), (Some("'use strict';"), ""), (None, "")];
    syntax_error_test(
      &env,
      "((a, b, b) => {})",
      &[&s_pos!(0, 0, 0, 0), &s_pos!(21, 22, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_direct_super_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, b, super) {}})",
      &[&s_pos!(10, 15, 0, 0), &s_pos!(23, 28, 0, 0), &s_pos!(25, 30, 0, 0)],
      false,
    );

    syntax_error_test(
      &BASIC_ENV,
      "({a(a, b) { super() }})",
      &[&s_pos!(12, 17, 0, 0), &s_pos!(25, 30, 0, 0), &s_pos!(27, 32, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, a) {}})",
      &[&s_pos!(7, 8, 0, 0), &s_pos!(20, 21, 0, 0), &s_pos!(22, 23, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_with_simple_object_pattern_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, {a}) {}})",
      &[&s_pos!(8, 9, 0, 0), &s_pos!(21, 22, 0, 0), &s_pos!(23, 24, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_with_simple_object_pattern_error_test_2() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, {a, b}) {}})",
      &[&s_pos!(8, 9, 0, 0), &s_pos!(21, 22, 0, 0), &s_pos!(23, 24, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_with_nested_object_pattern_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, {b: {c: {a}}}) {}})",
      &[&s_pos!(16, 17, 0, 0), &s_pos!(29, 30, 0, 0), &s_pos!(31, 32, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_with_complex_pattern_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, {[((a, b) => {})()]: [{c: {o: [{a}]}}]}) {}})",
      &[&s_pos!(39, 40, 0, 0), &s_pos!(52, 53, 0, 0), &s_pos!(54, 55, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_duplicated_parameters_skip_parser_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a(a, b) { ({b(a, a) {}}) }})",
      &[&s_pos!(19, 20, 0, 0), &s_pos!(32, 33, 0, 0), &s_pos!(34, 35, 0, 0)],
      false,
    );
  }

  #[test]
  fn method_has_not_simple_parameter_but_declare_strict() {
    syntax_error_test(
      &BASIC_ENV,
      "({a({a}) {'use strict'}})",
      &[&s_pos!(10, 22, 0, 0), &s_pos!(23, 35, 0, 0), &s_pos!(25, 37, 0, 0)],
      false,
    );
  }

  #[test]
  fn setter_has_not_simple_parameter_but_declare_strict() {
    syntax_error_test(
      &BASIC_ENV,
      "({set a({a}) {'use strict'}})",
      &[&s_pos!(14, 26, 0, 0), &s_pos!(27, 39, 0, 0), &s_pos!(29, 41, 0, 0)],
      false,
    );
  }

  #[test]
  fn object_literal_has_propery_name_initializer_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({a = 1, b})",
      &[&s_pos!(4, 5, 0, 0), &s_pos!(17, 18, 0, 0), &s_pos!(19, 20, 0, 0)],
      false,
    );
  }

  #[test]
  fn generator_method_not_allowed_to_use_yield_param_early_error_test() {
    syntax_error_test(
      &BASIC_ENV,
      "({*a(yield) {}})",
      &[&s_pos!(5, 10, 0, 0), &s_pos!(18, 23, 0, 0), &s_pos!(20, 25, 0, 0)],
      false,
    );
  }
}
