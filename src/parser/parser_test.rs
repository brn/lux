#[cfg(test)]
mod parser_test {
  use super::super::ast::*;
  use super::super::error_reporter::*;
  use super::super::parser::*;
  use super::super::source_position::*;
  use super::*;
  use crate::context::*;
  use crate::utility::*;

  struct TestableAst {
    name: String,
    attr: String,
    pos: SourcePosition,
    children: Vec<Box<TestableAst>>,
  }

  impl TestableAst {
    fn ast(name: &str, attr: &str, pos: SourcePosition) -> Self {
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

  macro_rules! pos {
    ($start_col:expr, $end_col:expr, $start_line:expr, $end_line:expr) => {
      SourcePosition::with(Some($start_col), Some($end_col), Some($start_line), Some($end_line))
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
      )*;
      ast
    }};
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

  macro_rules! fnexpr {
    ($pos:expr, $type:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {}", $type);
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

  macro_rules! computed {
    ($type:expr, $pos:expr, $prop:expr) => {{
      ast_with_children!("PropertyAccessExpression", "property_access = element", $pos, $prop)
    }};
  }

  macro_rules! lit {
    ($attr:expr, $pos:expr) => {{
      ast!("Literal", $attr, $pos)
    }};
  }

  macro_rules! arraylit {
    ($has_spread:expr, $pos:expr, $callee:expr, $($asts:expr),*$(,)*) => {{
      let attr = if has_sparead {
        "type = ArrayLiteral spread = true"
      } else {
        "type = ArrayLiteral"
      };
      ast_with_children!("StructuralLiteral", attr, $pos, $($asts,)*)
    }};
  }

  bitflags! {
    struct ObjectLitType: u8 {
      const HAS_ACCESSOR = 1;
      const HAS_GENERATOR = 2;
      const HAS_SPREAD = 4;
    }
  }
  macro_rules! objectlit {
    ($lit_type:expr, $pos:expr, $callee:expr, $($asts:expr),*$(,)*) => {{
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
    ($pos:expr, $then:expr, $else:expr) => {{
      ast_with_children!("ConditionalExpression", &attr, $pos, $then, $else)
    }};
  }

  macro_rules! elision {
    ($pos:expr) => {{
      ast!("Elision", "", $pos)
    }};
  }

  type Expectations<'a> = [&'a str; 3];

  fn parse_test<'a>(env_list: &[(Option<&'a str>, &'a str); 3], code: &str, expectations: &'a Expectations) {
    for i in 0..3 {
      let env = env_list[i];
      if env.0.is_none() {
        continue;
      }
      let str = format!("{}{}{};PARSER_SENTINEL", env.0.unwrap(), code, env.1);
      let context = LuxContext::new_until_internal_object_records();
      let mut parser = Parser::new(context, &str);
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
          panic!(err);
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
    for i in 0..3 {
      let env = env_list[i];
      if env.0.is_some() {
        let str = format!("{}{}{};PARSER_SENTINEL", env.0.unwrap(), code, env.1);
        let context = LuxContext::new_until_internal_object_records();
        let mut parser = Parser::new(context, &str);
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
              println!("error is {:?}", e);
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

  fn wrap_with_function_expr<F: Fn((u32, u32)) -> TestableAst>(
    expr_size: u32,
    ast_builder: F,
    end_line_number: u32,
    mut before_line_break_col_count: u32,
  ) -> (F, String) {
    let buf = String::new();
    let original_blb_cc = before_line_break_col_count;
    if before_line_break_col_count > 0 {
      before_line_break_col_count += 14;
    }

    let exit = (31 + expr_size) - before_line_break_col_count;
    let exit_expr = (13 + expr_size) - before_line_break_col_count;
    let func_exit = exit_expr + 2;
    let base_position = 9;
    let sentinel_start = if original_blb_cc > 0 {
      (expr_size - original_blb_cc) + 2
    } else {
      func_exit + 1
    };

    let ast = stmts!(
      pos!(0, exit, 0, end_line_number),
      stmt!(
        pos!(0, func_exit, 0, end_line_number),
        fnexpr!(
          pos!(0, func_exit, 0, end_line_number),
          "Function",
          ident!("X", pos!(base_position, base_position + 1, 0, 0)),
          exprs!(pos!(base_position + 1, base_position + 3, 0, 0)),
          stmts!(
            pos!(
              base_position + 4,
              (base_position + (7 + expr_size)) - before_line_break_col_count,
              0,
              end_line_number
            ),
            stmt!(
              pos!(
                base_position + 5,
                (base_position + (5 + expr_size)) - before_line_break_col_count,
                0,
                end_line_number
              ),
              ast_builder((
                base_position + 5,
                (base_position + (5 + expr_size) - before_line_break_col_count)
              ))
            )
          ),
        )
      ),
      stmt!(
        pos!(sentinel_start, exit, 0, end_line_number),
        ident!("PARSER_SENTINEL", pos!(sentinel_start, exit, 0, end_line_number))
      )
    );

    return (ast_builder, ast.to_string());
  }

  fn single_expression_test<F: Fn((u32, u32)) -> TestableAst>(ast_builder: F, value: &str) {
    single_expression_test_with_options(ast_builder, value, false, 0, 0);
  }

  fn single_expression_test_with_options<F: Fn((u32, u32)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    is_skip_strict_mode: bool,
    end_line_number: u32,
    before_line_break_count: u32,
  ) {
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
      (Some("function X() {"), "}"),
    ];

    let size = value.len() as u32;
    let product1 = ast_builder((0, size - before_line_break_count));
    let (ast_b, f) = wrap_with_function_expr(size, ast_builder, end_line_number, before_line_break_count);
    let mut exit = (16 + size) - before_line_break_count;
    let expr_exit = size - before_line_break_count;
    let normal = stmts!(
      pos!(0, exit, 0, end_line_number),
      stmt!(pos!(0, expr_exit, 0, end_line_number), product1),
      stmt!(
        pos!((size + 1) - before_line_break_count, exit, 0, end_line_number),
        ident!(
          "PARSER_SENTINEL",
          pos!((size + 1) - before_line_break_count, exit, 0, end_line_number)
        )
      )
    )
    .to_string();

    exit = if before_line_break_count > 0 {
      size - before_line_break_count + 16
    } else {
      29 + size
    };
    let expr_stmt_exit = if before_line_break_count > 0 {
      size - before_line_break_count
    } else {
      (13 + size) - before_line_break_count
    };
    let sentinel_start_col = if before_line_break_count > 0 {
      size - before_line_break_count + 1
    } else {
      (size + 14) - before_line_break_count
    };
    let product2 = ast_b((13, expr_stmt_exit));
    let strict = stmts!(
      pos!(13, exit, 0, end_line_number),
      stmt!(pos!(13, expr_stmt_exit, 0, end_line_number), product2),
      stmt!(
        pos!(sentinel_start_col, exit, 0, end_line_number),
        ident!("PARSER_SENTINEL", pos!(sentinel_start_col, exit, 0, end_line_number))
      )
    )
    .to_string();

    let expected: Expectations = [&normal, &strict, &f];
    parse_test(&env, value, &expected);
  }

  #[test]
  fn parse_single_decimal_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("1", pos!(start, end, 0, 0));
      },
      "1",
    );
  }

  #[test]
  fn parse_multi_decimal_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("1024", pos!(start, end, 0, 0));
      },
      "1024",
    );
  }

  #[test]
  fn parse_multi_decimal_exponent_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("130000000000", pos!(start, end, 0, 0));
      },
      "13e+10",
    );
  }

  #[test]
  fn parse_float_leading_zero_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("0.12", pos!(start, end, 0, 0));
      },
      "0.12",
    );
  }

  #[test]
  fn parse_float_not_leading_zero_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("0.12", pos!(start, end, 0, 0));
      },
      ".12",
    );
  }

  #[test]
  fn parse_hex_decimal_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("12379813812177893000", pos!(start, end, 0, 0));
      },
      "0xabcdef1234567890",
    );
  }

  #[test]
  fn parse_binary_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("21", pos!(start, end, 0, 0));
      },
      "0b010101",
    );
  }

  #[test]
  fn parse_octal_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("511", pos!(start, end, 0, 0));
      },
      "0o777",
    );
  }

  #[test]
  fn parse_implicit_octal_literal_test() {
    single_expression_test_with_options(
      |(start, end)| {
        return octal!("511", pos!(start, end, 0, 0));
      },
      "0777",
      true,
      0,
      0,
    );
  }

  #[test]
  fn parse_decimal_leading_zero_literal_test() {
    single_expression_test(
      |(start, end)| {
        return number!("7778", pos!(start, end, 0, 0));
      },
      "07778",
    );
  }

  #[test]
  fn numeric_literal_error_test() {
    let env = [(Some(""), ""), (Some("'use strict'"), ""), (Some("function X() {"), "")];
    syntax_error_test(
      &env,
      "0x_",
      &[&pos!(0, 2, 0, 0), &pos!(12, 14, 0, 0), &pos!(14, 16, 0, 0)],
      false,
    );

    syntax_error_test(
      &env,
      "0b_",
      &[&pos!(0, 2, 0, 0), &pos!(12, 14, 0, 0), &pos!(14, 16, 0, 0)],
      false,
    );

    syntax_error_test(
      &env,
      "0o_",
      &[&pos!(0, 2, 0, 0), &pos!(12, 14, 0, 0), &pos!(14, 16, 0, 0)],
      false,
    );

    syntax_error_test(
      &env,
      "13e",
      &[&pos!(0, 3, 0, 0), &pos!(12, 15, 0, 0), &pos!(14, 17, 0, 0)],
      false,
    );

    syntax_error_test(
      &env,
      "13e+",
      &[&pos!(0, 4, 0, 0), &pos!(12, 16, 0, 0), &pos!(14, 18, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_implicit_octal_error_test() {
    let env = [(None, ""), (None, ""), (Some("'use strict';"), "")];
    syntax_error_test(
      &env,
      "0777",
      &[&pos!(0, 0, 0, 0), &pos!(0, 0, 0, 0), &pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_single_quote_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test", pos!(start, end, 0, 0));
      },
      "'test'",
    );
  }

  #[test]
  fn parse_double_quote_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test", pos!(start, end, 0, 0));
      },
      "\"test\"",
    );
  }

  #[test]
  fn parse_single_quote_escaped_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test 'value", pos!(start, end, 0, 0));
      },
      "'test \\'value'",
    );
  }

  #[test]
  fn parse_double_quote_escaped_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test \"value", pos!(start, end, 0, 0));
      },
      "\"test \\\"value\"",
    );
  }

  #[test]
  fn parse_single_quote_backslash_escaped_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test\\ value", pos!(start, end, 0, 0));
      },
      "'test\\\\ value'",
    );
  }

  #[test]
  fn parse_double_quote_backslash_escaped_string_literal_test() {
    single_expression_test(
      |(start, end)| {
        return str!("test\\ value", pos!(start, end, 0, 0));
      },
      "\"test\\\\ value\"",
    );
  }

  #[test]
  fn parse_string_literal_unicode_escape_sequence_test() {
    single_expression_test(
      |(start, end)| {
        return str!("A_B_C_D", pos!(start, end, 0, 0));
      },
      "'\\u0041_\\u0042_\\u0043_\\u0044'",
    );
  }

  #[test]
  fn parse_string_literal_ascii_escape_sequence_test() {
    single_expression_test(
      |(start, end)| {
        return str!("A_B_C_D", pos!(start, end, 0, 0));
      },
      "'\\x41_\\x42_\\x43_\\x44'",
    );
  }

  #[test]
  fn parse_template_literal_without_interpolation_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(pos!(start, end, 0, 0), str!("test", pos!(start + 1, end - 1, 0, 0)));
      },
      "`test`",
    );
  }

  #[test]
  fn parse_template_literal_escaped_without_interpolation_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(
          pos!(start, end, 0, 0),
          str!("test${aaa}", pos!(start + 1, end - 1, 0, 0))
        );
      },
      "`test\\${aaa}`",
    );
  }

  #[test]
  fn parse_template_literal_linebreak_without_interpolation_test() {
    single_expression_test_with_options(
      |(start, end)| {
        return tmpl!(pos!(start, end, 0, 1), str!("test\ntest", pos!(start + 1, 4, 0, 1)));
      },
      "`test\ntest`",
      false,
      1,
      6,
    );
  }

  #[test]
  fn parse_template_literal_with_empty_suffix_interpolation_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(
          pos!(start, end, 0, 0),
          str!("test", pos!(start + 1, end - 8, 0, 0)),
          ident!("test", pos!(start + 7, end - 2, 0, 0))
        );
      },
      "`test${test}`",
    );
  }

  #[test]
  fn parse_template_literal_with_suffix_interpolation_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(
          pos!(start, end, 0, 0),
          str!("foo", pos!(start + 1, end - 10, 0, 0)),
          ident!("bar", pos!(start + 6, end - 5, 0, 0)),
          str!("baz", pos!(start + 10, end - 1, 0, 0)),
        );
      },
      "`foo${bar}baz`",
    );
  }

  #[test]
  fn parse_template_literal_with_many_suffix_interpolation_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(
          pos!(start, end, 0, 0),
          str!("foo", pos!(start + 1, end - 23, 0, 0)),
          ident!("bar", pos!(start + 6, end - 18, 0, 0)),
          str!("baz", pos!(start + 10, end - 14, 0, 0)),
          number!("100", pos!(start + 15, end - 9, 0, 0)),
          unary!(
            "OpPlus",
            "Pre",
            pos!(start + 21, end - 2, 0, 0),
            ident!("foo", pos!(start + 22, end - 2, 0, 0))
          )
        );
      },
      "`foo${bar}baz${100}${+foo}`",
    );
  }

  #[test]
  fn parse_nested_template_literal_test() {
    single_expression_test(
      |(start, end)| {
        return tmpl!(
          pos!(start, end, 0, 0),
          str!("foo", pos!(start + 1, end - 15, 0, 0)),
          tmpl!(
            pos!(start + 6, end - 2, 0, 0),
            str!("foo", pos!(start + 7, end - 9, 0, 0)),
            ident!("bar", pos!(start + 12, end - 4, 0, 0)),
          ),
        );
      },
      "`foo${`foo${bar}`}`",
    );
  }

  #[test]
  fn parse_unterminated_string_error_test() {
    let env = [
      (Some(""), ""),
      (Some("'use strict';"), ""),
      (Some("function X() {"), ""),
    ];
    syntax_error_test(
      &env,
      "'test",
      &[&pos!(0, 21, 0, 0), &pos!(13, 34, 0, 0), &pos!(14, 35, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_unterminated_string_error_with_linebreak_test() {
    let env = [
      (Some(""), ""),
      (Some("'use strict';"), ""),
      (Some("function X() {"), ""),
    ];
    syntax_error_test(
      &env,
      "'test\\n",
      &[&pos!(0, 23, 0, 0), &pos!(13, 36, 0, 0), &pos!(14, 37, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_invalid_unicode_sequence_error_test() {
    let env = [
      (Some(""), ""),
      (Some("'use strict';"), ""),
      (Some("function X() {"), ""),
    ];
    syntax_error_test(
      &env,
      "'\\u0041_\\u0042_\\u043_\\u0044'",
      &[&pos!(0, 20, 0, 0), &pos!(13, 33, 0, 0), &pos!(14, 34, 0, 0)],
      false,
    )
  }

  #[test]
  fn parse_unary_expression_plus_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpPlus",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 1, end, 0, 0))
        );
      },
      "+1",
    );
  }

  #[test]
  fn parse_unary_expression_minus_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpMinus",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 1, end, 0, 0))
        );
      },
      "-1",
    );
  }

  #[test]
  fn parse_unary_expression_not_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpNot",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 1, end, 0, 0))
        );
      },
      "!1",
    );
  }

  #[test]
  fn parse_unary_expression_tilde_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpTilde",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 1, end, 0, 0))
        );
      },
      "~1",
    );
  }

  #[test]
  fn parse_unary_expression_delete_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "Delete",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 7, end, 0, 0))
        );
      },
      "delete 1",
    );
  }

  #[test]
  fn parse_unary_expression_typeof_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "Typeof",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 7, end, 0, 0))
        );
      },
      "typeof 1",
    );
  }

  #[test]
  fn parse_unary_expression_void_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "Void",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 5, end, 0, 0))
        );
      },
      "void 1",
    );
  }

  #[test]
  fn parse_unary_expression_increments_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpIncrement",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 2, end, 0, 0))
        );
      },
      "++1",
    );
  }

  #[test]
  fn parse_unary_expression_decrements_pre_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpDecrement",
          "Pre",
          pos!(start, end, 0, 0),
          number!("1", pos!(start + 2, end, 0, 0))
        );
      },
      "--1",
    );
  }

  #[test]
  fn parse_unary_expression_increments_post_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpIncrement",
          "Post",
          pos!(start, end, 0, 0),
          number!("1", pos!(start, end - 2, 0, 0))
        );
      },
      "1++",
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_test() {
    single_expression_test(
      |(start, end)| {
        return unary!(
          "OpDecrement",
          "Post",
          pos!(start, end, 0, 0),
          number!("1", pos!(start, end - 2, 0, 0))
        );
      },
      "1--",
    );
  }

  #[test]
  fn parse_new_expression_no_args_test() {
    single_expression_test(
      |(start, end)| {
        return newexpr!(pos!(start, end, 0, 0), ident!("X", pos!(start + 4, end, 0, 0)));
      },
      "new X",
    );
  }

  #[test]
  fn parse_new_expression_with_args_test() {
    single_expression_test(
      |(start, end)| {
        return newexpr!(
          pos!(start, end, 0, 0),
          callexpr!(
            "Expr",
            pos!(start + 4, end, 0, 0),
            ident!("X", pos!(start + 4, end - 3, 0, 0)),
            exprs!(pos!(start + 5, end, 0, 0), number!("1", pos!(start + 6, end - 1, 0, 0)))
          )
        );
      },
      "new X(1)",
    );
  }

  #[test]
  fn parse_new_expression_with_props_call_test() {
    single_expression_test(
      |(start, end)| {
        return newexpr!(
          pos!(start, end, 0, 0),
          callexpr!(
            "Expr",
            pos!(start + 4, end, 0, 0),
            prop!(
              "dot",
              pos!(start + 4, end - 3, 0, 0),
              ident!("X", pos!(start + 4, end - 5, 0, 0)),
              ident!("a", pos!(start + 6, end - 3, 0, 0))
            ),
            exprs!(pos!(start + 7, end, 0, 0), number!("1", pos!(start + 8, end - 1, 0, 0)))
          )
        );
      },
      "new X.a(1)",
    );
  }

  #[test]
  fn parse_new_expression_with_element_call_test() {
    single_expression_test(
      |(start, end)| {
        return newexpr!(
          pos!(start, end, 0, 0),
          callexpr!(
            "Expr",
            pos!(start + 4, end, 0, 0),
            prop!(
              "element",
              pos!(start + 4, end - 3, 0, 0),
              ident!("X", pos!(start + 4, end - 8, 0, 0)),
              str!("a", pos!(start + 6, end - 4, 0, 0))
            ),
            exprs!(
              pos!(start + 10, end, 0, 0),
              number!("1", pos!(start + 11, end - 1, 0, 0))
            )
          )
        );
      },
      "new X['a'](1)",
    );
  }

  #[test]
  fn parse_new_expression_with_props_chain_test() {
    single_expression_test(
      |(start, end)| {
        return newexpr!(
          pos!(start, end, 0, 0),
          callexpr!(
            "Expr",
            pos!(start + 4, end, 0, 0),
            prop!(
              "dot",
              pos!(start + 4, end - 2, 0, 0),
              prop!(
                "dot",
                pos!(start + 4, end - 4, 0, 0),
                ident!("a", pos!(start + 4, end - 6, 0, 0)),
                ident!("b", pos!(start + 6, end - 4, 0, 0))
              ),
              ident!("c", pos!(start + 8, end - 2, 0, 0))
            ),
            exprs!(pos!(start + 9, end, 0, 0),)
          )
        );
      },
      "new a.b.c()",
    );
  }
}
