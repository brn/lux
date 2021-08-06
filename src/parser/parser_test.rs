#[cfg(test)]
mod parser_test {
  use super::super::ast::*;
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
    ($receiver:expr, $pos:expr, $callee:expr, $($asts:expr),*$(,)*) => {{
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

  macro_rules! str {
    ($val:expr, $pos:expr) => {{
      let attr = format!("type = StringLiteral value = {}", $val);
      ast_with_children!("Literal", $attr, $pos, $($asts,)*)
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

  fn parse_test<'a>(env_list: &[[&'a str; 2]; 3], code: &str, expectations: &'a Expectations) {
    for i in 0..3 {
      let env = env_list[i];
      let str = format!("{}{}{};PARSER_SENTINEL", env[0], code, env[1]);
      let context = LuxContext::new_until_internal_object_records();
      let mut parser = Parser::new(context, &str);
      match parser.parse(ParserType::Script) {
        Ok(ast) => {
          let tree = ast.to_string_tree();
          compare_node(&str, &tree, expectations[i]);
        }
        Err(err) => {
          parser.print_stack_trace();
          println!("code is {}", str);
          panic!(err);
        }
      }
    }
  }

  fn syntax_error_test<'a>(env_list: &[[&'a str; 2]; 3], code: &str, show_error: bool) {
    for i in 0..3 {
      let env = env_list[i];
      if !env[0].is_empty() {
        let str = format!("{}{}{};PARSER_SENTINEL", env[0], code, env[1]);
        let context = LuxContext::new_until_internal_object_records();
        let mut parser = Parser::new(context, &str);
        let ast = parser.parse(ParserType::Script);
        let m = format!("Code {} not generate error", code);
        assert!(ast.is_err(), m);
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
    let env: [[&str; 2]; 3] = [
      ["", ""],
      [if is_skip_strict_mode { "" } else { "'use strict';" }, ""],
      ["function X() {", "}"],
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

    exit = 29 + size;
    let expr_stmt_exit = (13 + size) - before_line_break_count;
    let product2 = ast_b((13, expr_stmt_exit));
    let strict = stmts!(
      pos!(13, exit, 0, end_line_number),
      stmt!(pos!(13, expr_stmt_exit, 0, end_line_number), product2),
      stmt!(
        pos!((size + 14) - before_line_break_count, exit, 0, end_line_number),
        ident!(
          "PARSER_SENTINEL",
          pos!((size + 14) - before_line_break_count, exit, 0, end_line_number)
        )
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
}
