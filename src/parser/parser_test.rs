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

  const STATIC_STR_LEN: u64 = "static ".len() as u64;

  bitflags! {
    struct EnvFlag: u16 {
      const BASIC = 0x1;
      const STRICT_MODE = 0x2;
      const FUNCTION_WRAPPER = 0x4;
    }
  }
  type EnvList = [Option<(&'static str, &'static str)>; 3];

  #[derive(Clone)]
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
    (@lexical $is_strict:expr, $children_scope_len:expr, $has_parent:expr) => {
      scope!("lexical", $is_strict, $children_scope_len, $has_parent)
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
      ast_with_children!("Function", &attr, $pos, $($asts,)*)
    }}
  }

  macro_rules! afnexpr {
    ($pos:expr, $type:expr, $start:expr, $end:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {} async body_start = {} body_end = {} {}", $type, $start, $end, $scope);
      ast_with_children!("Function", &attr, $pos, $($asts,)*)
    }}
  }

  macro_rules! getset_fnexpr {
    ($pos:expr, $type:expr, $accessor:expr, $start:expr, $end:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      let attr = format!("type = {} {} body_start = {} body_end = {} {}", $type, $accessor, $start, $end, $scope);
      ast_with_children!("Function", &attr, $pos, $($asts,)*)
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
    ($type:expr, $receiver:expr, $pos:expr, $callee:expr, $prop:expr) => {{
      let attr = format!("property_access = {} receiver = {}", $type, $receiver);
      ast_with_children!("PropertyAccessExpression", &attr, $pos, $callee, $prop)
    }};
    ($type:expr, $receiver:expr, $pos:expr) => {{
      let attr = format!("property_access = {} receiver = {}", $type, $receiver);
      ast!("PropertyAccessExpression", &attr, $pos)
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

  macro_rules! class_field {
    ($attr:expr, $pos:expr, $value:expr) => {{
      ast_with_children!("ClassField", $attr, $pos, $value)
    }};
  }

  macro_rules! import_specifier {
    ($pos:expr, $is_ns:expr, $name:expr, $as_expr:expr) => {{
      let attr = format!("is_namespace = {}", $is_ns);
      ast_with_children!("ImportSpecifier", &attr, $pos, $name, $as_expr)
    }};
    ($pos:expr, $is_ns:expr, $name:expr) => {{
      let attr = format!("is_namespace = {}", $is_ns);
      ast_with_children!("ImportSpecifier", &attr, $pos, $name)
    }};
    ($pos:expr, $is_ns:expr) => {{
      let attr = format!("is_namespace = {}", $is_ns);
      ast!("ImportSpecifier", &attr, $pos)
    }};
  }

  macro_rules! named_import_list {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("NamedImportList", "", $pos, $($asts),*)
    }};
  }

  macro_rules! import_binding {
    ($pos:expr, $default_binding:expr, $namespace_import:expr) => {{
      ast_with_children!("ImportBinding", "", $pos, $default_binding, $namespace_import)
    }};
    ($pos:expr, $binding:expr) => {{
      ast_with_children!("ImportBinding", "", $pos, $binding)
    }};
  }

  macro_rules! import_stmt {
    ($pos:expr, $binding:expr, $specifier:expr) => {{
      ast_with_children!("ImportDeclaration", "", $pos, $binding, $specifier)
    }};
    ($pos:expr, $specifier:expr) => {{
      ast_with_children!("ImportDeclaration", "", $pos, $binding, $specifier)
    }};
  }

  macro_rules! export_stmt {
    ($type:expr, $pos:expr, $export_clause:expr, $from_clause:expr) => {{
      let attr = format!("type = {}", $type);
      ast_with_children!("ExportDeclaration", &attr, $pos, $export_clause, $from_clause)
    }};
    ($type:expr, $pos:expr, $export_clause:expr) => {{
      let attr = format!("type = {}", $type);
      ast_with_children!("ExportDeclaration", &attr, $pos, $export_clause)
    }};
  }

  fn class(
    pos: RuntimeSourcePosition,
    name: Option<TestableAst>,
    heritage: Option<TestableAst>,
    fields: &[TestableAst],
    methods: &[TestableAst],
  ) -> TestableAst {
    let mut ast = if let Some(n) = name {
      if let Some(h) = heritage {
        ast_with_children!("Class", "", pos, n, h)
      } else {
        ast_with_children!("Class", "", pos, n)
      }
    } else {
      if let Some(h) = heritage {
        ast_with_children!("Class", "", pos, h)
      } else {
        ast!("Class", "", pos)
      }
    };
    for field in fields.iter() {
      ast.push(field.clone());
    }
    for method in methods.iter() {
      ast.push(method.clone());
    }
    ast
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
    () => {{
      ast!("Empty", "", pos!(0, 0))
    }};
  }

  macro_rules! block {
    ($pos:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("BlockStatement", &$scope, $pos, $($asts,)*)
    }};
    ($pos:expr, $scope:expr) => {{
      ast!("BlockStatement", &$scope, $pos)
    }};
  }

  macro_rules! var {
    ($type:expr, $pos:expr, $lhs:expr, $rhs:expr) => {{
      let attr = format!("type = {}", $type);
      ast_with_children!("VariableDeclaration", &attr, $pos, $lhs, $rhs)
    }};
    ($type:expr, $pos:expr, $lhs:expr) => {{
      let attr = format!("type = {}", $type);
      ast_with_children!("VariableDeclaration", &attr, $pos, $lhs)
    }};
  }

  macro_rules! vars {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("VariableDeclarations", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! if_stmt {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("IfStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! if_stmt {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("IfStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! label {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("LabelledStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! switch {
    ($pos:expr, $scope:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("SwitchStatement", &$scope, $pos, $($asts,)*)
    }};
  }

  macro_rules! case {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("SwitchCase", "", $pos, $($asts,)*)
    }};
    (@default, $pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("SwitchCase", "default", $pos, $($asts,)*)
    }};
  }

  macro_rules! while_stmt {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("WhileStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! do_while {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("DoWhileStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! for_stmt {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("ForStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! forin {
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("ForInStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! forof {
    (@await, $pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("ForOfStatement", "await", $pos, $($asts,)*)
    }};
    ($pos:expr, $($asts:expr),*$(,)*) => {{
      ast_with_children!("ForOfStatement", "", $pos, $($asts,)*)
    }};
  }

  macro_rules! break_stmt {
    ($pos:expr, $ident:expr) => {{
      let attr = format!("identifier = '{}'", $ident);
      ast!("BreakStatement", &attr, $pos)
    }};
    ($pos:expr) => {{
      ast!("BreakStatement", "", $pos)
    }};
  }

  macro_rules! continue_stmt {
    ($pos:expr, $ident:expr) => {{
      let attr = format!("identifier = '{}'", $ident);
      ast!("ContinueStatement", &attr, $pos)
    }};
    ($pos:expr) => {{
      ast!("ContinueStatement", "", $pos)
    }};
  }

  macro_rules! return_stmt {
    ($pos:expr, $expr:expr) => {{
      ast_with_children!("ReturnStatement", "", $pos, $expr)
    }};
    ($pos:expr) => {{
      ast!("ReturnStatement", "", $pos)
    }};
  }

  macro_rules! throw_stmt {
    ($pos:expr, $expr:expr) => {{
      ast_with_children!("ThrowStatement", "", $pos, $expr)
    }};
  }

  macro_rules! catch_block {
    ($pos:expr, $param:expr, $body:expr) => {{
      ast_with_children!("CatchBlock", "", $pos, $param, $body)
    }};
    ($pos:expr, $body:expr) => {{
      ast_with_children!("CatchBlock", "", $pos, $body)
    }};
  }

  macro_rules! try_catch_stmt {
    ($pos:expr, $try_block:expr, $catch_block:expr, $finally_block:expr) => {{
      ast_with_children!("TryCatchStatement", "", $pos, $try_block, $catch_block, $finally_block)
    }};
    ($pos:expr, $try_block:expr, $finally_or_catch_block:expr) => {{
      ast_with_children!("TryCatchStatement", "", $pos, $try_block, $finally_or_catch_block)
    }};
  }

  macro_rules! with_stmt {
    ($pos:expr, $expr:expr, $body:expr) => {{
      ast_with_children!("WithStatement", "", $pos, $expr, $body)
    }};
  }

  macro_rules! debugger_stmt {
    ($pos:expr) => {{
      ast!("DebuggerStatement", "", $pos)
    }};
  }

  type Expectations<'a> = [&'a str; 3];

  fn parse_test<'a>(
    env_list: &EnvList,
    code: &str,
    expectations: &'a Expectations,
    parser_option: ParserOption,
    parser_type: ParserType,
    should_skip_node_comparison: bool,
  ) {
    for i in 0..3 {
      let maybe_env = env_list[i];
      if let Some(env) = maybe_env {
        let str = format!("{}{}{};PARSER_SENTINEL", env.0, code, env.1);
        let context = LuxContext::new_until_internal_object_records();
        let source = Source::new(context, "anonymouse", &str);
        let mut parser = Parser::new(context, source.clone(), parser_option.clone());
        match parser.parse(parser_type) {
          Ok(ast) => {
            if !should_skip_node_comparison {
              let tree = ast.to_string_tree();
              match compare_node(&str, &tree, expectations[i]) {
                Err(em) => {
                  println!("{}", em);
                  parser.print_stack_trace();
                  panic!("Parser test failed");
                }
                _ => {}
              }
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
    if !should_skip_node_comparison {
      parse_test(
        env_list,
        code,
        expectations,
        parser_option.clone().with_skip_parser(),
        parser_type,
        true,
      );
    }
  }

  fn syntax_error_test<'a>(env_list: &EnvList, code: &str, source_positions: &[&SourcePosition], show_error: bool) {
    syntax_error_test_with_parser_type(env_list, code, source_positions, show_error, ParserType::Script);
  }

  fn syntax_error_test_with_parser_type<'a>(
    env_list: &EnvList,
    code: &str,
    source_positions: &[&SourcePosition],
    show_error: bool,
    parser_type: ParserType,
  ) {
    let parser_options = [
      ParserOption::default(),
      ParserOption::default().with_disable_skip_parser(),
      ParserOption::default().with_skip_parser(),
    ];
    for opt in (&parser_options).iter() {
      for i in 0..3 {
        let maybe_env = env_list[i];
        if let Some(env) = maybe_env {
          let str = format!("{}{}{};PARSER_SENTINEL", env.0, code, env.1);
          let context = LuxContext::new_until_internal_object_records();
          let source = Source::new(context, "anonymouse", &str);
          let mut parser = Parser::new(context, source.clone(), opt.clone());
          let ast = parser.parse(parser_type);
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
    code: &str,
    expr_size: u64,
    ast_builder: F,
    scope_count: u32,
    parser_option: ParserOption,
    mut before_line_break_col_count: u64,
    is_stmt: bool,
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
    let lb_count = code.matches('\n').count() as u32;

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
            if !is_stmt {
              stmt!(
                pos!(base_position + 5, 0),
                ast_builder((base_position + 5, 0, false, !parser_option.disable_skip_parser()))
              )
            } else {
              ast_builder((base_position + 5, 0, false, !parser_option.disable_skip_parser()))
            }
          )
        )
      ),
      stmt!(
        pos!(sentinel_start, lb_count),
        ident!("PARSER_SENTINEL", pos!(sentinel_start, lb_count))
      )
    );

    return (ast_builder, ast.to_string());
  }

  fn single_expression_test<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      0,
      EnvFlag::all(),
      0,
      ParserType::Script,
      false,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      0,
      EnvFlag::all(),
      0,
      ParserType::Script,
      false,
    );
  }

  fn single_expression_test_with_scope<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str, scope_count: u32) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      scope_count,
      EnvFlag::all(),
      0,
      ParserType::Script,
      false,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      scope_count,
      EnvFlag::all(),
      0,
      ParserType::Script,
      false,
    );
  }

  fn single_expression_test_with_options<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    parser_option: ParserOption,
    scope_count: u32,
    env_flag: EnvFlag,
    before_line_break_count: u64,
  ) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      parser_option.clone(),
      scope_count,
      env_flag,
      before_line_break_count,
      ParserType::Script,
      false,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      parser_option.clone(),
      scope_count,
      env_flag,
      before_line_break_count,
      ParserType::Script,
      false,
    );
  }

  fn stmt_test<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      0,
      EnvFlag::all(),
      0,
      ParserType::Script,
      true,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      0,
      EnvFlag::all(),
      0,
      ParserType::Script,
      true,
    );
  }

  fn module_stmt_test<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str, env_flag: EnvFlag) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      0,
      env_flag,
      0,
      ParserType::Module,
      true,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      0,
      env_flag,
      0,
      ParserType::Module,
      true,
    );
  }

  fn stmt_test_with_scope<F: Fn((u64, u32, bool, bool)) -> TestableAst>(ast_builder: F, value: &str, scope_count: u32) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      scope_count,
      EnvFlag::all(),
      0,
      ParserType::Script,
      true,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      scope_count,
      EnvFlag::all(),
      0,
      ParserType::Script,
      true,
    );
  }

  fn stmt_test_with_linebreak<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    scope_count: u32,
    before_line_break_count: u64,
  ) {
    let ast_b = parser_ast_test_with_options(
      ast_builder,
      value,
      ParserOption::default(),
      scope_count,
      EnvFlag::all(),
      before_line_break_count,
      ParserType::Script,
      true,
    );
    parser_ast_test_with_options(
      ast_b,
      value,
      ParserOption::default().with_disable_skip_parser(),
      scope_count,
      EnvFlag::all(),
      before_line_break_count,
      ParserType::Script,
      true,
    );
  }

  fn parser_ast_test_with_options<F: Fn((u64, u32, bool, bool)) -> TestableAst>(
    ast_builder: F,
    value: &str,
    parser_option: ParserOption,
    scope_count: u32,
    env_flag: EnvFlag,
    before_line_break_count: u64,
    parser_type: ParserType,
    is_stmt: bool,
  ) -> F {
    let env: EnvList = [
      if env_flag.contains(EnvFlag::BASIC) { Some(("", "")) } else { None },
      if env_flag.contains(EnvFlag::STRICT_MODE) {
        Some(("'use strict';", ""))
      } else {
        None
      },
      if env_flag.contains(EnvFlag::FUNCTION_WRAPPER) {
        Some(("!function X() {", "}"))
      } else {
        None
      },
    ];
    let lb_count = value.matches('\n').count() as u32;

    let size = value.len() as u64;
    let product1 = ast_builder((0, 0, false, !parser_option.disable_skip_parser()));
    let (ast_b, f) = wrap_with_function_expr(
      value,
      size,
      ast_builder,
      scope_count,
      parser_option.clone(),
      before_line_break_count,
      is_stmt,
    );
    let normal = stmts!(
      pos!(0, 0),
      if !is_stmt { stmt!(pos!(0, 0), product1) } else { product1 },
      stmt!(
        pos!((size + 1) - before_line_break_count, lb_count),
        ident!("PARSER_SENTINEL", pos!((size + 1) - before_line_break_count, lb_count))
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
      if !is_stmt { stmt!(pos!(13, 0), product2) } else { product2 },
      stmt!(
        pos!(sentinel_start_col, lb_count),
        ident!("PARSER_SENTINEL", pos!(sentinel_start_col, lb_count))
      )
    )
    .to_string();

    let expected: Expectations = [&normal, &strict, &f];
    parse_test(&env, value, &expected, parser_option, parser_type, false);
    return ast_b;
  }

  #[inline]
  fn basic_env_expression_eary_error_test(env_flag: EnvFlag, start: u64, end: u64, code: &str) {
    basic_env_expression_eary_error_test_with_parser_type(env_flag, start, end, code, ParserType::Script);
  }

  fn basic_env_expression_eary_error_test_with_parser_type(env_flag: EnvFlag, start: u64, end: u64, code: &str, parser_type: ParserType) {
    let env: EnvList = [
      if env_flag.contains(EnvFlag::BASIC) { Some(("", "")) } else { None },
      if env_flag.contains(EnvFlag::STRICT_MODE) {
        Some(("'use strict';", ""))
      } else {
        None
      },
      if env_flag.contains(EnvFlag::FUNCTION_WRAPPER) {
        Some(("!function X() {", "}"))
      } else {
        None
      },
    ];
    syntax_error_test_with_parser_type(
      &env,
      code,
      &[
        &s_pos!(start, end, 0, 0),
        &s_pos!(start + 13, end + 13, 0, 0),
        &s_pos!(start + 15, end + 15, 0, 0),
      ],
      false,
      parser_type,
    );
  }

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
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
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
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "0x_");

    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "0b_");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "0o_");

    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "13e");

    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "13e+");
  }

  #[test]
  fn parse_implicit_octal_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 4, "0777");
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
      ParserOption::default(),
      0,
      EnvFlag::all(),
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
          unary!("OpPlus", "Pre", pos!(col + 21, line), ident!("foo", pos!(col + 22, line)))
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
      &[Some(("", "")), Some(("'use strict';", "")), Some(("!function X() {", "}"))],
      "'test",
      &[&s_pos!(0, 21, 0, 0), &s_pos!(13, 34, 0, 0), &s_pos!(15, 37, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_unterminated_string_error_with_linebreak_test() {
    syntax_error_test(
      &[Some(("", "")), Some(("'use strict';", "")), Some(("!function X() {", "}"))],
      "'test\\n",
      &[&s_pos!(0, 23, 0, 0), &s_pos!(13, 36, 0, 0), &s_pos!(15, 39, 0, 0)],
      false,
    );
  }

  #[test]
  fn parse_invalid_unicode_sequence_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 21, "'\\u0041_\\u0042_\\u043_\\u0044'");
  }

  #[test]
  fn template_literal_not_allow_octal_escape_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 1, 4, "`\\071`");
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
  fn unary_expression_delete_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 8, "delete a");
  }

  #[test]
  fn unary_expression_delete_rec_parenthesized_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 16, "delete ((((a))))");
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
        return unary!("OpIncrement", "Pre", pos!(col, line), ident!("eval", pos!(col + 2, line)));
      },
      "++eval",
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_increments_pre_with_arguments_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!("OpIncrement", "Pre", pos!(col, line), ident!("arguments", pos!(col + 2, line)));
      },
      "++arguments",
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
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
        return unary!("OpDecrement", "Pre", pos!(col, line), ident!("yield", pos!(col + 2, line)));
      },
      "--yield",
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_pre_with_await_keyword_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Pre", pos!(col, line), ident!("await", pos!(col + 2, line)));
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
  fn parse_unary_expression_increments_post_convert_num_test() {
    single_expression_test(
      |(col, line, _, _)| {
        return unary!(
          "OpPlus",
          "Pre",
          pos!(col, line),
          unary!("OpIncrement", "Post", pos!(col + 1, line), ident!("a", pos!(col + 1, line)))
        );
      },
      "+a++",
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
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
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
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
      0,
    );
  }

  #[test]
  fn parse_unary_expression_decrements_post_with_arguments_identifier_test() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return unary!("OpDecrement", "Post", pos!(col, line), ident!("arguments", pos!(col, line)));
      },
      "arguments--",
      ParserOption::default(),
      0,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
      0,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 4, "eval++");
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 9, "arguments++");
  }

  #[test]
  fn pre_update_expression_assignment_target_yield_early_error_test() {
    const ENV: EnvList = [Some(("!function *x() {", "}")), None, None];
    syntax_error_test(
      &ENV,
      "++yield",
      &[&s_pos!(18, 23, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_await_early_error_test() {
    const ENV: EnvList = [Some(("!async function x() {", "}")), None, None];
    syntax_error_test(
      &ENV,
      "++await",
      &[&s_pos!(23, 28, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(13, 17, 0, 0)],
      false,
    );
  }

  #[test]
  fn pre_update_expression_assignment_target_primary_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 6, "++this");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++'a'");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 3, "++1");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 15, "++function() {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 16, "++function*() {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 21, "++async function() {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 22, "++async function*() {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 7, "++`abc`");
  }

  #[test]
  fn pre_update_expression_assignment_target_call_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++(1)");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 9, "++super()");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 15, "++import('abc')");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 6, "++x(1)");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 10, "++x()`abc`");
  }

  #[test]
  fn pre_update_expression_assignment_target_new_to_left_hand_side_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 7, "++new x");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 9, "++new x()");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 13, "++new new x()");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 7, "++a`aa`");
    let env_list = [Some(("!function X() {", "")), None, None];
    syntax_error_test(
      &env_list,
      "++new.target",
      &[&s_pos!(17, 27, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    );
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 13, "++import.meta");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 8, "++a.b?.c");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 12, "++a.b?.['c']");
  }

  #[test]
  fn pre_update_expression_assignment_target_update_to_unary_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++x++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++x--");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++++x");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 5, "++++x");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 12, "++delete a.b");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 8, "++void a");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 4, "+++a");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 4, "++-a");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 4, "++~a");
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 4, "++!a");
  }

  #[test]
  fn post_update_expression_assignment_target_primary_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "this++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "'a'++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 1, "1++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 15, "(function() {})++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 16, "(function*() {})++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 21, "(async function() {})++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 22, "(async function*() {})++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "`abc`++");
  }

  #[test]
  fn post_update_expression_assignment_target_call_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, "(1)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, "super()++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 13, "import('abc')++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "x(1)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 8, "x()`abc`++");
  }

  #[test]
  fn post_update_expression_assignment_target_new_to_left_hand_side_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "new x++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, "new x()++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 11, "new new x()++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "a`aa`++");
    let env_list = [Some(("!function X() {", "")), None, None];
    syntax_error_test(
      &env_list,
      "new.target++",
      &[&s_pos!(15, 25, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    );
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 11, "import.meta++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 6, "a.b?.c++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 10, "a.b?.['c']++");
  }

  #[test]
  fn post_update_expression_assignment_target_update_to_unary_expr_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 12, "(delete a.b)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 8, "(void a)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "(+a)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "(-a)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "(~a)++");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, "(!a)++");
  }

  #[test]
  fn pre_update_expression_assignment_await_expr_early_error_test() {
    syntax_error_test(
      &[Some(("!async function() {", "}")), None, None],
      "++await a",
      &[&s_pos!(21, 26, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0)],
      false,
    );
  }

  macro_rules! _make_assignment_pre_update_expr_early_error_test {
    ($token:expr) => {
      paste! {
        #[test]
        fn [<pre_update_expression_assignment_ $token _test>]() {
          use Token::*;
          let len = $token.symbol().len() as u64;
          basic_env_expression_eary_error_test(EnvFlag::all(), 2, 8 + len, &format!("++(a {} b)", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 6 + len, &format!("(a {} b)++", $token.symbol()));
        }
      }
    };
  }

  _make_assignment_pre_update_expr_early_error_test!(OpPow);
  _make_assignment_pre_update_expr_early_error_test!(OpMul);
  _make_assignment_pre_update_expr_early_error_test!(OpDiv);
  _make_assignment_pre_update_expr_early_error_test!(OpPlus);
  _make_assignment_pre_update_expr_early_error_test!(OpMinus);
  _make_assignment_pre_update_expr_early_error_test!(OpShl);
  _make_assignment_pre_update_expr_early_error_test!(OpShr);
  _make_assignment_pre_update_expr_early_error_test!(OpUShr);
  _make_assignment_pre_update_expr_early_error_test!(In);
  _make_assignment_pre_update_expr_early_error_test!(Instanceof);
  _make_assignment_pre_update_expr_early_error_test!(OpGreaterThan);
  _make_assignment_pre_update_expr_early_error_test!(OpGreaterThanOrEq);
  _make_assignment_pre_update_expr_early_error_test!(OpLessThan);
  _make_assignment_pre_update_expr_early_error_test!(OpLessThanOrEq);
  _make_assignment_pre_update_expr_early_error_test!(OpEq);
  _make_assignment_pre_update_expr_early_error_test!(OpStrictEq);
  _make_assignment_pre_update_expr_early_error_test!(OpNotEq);
  _make_assignment_pre_update_expr_early_error_test!(OpStrictNotEq);
  _make_assignment_pre_update_expr_early_error_test!(OpAnd);
  _make_assignment_pre_update_expr_early_error_test!(OpOr);
  _make_assignment_pre_update_expr_early_error_test!(OpXor);
  _make_assignment_pre_update_expr_early_error_test!(OpLogicalAnd);
  _make_assignment_pre_update_expr_early_error_test!(OpLogicalOr);
  _make_assignment_pre_update_expr_early_error_test!(OpNullCoalescing);
  _make_assignment_pre_update_expr_early_error_test!(OpLogicalOrAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpLogicalAndAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpMulAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpDivAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpModAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpPlusAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpMinusAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpAndAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpOrAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpXorAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpShlAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpShrAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpUShrAssign);
  _make_assignment_pre_update_expr_early_error_test!(OpPowAssign);

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
              "Dot",
              "Expr",
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
              "Element",
              "Expr",
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
              "Dot",
              "Expr",
              pos!(col + 4, line),
              prop!(
                "Dot",
                "Expr",
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
    basic_env_expression_eary_error_test(EnvFlag::all(), 3, 4, "a?.`a`");
  }

  #[test]
  fn template_literal_not_allowed_after_op_chain_early_error_test_2() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 6, 7, "a?.a?.`a`");
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
              "Dot",
              "Expr",
              pos!(col + 4, line),
              prop!(
                "Element",
                "Expr",
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
            "Dot",
            "Expr",
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
            "OpChaining",
            "Expr",
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

  macro_rules! _make_binary_expr_test {
    ($token:expr) => {
      paste! {
        #[test]
        fn [<parse_ $token _expr_test>]() {
          use Token::*;
          let len = $token.symbol().len() as u64;
          single_expression_test(
            |(col, line, _, _)| {
              return binary!(
                &format!("{:?}", $token),
                pos!(col, line),
                number!("1", pos!(col, line)),
                number!("1", pos!(col + 3 + len, line))
              );
            },
            &format!("1 {} 1", $token.symbol()),
          );
        }
      }
    };
  }

  _make_binary_expr_test!(OpPow);
  _make_binary_expr_test!(OpMul);
  _make_binary_expr_test!(OpDiv);
  _make_binary_expr_test!(OpPlus);
  _make_binary_expr_test!(OpMinus);
  _make_binary_expr_test!(OpShl);
  _make_binary_expr_test!(OpShr);
  _make_binary_expr_test!(OpUShr);
  _make_binary_expr_test!(In);
  _make_binary_expr_test!(Instanceof);
  _make_binary_expr_test!(OpGreaterThan);
  _make_binary_expr_test!(OpGreaterThanOrEq);
  _make_binary_expr_test!(OpLessThan);
  _make_binary_expr_test!(OpLessThanOrEq);
  _make_binary_expr_test!(OpEq);
  _make_binary_expr_test!(OpStrictEq);
  _make_binary_expr_test!(OpNotEq);
  _make_binary_expr_test!(OpStrictNotEq);
  _make_binary_expr_test!(OpAnd);
  _make_binary_expr_test!(OpOr);
  _make_binary_expr_test!(OpXor);
  _make_binary_expr_test!(OpLogicalAnd);
  _make_binary_expr_test!(OpLogicalOr);
  _make_binary_expr_test!(OpNullCoalescing);

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

  macro_rules! _assignment_op_list {
    ($macro:ident) => {
      $macro!(OpLogicalOrAssign);
      $macro!(OpLogicalAndAssign);
      $macro!(OpMulAssign);
      $macro!(OpDivAssign);
      $macro!(OpModAssign);
      $macro!(OpPlusAssign);
      $macro!(OpMinusAssign);
      $macro!(OpAndAssign);
      $macro!(OpOrAssign);
      $macro!(OpXorAssign);
      $macro!(OpAssign);
      $macro!(OpShlAssign);
      $macro!(OpShrAssign);
      $macro!(OpUShrAssign);
      $macro!(OpPowAssign);
    };
  }

  _assignment_op_list!(_make_assignment_test);

  macro_rules! _make_assignment_early_error_test {
    ($token:expr) => {
      paste! {
        #[test]
        fn [<assignment_expr_primary_to_new_expr_early_error_test_ $token>]() {
          use Token::*;
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, &format!("this {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, &format!("'a' {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 1, &format!("1 {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 14,&format!("!function() {{}} {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 15, &format!("!function*() {{}} {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 20, &format!("!async function() {{}} {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 21, &format!("!async function*() {{}} {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, &format!("`abc` {} 1", $token.symbol()));
        }

        #[test]
        fn [<assignment_expr_call_expr_early_error_test_ $token>]() {
          use Token::*;
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, &format!("(((1))) {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, &format!("super() {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 13, &format!("import('abc') {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 4, &format!("x(1) {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 8, &format!("x()`abc` {} 1", $token.symbol()));
        }

        #[test]
        fn [<assignment_expr_new_to_left_hand_side_expr_early_error_test_ $token>]() {
          use Token::*;
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, &format!("new x {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, &format!("new x() {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 11, &format!("new new x() {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, &format!("a`aa` {} 1", $token.symbol()));
          let env_list = [Some(("!function X() {", "")), None, None];
          syntax_error_test(&env_list, &format!("new.target {} 1", $token.symbol()), &[&s_pos!(15, 25, 0, 0), &s_pos!(0, 0, 0, 0), &s_pos!(0, 0, 0, 0)], false);
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 11, &format!("import.meta {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 6, &format!("a.b?.c {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 10, &format!("a.b?.['c'] {} 1", $token.symbol()));
        }

        #[test]
        fn [<assignment_expr_update_to_unary_expr_early_error_test_ $token>]() {
          use Token::*;
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, &format!("++x {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, &format!("++x {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, &format!("x++ {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 3, &format!("x-- {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 10, &format!("delete a.b {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 6, &format!("void a {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 2, &format!("+a {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 2, &format!("-a {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 2, &format!("~a {} 1", $token.symbol()));
          basic_env_expression_eary_error_test(EnvFlag::all(), 0, 2, &format!("!a {} 1", $token.symbol()));
        }
      }
    };
  }

  _assignment_op_list!(_make_assignment_early_error_test);

  #[test]
  fn identifier_reference_strict_mode_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 4, "eval = 1");
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 9, "arguments = 1")
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
              if is_skip_parser { col + 7 } else { 0 },
              if is_skip_parser { col + 7 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("a", pos!(col + 2, line)),
              exprs!(pos!(col + 4, line)),
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
          object_props!(
            pos!(col + 10, line),
            ident!("b", pos!(col + 10, line)),
            fnexpr!(
              pos!(col + 10, line),
              "Function",
              if is_skip_parser { col + 15 } else { 0 },
              if is_skip_parser { col + 15 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("b", pos!(col + 10, line)),
              exprs!(pos!(col + 12, line)),
              if is_skip_parser { void!() } else { empty!() }
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
              if is_skip_parser { col + 11 } else { 0 },
              if is_skip_parser { col + 11 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("a", pos!(col + 6, line)),
              exprs!(pos!(col + 8, line)),
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
          object_props!(
            pos!(col + 14, line),
            ident!("b", pos!(col + 18, line)),
            getset_fnexpr!(
              pos!(col + 14, line),
              "Function",
              "set",
              if is_skip_parser { col + 24 } else { 0 },
              if is_skip_parser { col + 24 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              ident!("b", pos!(col + 18, line)),
              exprs!(pos!(col + 20, line), ident!("x", pos!(col + 20, line))),
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
        );
      },
      "({get a() {}, set b(x) {}})",
      2,
    )
  }

  #[test]
  fn parser_parse_object_literal_arrow_function_expr_value() {
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
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
        );
      },
      "({a: () => {}})",
      1,
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
              "Function",
              if is_skip_parser { col + 17 } else { 0 },
              if is_skip_parser { col + 17 } else { 0 },
              scope!(@opaque is_strict, 0, true),
              exprs!(pos!(col + 13, line)),
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
        );
      },
      "({a: function() {}})",
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
              if is_skip_parser { col + 13 } else { 0 },
              if is_skip_parser { col + 13 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 8, line)),
              exprs!(pos!(col + 10, line)),
              if is_skip_parser { void!() } else { empty!() }
            )
          ),
          object_props!(
            pos!(col + 16, line),
            ident!("b", pos!(col + 16, line)),
            fnexpr!(
              pos!(col + 16, line),
              "Function",
              if is_skip_parser { col + 21 } else { 0 },
              if is_skip_parser { col + 21 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("b", pos!(col + 16, line)),
              exprs!(pos!(col + 18, line)),
              if is_skip_parser { void!() } else { empty!() }
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
              if is_skip_parser { col + 13 } else { 0 },
              if is_skip_parser { col + 22 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 8, line)),
              exprs!(pos!(col + 10, line)),
              if is_skip_parser {
                void!()
              } else {
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
              if is_skip_parser { col + 8 } else { 0 },
              if is_skip_parser { col + 17 } else { 0 },
              scope!(@opaque is_strict,0, true),
              ident!("a", pos!(col + 3, line)),
              exprs!(pos!(col + 5, line)),
              if is_skip_parser {
                void!()
              } else {
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
              if is_skip_parser { col + 14 } else { 0 },
              if is_skip_parser { col + 35 } else { 0 },
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
    );
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
    );
  }

  #[test]
  fn object_pattern_spread_is_not_last_element_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 6, "([...a, b] = x)");
  }

  #[test]
  fn array_pattern_spread_is_not_identifier_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 11, "([...{a: b}] = x)");
  }

  #[test]
  fn object_pattern_spread_is_not_identifier_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 2, 11, "({...{a: b}} = x)");
  }

  #[test]
  fn function_has_duplicated_param_in_strict_mode_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 8, 9, "((a, b, b) => {})");
  }

  #[test]
  fn method_has_direct_super_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 10, 15, "({a(a, b, super) {}})");

    basic_env_expression_eary_error_test(EnvFlag::all(), 12, 17, "({a(a, b) { super() }})");
  }

  #[test]
  fn method_has_duplicated_parameters_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 7, 8, "({a(a, a) {}})");
  }

  #[test]
  fn method_has_duplicated_parameters_with_simple_object_pattern_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 8, 9, "({a(a, {a}) {}})");
  }

  #[test]
  fn method_has_duplicated_parameters_with_simple_object_pattern_error_test_2() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 8, 9, "({a(a, {a, b}) {}})");
  }

  #[test]
  fn method_has_duplicated_parameters_with_nested_object_pattern_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 16, 17, "({a(a, {b: {c: {a}}}) {}})");
  }

  #[test]
  fn method_has_duplicated_parameters_with_complex_pattern_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 39, 40, "({a(a, {[((a, b) => {})()]: [{c: {o: [{a}]}}]}) {}})");
  }

  #[test]
  fn method_has_duplicated_parameters_skip_parser_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 19, 20, "({a(a, b) { ({b(a, a) {}}) }})");
  }

  #[test]
  fn method_has_not_simple_parameter_but_declare_strict() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 10, 22, "({a({a}) {'use strict'}})");
  }

  #[test]
  fn setter_has_not_simple_parameter_but_declare_strict() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 14, 26, "({set a({a}) {'use strict'}})");
  }

  #[test]
  fn object_literal_has_propery_name_initializer_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 4, 5, "({a = 1, b})");
  }

  #[test]
  fn generator_method_not_allowed_to_use_yield_param_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 5, 10, "({*a(yield) {}})");
  }

  #[test]
  fn in_param_array_pattern_spread_is_not_identifier_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 5, 11, "({a([...{a}]) {}})");
  }

  #[test]
  fn in_param_object_pattern_spread_is_not_identifier_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 5, 11, "({a([...[a]]) {}})");
  }

  #[test]
  fn parser_parse_empty_class_expression() {
    single_expression_test(
      |(col, line, _, _)| {
        return class(pos!(col + 1, line), Some(ident!("A", pos!(col + 7, line))), None, &[], &[]);
      },
      "(class A {})",
    )
  }

  fn get_visibility_str(flag: ClassFieldFlag) -> String {
    let mut buf = Vec::new();
    if flag.contains(ClassFieldFlag::STATIC) {
      buf.push("static");
    }
    if flag.contains(ClassFieldFlag::PUBLIC) {
      buf.push("public");
    } else if flag.contains(ClassFieldFlag::PRIVATE) {
      buf.push("private");
    }
    return buf.join(" ").to_string();
  }

  fn class_field_without_value(attr: ClassFieldFlag, key_expr: &str, start_pos: u64, line: u32) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    return class_field!(
      &get_visibility_str(attr),
      pos!(start_pos, line),
      ident!(key_expr, pos!(start_pos + static_additional, line))
    );
  }

  fn class_field_with_value(attr: ClassFieldFlag, key_expr: &str, value_expr: &str, mut start_pos: u64, line: u32) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let key_len = key_expr.len() as u64;
    return class_field!(
      &get_visibility_str(attr),
      pos!(start_pos, line),
      binary!(
        "OpAssign",
        pos!(start_pos, line),
        ident!(key_expr, pos!(start_pos + static_additional, line)),
        number!(
          value_expr,
          pos!(
            start_pos + static_additional + key_len + if attr.contains(ClassFieldFlag::PRIVATE) { 4 } else { 3 },
            line
          )
        )
      )
    );
  }

  fn normal_method(
    attr: ClassFieldFlag,
    start_col: u64,
    is_strict: bool,
    name: &str,
    mut col: u64,
    line: u32,
    fn_start: u64,
    fn_end: u64,
  ) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let addition = if attr.contains(ClassFieldFlag::PRIVATE) { 1 } else { 0 };
    return class_field!(
      &get_visibility_str(attr),
      pos!(col, line),
      fnexpr!(
        pos!(col + static_additional, line),
        "Function",
        start_col + fn_start,
        start_col + fn_end,
        if is_strict {
          scope!(@opaque @strict 0, true)
        } else {
          scope!(@opaque 0, true)
        },
        ident!(name, pos!(col + static_additional, line)),
        exprs!(pos!(col + static_additional + (name.len() as u64) + addition + 1, line))
      )
    );
  }

  fn generator_method(
    attr: ClassFieldFlag,
    start_col: u64,
    is_strict: bool,
    name: &str,
    mut col: u64,
    line: u32,
    fn_start: u64,
    fn_end: u64,
  ) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let addition = if attr.contains(ClassFieldFlag::PRIVATE) { 1 } else { 0 };
    return class_field!(
      &get_visibility_str(attr),
      pos!(col, line),
      fnexpr!(
        pos!(col + static_additional, line),
        "Generator",
        start_col + fn_start,
        start_col + fn_end,
        if is_strict {
          scope!(@opaque @strict 0, true)
        } else {
          scope!(@opaque 0, true)
        },
        ident!(name, pos!(col + 1 + static_additional, line)),
        exprs!(pos!(col + static_additional + (name.len() as u64) + 2 + addition, line))
      )
    );
  }

  fn async_method(
    attr: ClassFieldFlag,
    start_col: u64,
    is_strict: bool,
    name: &str,
    mut col: u64,
    line: u32,
    fn_start: u64,
    fn_end: u64,
  ) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let async_len = ("async ".len() as u64);
    let addition = if attr.contains(ClassFieldFlag::PRIVATE) { 1 } else { 0 };
    return class_field!(
      &get_visibility_str(attr),
      pos!(col, line),
      afnexpr!(
        pos!(col + static_additional, line),
        "Function",
        start_col + fn_start,
        start_col + fn_end,
        if is_strict {
          scope!(@opaque @strict 0, true)
        } else {
          scope!(@opaque 0, true)
        },
        ident!(name, pos!(col + static_additional + async_len, line)),
        exprs!(pos!(col + static_additional + (name.len() as u64) + 1 + async_len + addition, line))
      )
    );
  }

  fn async_generator_method(
    attr: ClassFieldFlag,
    start_col: u64,
    is_strict: bool,
    name: &str,
    mut col: u64,
    line: u32,
    fn_start: u64,
    fn_end: u64,
  ) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let async_len = ("async *".len() as u64);
    let addition = if attr.contains(ClassFieldFlag::PRIVATE) { 1 } else { 0 };
    return class_field!(
      &get_visibility_str(attr),
      pos!(col, line),
      afnexpr!(
        pos!(col + static_additional, line),
        "Generator",
        start_col + fn_start,
        start_col + fn_end,
        if is_strict {
          scope!(@opaque @strict 0, true)
        } else {
          scope!(@opaque 0, true)
        },
        ident!(name, pos!(col + static_additional + async_len, line)),
        exprs!(pos!(col + static_additional + (name.len() as u64) + 1 + async_len + addition, line))
      )
    );
  }

  fn getset_method(
    getset: &str,
    attr: ClassFieldFlag,
    start_col: u64,
    is_strict: bool,
    name: &str,
    mut col: u64,
    line: u32,
    fn_start: u64,
    fn_end: u64,
  ) -> TestableAst {
    let static_additional = if attr.contains(ClassFieldFlag::STATIC) { STATIC_STR_LEN } else { 0 };
    let addition = if attr.contains(ClassFieldFlag::PRIVATE) { 1 } else { 0 };
    let getset_additional = (getset.len() as u64) + 1;
    return class_field!(
      &get_visibility_str(attr),
      pos!(col, line),
      getset_fnexpr!(
        pos!(col + getset_additional + static_additional, line),
        "Function",
        getset,
        start_col + fn_start,
        start_col + fn_end,
        if is_strict {
          scope!(@opaque @strict 0, true)
        } else {
          scope!(@opaque 0, true)
        },
        ident!(name, pos!(col + getset_additional + static_additional, line)),
        if getset.bytes().eq("set".bytes()) {
          exprs!(
            pos!(
              col + getset_additional + static_additional + (name.len() as u64) + 1 + addition,
              line
            ),
            ident!(
              "a",
              pos!(
                col + getset_additional + static_additional + (name.len() as u64) + 1 + addition,
                line
              )
            )
          )
        } else {
          exprs!(pos!(
            col + getset_additional + static_additional + (name.len() as u64) + 1 + addition,
            line
          ))
        }
      )
    );
  }

  macro_rules! cfa {
    ($($name:ident)|*$(|)*) => {{
      let mut attr = ClassFieldFlag::NONE;
      $(
        attr |= ClassFieldFlag::$name;
      )*
      attr
    }}
  }

  #[test]
  fn parser_parse_class_expression() {
    single_expression_test_with_options(
      |(col, line, is_strict, _)| {
        return class(
          pos!(col + 1, line),
          Some(ident!("A", pos!(col + 7, line))),
          None,
          &[
            class_field_without_value(ClassFieldFlag::PUBLIC, "field", 2, 5),
            class_field_with_value(ClassFieldFlag::PUBLIC, "field1", "1", 2, 6),
            class_field_with_value(ClassFieldFlag::PRIVATE, "priv_field", "1", 2, 7),
            class_field_without_value(ClassFieldFlag::PUBLIC | ClassFieldFlag::STATIC, "s_field", 2, 11),
            class_field_without_value(ClassFieldFlag::PRIVATE | ClassFieldFlag::STATIC, "s_priv_field", 2, 12),
            class_field_with_value(ClassFieldFlag::PUBLIC | ClassFieldFlag::STATIC, "s_field_v", "1", 2, 13),
            class_field_with_value(ClassFieldFlag::PRIVATE | ClassFieldFlag::STATIC, "s_priv_field_v", "1", 2, 14),
          ],
          &[
            normal_method(ClassFieldFlag::PUBLIC, col, is_strict, "fn", 2, 1, 19, 19),
            generator_method(ClassFieldFlag::PUBLIC, col, is_strict, "gen", 2, 2, 31, 31),
            async_method(ClassFieldFlag::PUBLIC, col, is_strict, "asy", 2, 3, 48, 48),
            async_generator_method(ClassFieldFlag::PUBLIC, col, is_strict, "asyg", 2, 4, 67, 67),
            normal_method(ClassFieldFlag::PRIVATE, col, is_strict, "priv_fn", 2, 8, 122, 122),
            generator_method(ClassFieldFlag::PRIVATE, col, is_strict, "priv_gen", 2, 9, 140, 140),
            async_generator_method(ClassFieldFlag::PRIVATE, col, is_strict, "priv_agen", 2, 10, 165, 165),
            normal_method(cfa!(PUBLIC | STATIC), col, is_strict, "s_fn", 2, 15, 276, 276),
            generator_method(cfa!(PUBLIC | STATIC), col, is_strict, "s_gen", 2, 16, 297, 297),
            async_method(cfa!(PUBLIC | STATIC), col, is_strict, "s_asy", 2, 17, 323, 323),
            async_generator_method(cfa!(PUBLIC | STATIC), col, is_strict, "s_asyg", 2, 18, 351, 351),
            normal_method(cfa!(PRIVATE | STATIC), col, is_strict, "s_p_fn", 2, 19, 373, 373),
            generator_method(cfa!(PRIVATE | STATIC), col, is_strict, "s_p_gen", 2, 20, 397, 397),
            async_method(cfa!(PRIVATE | STATIC), col, is_strict, "s_p_asy", 2, 21, 426, 426),
            async_generator_method(cfa!(PRIVATE | STATIC), col, is_strict, "s_p_asyg", 2, 22, 457, 457),
            getset_method("get", cfa!(PUBLIC), col, is_strict, "get_fn", 2, 23, 475, 475),
            getset_method("set", cfa!(PUBLIC), col, is_strict, "set_fn", 2, 24, 494, 494),
            getset_method("get", cfa!(PRIVATE), col, is_strict, "p_get_fn", 2, 25, 515, 515),
            getset_method("set", cfa!(PRIVATE), col, is_strict, "p_set_fn", 2, 26, 537, 537),
            getset_method("get", cfa!(PUBLIC | STATIC), col, is_strict, "s_get_fn", 2, 27, 564, 564),
            getset_method("set", cfa!(PUBLIC | STATIC), col, is_strict, "s_set_fn", 2, 28, 592, 592),
            getset_method("get", cfa!(PRIVATE | STATIC), col, is_strict, "s_p_get_fn", 2, 29, 622, 622),
            getset_method("set", cfa!(PRIVATE | STATIC), col, is_strict, "s_p_set_fn", 2, 30, 653, 653),
          ],
        );
      },
      "(class A {
  fn() {}
  *gen() {}
  async asy() {}
  async *asyg() {}
  field
  field1 = 1
  #priv_field = 1
  #priv_fn() {}
  *#priv_gen() {}
  async *#priv_agen() {}
  static s_field
  static #s_priv_field
  static s_field_v = 1
  static #s_priv_field_v = 1
  static s_fn() {}
  static *s_gen() {}
  static async s_asy() {}
  static async *s_asyg() {}
  static #s_p_fn() {}
  static *#s_p_gen() {}
  static async #s_p_asy() {}
  static async *#s_p_asyg() {}
  get get_fn() {}
  set set_fn(a) {}
  get #p_get_fn() {}
  set #p_set_fn(a) {}
  static get s_get_fn() {}
  static set s_set_fn(a) {}
  static get #s_p_get_fn() {}
  static set #s_p_set_fn(a) {}
})",
      ParserOption::default(),
      23,
      EnvFlag::all(),
      655,
    );
  }

  #[test]
  fn parser_parse_class_expression_with_heritage() {
    single_expression_test_with_options(
      |(col, line, is_strict, _)| {
        return class(
          pos!(col + 1, line),
          Some(ident!("A", pos!(col + 7, line))),
          Some(callexpr!(
            "Expr",
            pos!(col + 17, line),
            ident!("fn", pos!(col + 17, line)),
            exprs!(pos!(col + 19, line))
          )),
          &[
            class_field!("public", pos!(2, 5), ident!("field", pos!(2, 5))),
            class_field!(
              "public",
              pos!(2, 6),
              binary!("OpAssign", pos!(2, 6), ident!("field1", pos!(2, 6)), number!("1", pos!(11, 6)))
            ),
            class_field!(
              "private",
              pos!(2, 7),
              binary!("OpAssign", pos!(2, 7), ident!("priv_field", pos!(2, 7)), number!("1", pos!(16, 7)))
            ),
          ],
          &[
            class_field!(
              "public",
              pos!(2, 1),
              fnexpr!(
                pos!(2, 1),
                "Function",
                col + 32,
                col + 32,
                if is_strict {
                  scope!(@opaque @strict 0, true)
                } else {
                  scope!(@opaque 0, true)
                },
                ident!("fn", pos!(2, 1)),
                exprs!(pos!(5, 1))
              )
            ),
            class_field!(
              "public",
              pos!(2, 2),
              fnexpr!(
                pos!(2, 2),
                "Generator",
                col + 44,
                col + 44,
                if is_strict {
                  scope!(@opaque @strict 0, true)
                } else {
                  scope!(@opaque 0, true)
                },
                ident!("gen", pos!(3, 2)),
                exprs!(pos!(7, 2))
              )
            ),
            class_field!(
              "public",
              pos!(2, 3),
              afnexpr!(
                pos!(2, 3),
                "Function",
                col + 61,
                col + 61,
                if is_strict {
                  scope!(@opaque @strict 0, true)
                } else {
                  scope!(@opaque 0, true)
                },
                ident!("asy", pos!(8, 3)),
                exprs!(pos!(12, 3))
              )
            ),
            class_field!(
              "public",
              pos!(2, 4),
              afnexpr!(
                pos!(2, 4),
                "Generator",
                col + 80,
                col + 80,
                if is_strict {
                  scope!(@opaque @strict 0, true)
                } else {
                  scope!(@opaque 0, true)
                },
                ident!("asyg", pos!(9, 4)),
                exprs!(pos!(14, 4))
              )
            ),
            class_field!(
              "private",
              pos!(2, 8),
              fnexpr!(
                pos!(2, 8),
                "Function",
                col + 135,
                col + 135,
                if is_strict {
                  scope!(@opaque @strict 0, true)
                } else {
                  scope!(@opaque 0, true)
                },
                ident!("priv_fn", pos!(2, 8)),
                exprs!(pos!(11, 8))
              )
            ),
          ],
        );
      },
      "(class A extends fn() {
  fn() {}
  *gen() {}
  async asy() {}
  async *asyg() {}
  field
  field1 = 1
  #priv_field = 1
  #priv_fn() {}
})",
      ParserOption::default(),
      5,
      EnvFlag::all(),
      137,
    );
  }

  #[test]
  fn parse_class_constructor_has_direct_super_with_heritage() {
    single_expression_test_with_options(
      |(col, line, is_strict, _)| {
        return class(
          pos!(col + 1, line),
          Some(ident!("K", pos!(col + 7, line))),
          Some(ident!("A", pos!(col + 17, line))),
          &[],
          &[normal_method(cfa!(PUBLIC), col, is_strict, "constructor", col + 20, line, 35, 42)],
        );
      },
      "(class K extends A {constructor() {super()}})",
      ParserOption::default(),
      1,
      EnvFlag::all(),
      0,
    );
  }

  #[test]
  fn parse_class_constructor_has_direct_super_with_heritage_without_identifier() {
    single_expression_test_with_options(
      |(col, line, is_strict, _)| {
        return class(
          pos!(col + 1, line),
          None,
          Some(ident!("A", pos!(col + 15, line))),
          &[],
          &[normal_method(cfa!(PUBLIC), col, is_strict, "constructor", col + 18, line, 33, 40)],
        );
      },
      "(class extends A {constructor() {super()}})",
      ParserOption::default(),
      1,
      EnvFlag::all(),
      0,
    );
  }

  #[test]
  fn class_constructor_has_direct_super_without_heritage_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 23, 28, "(class {constructor() {super()}})");
  }

  #[test]
  fn class_constructor_declared_twice_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 25, 36, "(class {constructor() {} constructor() {}})");
  }

  #[test]
  fn class_constructor_is_special_method_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 14, 25, "(class {async constructor() {}})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 9, 20, "(class {*constructor() {}})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 26, "(class {async *constructor() {}})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 12, 23, "(class {get constructor() {}})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 12, 23, "(class {set constructor(a) {}})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 8, 20, "(class {#constructor() {}})");
  }

  #[test]
  fn class_method_has_direct_super_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 14, 19, "(class {fn() {super()}})");
  }

  #[test]
  fn static_class_method_name_is_prototype_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 24, "(class {static prototype() {}})");
  }

  #[test]
  fn parse_function_expression() {
    single_expression_test_with_scope(
      |(col, line, is_strict, _)| {
        return fnexpr!(
          pos!(col + 1, line),
          "Function",
          0,
          0,
          if is_strict {
            scope!(@opaque @strict 0, true)
          } else {
            scope!(@opaque 0, true)
          },
          exprs!(pos!(col + 9, line)),
          empty!()
        );
      },
      "(function() {})",
      1,
    )
  }

  #[test]
  fn parse_generator_function_expression() {
    single_expression_test_with_scope(
      |(col, line, is_strict, _)| {
        return fnexpr!(
          pos!(col + 1, line),
          "Generator",
          0,
          0,
          if is_strict {
            scope!(@opaque @strict 0, true)
          } else {
            scope!(@opaque 0, true)
          },
          exprs!(pos!(col + 10, line)),
          empty!()
        );
      },
      "(function*() {})",
      1,
    )
  }

  #[test]
  fn parse_async_function_expression() {
    single_expression_test_with_scope(
      |(col, line, is_strict, _)| {
        return afnexpr!(
          pos!(col + 1, line),
          "Function",
          0,
          0,
          if is_strict {
            scope!(@opaque @strict 0, true)
          } else {
            scope!(@opaque 0, true)
          },
          exprs!(pos!(col + 15, line)),
          empty!()
        );
      },
      "(async function() {})",
      1,
    )
  }

  #[test]
  fn parse_async_generator_function_expression() {
    single_expression_test_with_scope(
      |(col, line, is_strict, _)| {
        return afnexpr!(
          pos!(col + 1, line),
          "Generator",
          0,
          0,
          if is_strict {
            scope!(@opaque @strict 0, true)
          } else {
            scope!(@opaque 0, true)
          },
          exprs!(pos!(col + 16, line)),
          empty!()
        );
      },
      "(async function*() {})",
      1,
    )
  }

  #[test]
  fn parse_function_parameter_pattern() {
    single_expression_test_with_scope(
      |(col, line, is_strict, _)| {
        return fnexpr!(
          pos!(col + 1, line),
          "Function",
          0,
          0,
          if is_strict {
            scope!(@opaque @strict 0, true)
          } else {
            scope!(@opaque 0, true)
          },
          ident!("X", pos!(col + 10, line)),
          exprs!(
            pos!(col + 11, line),
            ident!("a", pos!(col + 12, line)),
            objectlit!(
              ObjectLitType::NONE,
              pos!(col + 15, line),
              object_props!(pos!(col + 16, line), ident!("b", pos!(col + 16, line))),
              object_props!(
                pos!(col + 19, line),
                ident!("test", pos!(col + 19, line)),
                objectlit!(
                  ObjectLitType::NONE,
                  pos!(col + 25, line),
                  object_props!(pos!(col + 26, line), ident!("c", pos!(col + 26, line)))
                )
              )
            ),
            arraylit!(
              false,
              pos!(col + 31, line),
              ident!("d", pos!(col + 32, line)),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + 35, line),
                object_props!(
                  pos!(col + 36, line),
                  ident!("e", pos!(col + 36, line)),
                  ident!("f", pos!(col + 39, line)),
                ),
              )
            ),
            binary!(
              Token::OpAssign,
              pos!(col + 44, line),
              ident!("k", pos!(col + 44, line)),
              number!("1", pos!(col + 48, line))
            ),
            binary!(
              Token::OpAssign,
              pos!(col + 51, line),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + 51, line),
                object_props!(
                  pos!(col + 52, line),
                  ident!("j", pos!(col + 52, line)),
                  objectlit!(
                    ObjectLitType::NONE,
                    pos!(col + 55, line),
                    object_props!(pos!(col + 56, line), ident!("l", pos!(col + 56, line)))
                  )
                )
              ),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + 62, line),
                object_props!(
                  pos!(col + 63, line),
                  ident!("j", pos!(col + 63, line)),
                  objectlit!(
                    ObjectLitType::NONE,
                    pos!(col + 66, line),
                    object_props!(
                      pos!(col + 67, line),
                      ident!("l", pos!(col + 67, line)),
                      number!("1", pos!(col + 70, line))
                    )
                  )
                )
              )
            ),
            unary!("Spread", "Pre", pos!(col + 75, line), ident!("g", pos!(col + 78, line)))
          ),
          empty!()
        );
      },
      "(function X(a, {b, test: {c}}, [d, {e: f}], k = 1, {j: {l}} = {j: {l: 1}}, ...g) {})",
      1,
    )
  }

  #[test]
  fn function_strict_mode_unique_parameter_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 13, 14, "(function(a, a, b) {})");

    basic_env_expression_eary_error_test(
      EnvFlag::STRICT_MODE,
      54,
      55,
      "(function(a, b = {a}, {[((a, b) => {})()]: [{c: {o: [{a}]}}]}) {})",
    );
  }

  #[test]
  fn function_indentifier_eval_or_arguments_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 10, 14, "(function eval() {})");
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 10, 19, "(function arguments() {})");
  }

  #[test]
  fn function_arguments_is_not_simple_but_strict_mode_declared_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 17, 29, "(function ({a}) {'use strict';})");
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 19, 31, "(function (a = 1) {'use strict';})");
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 18, 30, "(function (...a) {'use strict';})");
  }

  #[test]
  fn function_body_contains_lexical_binding_same_key_of_formal_params_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 19, 20, "(function (a) {let a = 0;})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 45, 46, "(function ({a: {b: {c}}}) {let a = 0, b = 0, c = 0})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 26, 27, "(function ([a, b]) {const b = 1})");
  }

  #[test]
  fn function_contains_super_property_or_call_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 20, "(function (a = super.a) {})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 20, "(function (a = super()) {})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 20, "(function (a) {super()})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 20, "(function (a) {super.a})");
    basic_env_expression_eary_error_test(EnvFlag::all(), 11, 16, "(function (super) {})");
  }

  #[test]
  fn parse_lexical_or_var_binding() {
    macro_rules! let_or_const {
      ($type:expr, $type_l:expr) => {
        let len = ($type_l.len() + 1) as u64;
        stmt_test(
          |(col, line, _, _)| {
            return var!(
              $type,
              pos!(col, line),
              ident!("a", pos!(col + len, line)),
              number!("1", pos!(col + 4 + len, line))
            );
          },
          concat!($type_l, " a = 1"),
        );
      };
    }
    let_or_const!("Let", "let");
    let_or_const!("Const", "const");
    let_or_const!("Var", "var");
  }

  #[test]
  fn parse_lexical_or_var_pattern_binding() {
    macro_rules! let_or_const {
      ($type:expr, $type_l:expr) => {
        let len = ($type_l.len() + 1) as u64;
        stmt_test(
          |(col, line, _, _)| {
            return var!(
              $type,
              pos!(col, line),
              objectlit!(
                ObjectLitType::NONE,
                pos!(col + len, line),
                object_props!(
                  pos!(col + 1 + len, line),
                  ident!("a", pos!(col + 1 + len, line)),
                  objectlit!(
                    ObjectLitType::NONE,
                    pos!(col + 4 + len, line),
                    object_props!(
                      pos!(col + 5 + len, line),
                      ident!("b", pos!(col + 5 + len, line)),
                      number!("1", pos!(col + 9 + len, line))
                    )
                  )
                )
              ),
              ident!("a", pos!(col + 15 + len, line))
            );
          },
          concat!($type_l, " {a: {b = 1}} = a"),
        );
      };
    }
    let_or_const!("Let", "let");
    let_or_const!("Const", "const");
    let_or_const!("Var", "var");
  }

  #[test]
  fn parse_lexical_or_var_bindings() {
    macro_rules! let_or_const {
      ($type:expr, $type_l:expr) => {
        let len = ($type_l.len() + 1) as u64;
        stmt_test(
          |(col, line, _, _)| {
            return vars!(
              pos!(col, line),
              var!(
                $type,
                pos!(col, line),
                ident!("a", pos!(col + len, line)),
                number!("1", pos!(col + 4 + len, line))
              ),
              var!(
                $type,
                pos!(col, line),
                ident!("b", pos!(col + 7 + len, line)),
                number!("2", pos!(col + 11 + len, line))
              ),
              var!(
                $type,
                pos!(col, line),
                objectlit!(
                  ObjectLitType::NONE,
                  pos!(col + 14 + len, line),
                  object_props!(pos!(col + 15 + len, line), ident!("c", pos!(col + 15 + len, line)),)
                ),
                ident!("x", pos!(col + 20 + len, line))
              )
            );
          },
          concat!($type_l, " a = 1, b = 2, {c} = x"),
        );
      };
    }
    let_or_const!("Let", "let");
    let_or_const!("Const", "const");
    let_or_const!("Var", "var");
  }

  #[test]
  fn parse_duplicated_var_binding() {
    stmt_test(
      |(col, line, _, _)| {
        return vars!(
          pos!(col, line),
          var!(
            "Var",
            pos!(col, line),
            ident!("a", pos!(col + 4, line)),
            number!("1", pos!(col + 8, line))
          ),
          var!(
            "Var",
            pos!(col, line),
            ident!("a", pos!(col + 11, line)),
            number!("1", pos!(col + 15, line))
          ),
        );
      },
      "var a = 1, a = 1;",
    );
  }

  #[test]
  fn const_requires_initializer_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 7, "const a");
  }

  #[test]
  fn lexical_binding_not_allowed_duplication_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 14, 15, "let a = 1, b, a = 3");
    basic_env_expression_eary_error_test(EnvFlag::all(), 20, 21, "const a = 1, b = 2, a = 3");
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 16, "let a = 1; var a = 2;");
    basic_env_expression_eary_error_test(EnvFlag::all(), 17, 18, "const a = 1; var a = 2;");
  }

  #[test]
  fn lexical_binding_not_allowed_let_as_identifier_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 4, 7, "let let = 1");
    basic_env_expression_eary_error_test(EnvFlag::all(), 6, 9, "const let = 1");
  }

  #[test]
  fn parse_empty_block_stmt() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return block!(pos!(col, line), scope!(@lexical is_strict, 0, true), empty!());
      },
      "{}",
      1,
    )
  }

  #[test]
  fn parse_block_stmt() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return block!(
          pos!(col, line),
          scope!(@lexical is_strict, 0, true),
          var!(
            "Let",
            pos!(col + 1, line),
            ident!("a", pos!(col + 5, line)),
            number!("1", pos!(col + 9, line))
          )
        );
      },
      "{let a = 1;}",
      1,
    )
  }

  #[test]
  fn parse_netsted_block_stmt() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return block!(
          pos!(col, line),
          scope!(@lexical is_strict, 1, true),
          stmts!(
            pos!(col + 1, line),
            var!(
              "Let",
              pos!(col + 1, line),
              ident!("a", pos!(col + 5, line)),
              number!("1", pos!(col + 9, line))
            ),
            block!(
              pos!(col + 12, line),
              scope!(@lexical is_strict, 0, true),
              var!(
                "Let",
                pos!(col + 13, line),
                ident!("a", pos!(col + 17, line)),
                number!("1", pos!(col + 21, line))
              )
            )
          )
        );
      },
      "{let a = 1; {let a = 1}}",
      1,
    )
  }

  #[test]
  fn lexical_scope_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 15, 16, "{let a = 1, b, a = 3}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 21, 22, "{const a = 1, b = 2, a = 3}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 16, 17, "{let a = 1; var a = 2;}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 18, 19, "{const a = 1; var a = 2;}");
  }

  #[test]
  fn parse_labelled_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return label!(
          pos!(col, line),
          ident!("X", pos!(col, line)),
          block!(
            pos!(col + 3, line),
            scope!(@lexical is_strict, 1, true),
            label!(
              pos!(col + 4, line),
              ident!("Y", pos!(col + 4, line)),
              block!(
                pos!(col + 7, line),
                scope!(@lexical is_strict, 1, true),
                label!(
                  pos!(col + 8, line),
                  ident!("Z", pos!(col + 8, line)),
                  block!(pos!(col + 11, line), scope!(@lexical is_strict, 0, true), empty!())
                )
              )
            )
          )
        );
      },
      "X: {Y: {Z: {}}}",
      1,
    )
  }

  #[test]
  fn parse_parallel_labelled_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return block!(
          pos!(col, line),
          scope!(@lexical is_strict, 2, true),
          stmts!(
            pos!(col + 1, line),
            label!(
              pos!(col + 1, line),
              ident!("X", pos!(col + 1, line)),
              block!(pos!(col + 4, line), scope!(@lexical is_strict, 0, true), empty!())
            ),
            label!(
              pos!(col + 7, line),
              ident!("X", pos!(col + 7, line)),
              block!(pos!(col + 10, line), scope!(@lexical is_strict, 0, true), empty!())
            )
          )
        );
      },
      "{X: {} X: {}}",
      1,
    )
  }

  #[test]
  fn duplicated_label_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 4, 5, "X: {X: {}}");
  }

  #[test]
  fn parse_if_statement() {
    stmt_test_with_linebreak(
      |(col, line, _, _)| {
        return if_stmt!(
          pos!(col, 0),
          ident!("a", pos!(col + 4, 0)),
          stmt!(
            pos!(2, 1),
            binary!("OpAssign", pos!(2, 1), ident!("x", pos!(2, 1)), number!("1", pos!(6, 1)))
          ),
          if_stmt!(
            pos!(2, 3),
            ident!("b", pos!(6, 3)),
            stmt!(
              pos!(4, 4),
              binary!("OpAssign", pos!(4, 4), ident!("x", pos!(4, 4)), number!("2", pos!(8, 4)))
            ),
            stmt!(
              pos!(4, 6),
              binary!("OpAssign", pos!(4, 6), ident!("x", pos!(4, 6)), number!("3", pos!(8, 6)))
            ),
          )
        );
      },
      "if (a)
  x = 1
else
  if (b)
    x = 2
  else
    x = 3",
      0,
      46,
    )
  }

  #[test]
  fn parse_if_block_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return if_stmt!(
          pos!(col, 0),
          ident!("a", pos!(col + 4, 0)),
          block!(
            pos!(col + 7, line),
            scope!(@lexical is_strict, 0, true),
            stmt!(
              pos!(col + 8, line),
              binary!(
                "OpAssign",
                pos!(col + 8, line),
                ident!("x", pos!(col + 8, line)),
                number!("1", pos!(col + 12, line))
              )
            )
          ),
          if_stmt!(
            pos!(col + 20, line),
            ident!("b", pos!(col + 24, line)),
            block!(
              pos!(col + 27, line),
              scope!(@lexical is_strict, 0, true),
              stmt!(
                pos!(col + 28, line),
                binary!(
                  "OpAssign",
                  pos!(col + 28, line),
                  ident!("x", pos!(col + 28, line)),
                  number!("2", pos!(col + 32, line))
                )
              )
            ),
            block!(
              pos!(col + 40, line),
              scope!(@lexical is_strict, 0, true),
              stmt!(
                pos!(col + 41, line),
                binary!(
                  "OpAssign",
                  pos!(col + 41, line),
                  ident!("x", pos!(col + 41, line)),
                  number!("3", pos!(col + 45, line))
                )
              )
            ),
          )
        );
      },
      "if (a) {x = 1} else if (b) {x = 2} else {x = 3}",
      3,
    )
  }

  #[test]
  fn labelled_fn_contains_if_stmt_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 7, 24, "if (a) X:function K() {}")
  }

  #[test]
  fn parse_switch_statement() {
    stmt_test_with_linebreak(
      |(col, line, is_strict, _)| {
        return switch!(
          pos!(col, line),
          scope!(@lexical is_strict, 0, true),
          ident!("a", pos!(col + 8, line)),
          case!(
            pos!(2, 1),
            number!("1", pos!(7, 1)),
            stmt!(
              pos!(4, 2),
              binary!("OpAssign", pos!(4, 2), ident!("a", pos!(4, 2)), number!("2", pos!(8, 2)))
            )
          ),
          case!(
            pos!(2, 3),
            number!("2", pos!(7, 3)),
            stmt!(
              pos!(4, 4),
              binary!("OpAssign", pos!(4, 4), ident!("a", pos!(4, 4)), number!("3", pos!(8, 4)))
            )
          ),
          case!(
            @default,
            pos!(2, 5),
            stmt!(
              pos!(4, 6),
              binary!("OpAssign", pos!(4, 6), ident!("a", pos!(4, 6)), number!("4", pos!(8, 6)))
            )
          ),
        );
      },
      "switch (a) {
  case 1:
    a = 2;
  case 2:
    a = 3;
  default:
    a = 4;
}",
      1,
      77,
    )
  }

  #[test]
  fn parse_switch_statement_with_block() {
    stmt_test_with_linebreak(
      |(col, line, is_strict, _)| {
        return switch!(
          pos!(col, line),
          scope!(@lexical is_strict, 2, true),
          ident!("a", pos!(col + 8, line)),
          case!(
            pos!(2, 1),
            number!("1", pos!(7, 1)),
            block!(
              pos!(10, 1),
              scope!(@lexical is_strict, 0, true),
              stmts!(
                pos!(4, 2),
                var!("Let", pos!(4, 2), ident!("v", pos!(8, 2)), number!("1", pos!(12, 2))),
                stmt!(
                  pos!(4, 3),
                  binary!("OpAssign", pos!(4, 3), ident!("a", pos!(4, 3)), number!("2", pos!(8, 3)))
                )
              )
            )
          ),
          case!(
            pos!(2, 5),
            number!("2", pos!(7, 5)),
            block!(
              pos!(10, 5),
              scope!(@lexical is_strict, 0, true),
              stmts!(
                pos!(4, 6),
                var!("Let", pos!(4, 6), ident!("v", pos!(8, 6)), number!("1", pos!(12, 6))),
                stmt!(
                  pos!(4, 7),
                  binary!("OpAssign", pos!(4, 7), ident!("a", pos!(4, 7)), number!("3", pos!(8, 7)))
                )
              )
            )
          ),
          case!(
            @default,
            pos!(2, 9),
            stmts!(
              pos!(4, 10),
              var!(
                "Let",
                pos!(4, 10),
                ident!("v", pos!(8, 10)),
                number!("1", pos!(12, 10))
              ),
              stmt!(
                pos!(4, 11),
                binary!("OpAssign", pos!(4, 11), ident!("a", pos!(4, 11)), number!("4", pos!(8, 11)))
              )
            )
          ),
          case!(
            pos!(2, 12),
            number!("3", pos!(7, 12)),
            stmts!(
              pos!(4, 13),
              var!("Let", pos!(4, 13), ident!("w", pos!(8, 13)), number!("1", pos!(12, 13))),
              stmt!(
                pos!(4, 14),
                binary!("OpAssign", pos!(4, 14), ident!("a", pos!(4, 14)), number!("3", pos!(8, 14)))
              )
            )
          ),
        );
      },
      "switch (a) {
  case 1: {
    let v = 1;
    a = 2;
  }
  case 2: {
    let v = 1;
    a = 3;
  }
  default:
    let v = 1;
    a = 4;
  case 3:
    let w = 1;
    a = 3;
}",
      1,
      170,
    )
  }

  #[test]
  fn switch_stmt_block_duplicated_lexical_binding_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 43, 44, "switch (a) {case 1: let a = 1; case 2: let a = 2}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 43, 44, "switch (a) {case 1: var a = 1; case 2: let a = 2}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 43, 44, "switch (a) {case 1: let a = 1; case 2: var a = 2}");

    basic_env_expression_eary_error_test(EnvFlag::all(), 47, 48, "switch (a) {case 1: const a = 1; case 2: const a = 2}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 47, 48, "switch (a) {case 1: const a = 1; case 2: const a = 2}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 47, 48, "switch (a) {case 1: const a = 1; case 2: const a = 2}");
  }

  #[test]
  fn parse_while_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return while_stmt!(
          pos!(col, line),
          number!("1", pos!(col + 7, line)),
          block!(
            pos!(col + 10, line),
            scope!(@lexical is_strict, 0, true),
            var!(
              "Let",
              pos!(col + 11, line),
              ident!("a", pos!(col + 15, line)),
              number!("0", pos!(col + 19, line))
            )
          )
        );
      },
      "while (1) {let a = 0;}",
      1,
    )
  }

  #[test]
  fn parse_do_while_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return do_while!(
          pos!(col, line),
          number!("1", pos!(col + 22, line)),
          block!(
            pos!(col + 3, line),
            scope!(@lexical is_strict, 0, true),
            var!(
              "Let",
              pos!(col + 4, line),
              ident!("a", pos!(col + 8, line)),
              number!("0", pos!(col + 12, line))
            )
          )
        );
      },
      "do {let a = 0;} while(1)",
      1,
    )
  }

  #[test]
  fn parse_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Var",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 16, line),
            ident!("i", pos!(col + 16, line)),
            number!("10", pos!(col + 20, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 24, line), ident!("i", pos!(col + 24, line))),
          block!(pos!(col + 29, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (var i = 0; i < 10; i++) {}",
      1,
    )
  }

  #[test]
  fn parse_lexical_declaration_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 16, line),
            ident!("i", pos!(col + 16, line)),
            number!("10", pos!(col + 20, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 24, line), ident!("i", pos!(col + 24, line))),
          block!(pos!(col + 29, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (let i = 0; i < 10; i++) {}",
      1,
    )
  }

  #[test]
  fn parse_expression_declaration_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          binary!(
            "OpAssign",
            pos!(col + 5, line),
            ident!("i", pos!(col + 5, line)),
            number!("0", pos!(col + 9, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 12, line),
            ident!("i", pos!(col + 12, line)),
            number!("10", pos!(col + 16, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 20, line), ident!("i", pos!(col + 20, line))),
          block!(pos!(col + 25, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (i = 0; i < 10; i++) {}",
      1,
    )
  }

  #[test]
  fn parse_for_iteration_empty_declaration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          empty!(),
          binary!(
            "OpLessThan",
            pos!(col + 7, line),
            ident!("i", pos!(col + 7, line)),
            number!("10", pos!(col + 11, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 15, line), ident!("i", pos!(col + 15, line))),
          block!(pos!(col + 20, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (; i < 10; i++) {}",
      1,
    )
  }

  #[test]
  fn parse_empty_condition_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          empty!(),
          unary!("OpIncrement", "Post", pos!(col + 16, line), ident!("i", pos!(col + 16, line))),
          block!(pos!(col + 21, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (let i = 0;;i++) {}",
      1,
    )
  }

  #[test]
  fn parse_empty_computation_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 16, line),
            ident!("i", pos!(col + 16, line)),
            number!("10", pos!(col + 20, line))
          ),
          empty!(),
          block!(pos!(col + 25, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (let i = 0; i < 10;) {}",
      1,
    )
  }

  #[test]
  fn parse_all_empty_for_iteration_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          empty!(),
          empty!(),
          empty!(),
          block!(pos!(col + 9, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (;;) {}",
      1,
    )
  }

  #[test]
  fn parse_lhs_expr_for_in_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forin!(
          pos!(col, line),
          ident!("x", pos!(col + 5, line)),
          ident!("obj", pos!(col + 10, line)),
          block!(pos!(col + 15, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (x in obj) {}",
      1,
    )
  }

  #[test]
  fn parse_lexical_lhs_for_in_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forin!(
          pos!(col, line),
          var!("Let", pos!(col + 5, line), ident!("x", pos!(col + 9, line))),
          ident!("obj", pos!(col + 14, line)),
          block!(pos!(col + 19, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (let x in obj) {}",
      1,
    )
  }

  #[test]
  fn parse_var_lhs_for_in_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forin!(
          pos!(col, line),
          var!("Var", pos!(col + 5, line), ident!("x", pos!(col + 9, line))),
          ident!("obj", pos!(col + 14, line)),
          block!(pos!(col + 19, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (var x in obj) {}",
      1,
    )
  }

  #[test]
  fn parse_lhs_expr_for_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          pos!(col, line),
          ident!("x", pos!(col + 5, line)),
          ident!("obj", pos!(col + 10, line)),
          block!(pos!(col + 15, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (x of obj) {}",
      1,
    )
  }

  #[test]
  fn parse_lexical_lhs_for_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          pos!(col, line),
          var!("Let", pos!(col + 5, line), ident!("x", pos!(col + 9, line))),
          ident!("obj", pos!(col + 14, line)),
          block!(pos!(col + 19, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (let x of obj) {}",
      1,
    )
  }

  #[test]
  fn parse_var_lhs_for_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          pos!(col, line),
          var!("Var", pos!(col + 5, line), ident!("x", pos!(col + 9, line))),
          ident!("obj", pos!(col + 14, line)),
          block!(pos!(col + 19, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for (var x of obj) {}",
      1,
    )
  }

  #[test]
  fn parse_lhs_expr_for_await_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          @await,
          pos!(col, line),
          ident!("x", pos!(col + 11, line)),
          ident!("obj", pos!(col + 16, line)),
          block!(pos!(col + 21, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for await (x of obj) {}",
      1,
    )
  }

  #[test]
  fn parse_lexical_lhs_for_await_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          @await,
          pos!(col, line),
          var!("Let", pos!(col + 11, line), ident!("x", pos!(col + 15, line))),
          ident!("obj", pos!(col + 20, line)),
          block!(pos!(col + 25, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for await (let x of obj) {}",
      1,
    )
  }

  #[test]
  fn parse_var_lhs_for_await_of_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return forof!(
          @await,
          pos!(col, line),
          var!("Var", pos!(col + 11, line), ident!("x", pos!(col + 15, line))),
          ident!("obj", pos!(col + 20, line)),
          block!(pos!(col + 25, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "for await (var x of obj) {}",
      1,
    )
  }

  #[test]
  fn labelled_fn_contains_for_stmt_early_error_test() {
    let env = [(Some(""), ""), (None, ""), (None, "")];
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 29, 46, "for (var i = 0; i < 10; i++) X:function K() {}");
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 19, 36, "for (const k in v) X:function K() {}");
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 19, 36, "for (const k of v) X:function K() {}");
  }

  #[test]
  fn lexical_duplicated_name_in_for_stmt_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 33, 34, "for (let i = 0; i < 10; i++) var i = 0");
    basic_env_expression_eary_error_test(EnvFlag::all(), 21, 22, "for (let i in k) var i = 0");
    basic_env_expression_eary_error_test(EnvFlag::all(), 21, 22, "for (let i of k) var i = 0");
    basic_env_expression_eary_error_test(EnvFlag::all(), 9, 12, "for (let let in k) var i = 0");
    basic_env_expression_eary_error_test(EnvFlag::all(), 9, 12, "for (let let of k) var i = 0");
    basic_env_expression_eary_error_test(EnvFlag::all(), 13, 14, "for (let {i, i} in k) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 13, 14, "for (let {i, i} of k) {}");
  }

  #[test]
  fn invalid_pattern_in_for_in_of_stmt_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 13, 16, "for (let {a: x()} in k) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 13, 16, "for (let {a: x()} of k) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 19, 22, "for await (let {a: x()} of k) {}");
  }

  #[test]
  fn invalid_assignment_target_type_in_for_in_of_stmt_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 5, 8, "for (a++ in k) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 5, 8, "for (a++ of k) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 11, 14, "for await (a++ of k) {}");
  }

  #[test]
  fn parse_break_statement_in_while() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return while_stmt!(
          pos!(col, line),
          number!("1", pos!(col + 7, line)),
          block!(
            pos!(col + 10, line),
            scope!(@lexical is_strict, 0, true),
            break_stmt!(pos!(col + 11, line))
          )
        );
      },
      "while (1) {break;}",
      1,
    )
  }

  #[test]
  fn parse_break_statement_in_do_while() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return do_while!(
          pos!(col, line),
          number!("1", pos!(col + 19, line)),
          block!(
            pos!(col + 3, line),
            scope!(@lexical is_strict, 0, true),
            break_stmt!(pos!(col + 4, line))
          )
        );
      },
      "do {break;} while (1)",
      1,
    )
  }

  #[test]
  fn parse_break_statement_in_for() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 16, line),
            ident!("i", pos!(col + 16, line)),
            number!("10", pos!(col + 20, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 24, line), ident!("i", pos!(col + 24, line))),
          block!(
            pos!(col + 29, line),
            scope!(@lexical is_strict, 0, true),
            break_stmt!(pos!(col + 30, line))
          )
        );
      },
      "for (let i = 0; i < 10; i++) {break;}",
      1,
    )
  }

  #[test]
  fn parse_break_statement_in_switch() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return switch!(
          pos!(col, line),
          scope!(@lexical is_strict, 0, true),
          ident!("a", pos!(col + 8, line)),
          case!(
            pos!(col + 12, line),
            number!("1", pos!(col + 17, line)),
            break_stmt!(pos!(col + 20, line))
          ),
        );
      },
      "switch (a) {case 1: break;}",
      1,
    )
  }

  #[test]
  fn parse_nested_break_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return while_stmt!(
          pos!(col, line),
          number!("1", pos!(col + 7, line)),
          block!(
            pos!(col + 10, line),
            scope!(@lexical is_strict, 1, true),
            stmt!(
              pos!(col + 11, line),
              unary!(
                "OpNot",
                "Pre",
                pos!(col + 11, line),
                fnexpr!(
                  pos!(col + 12, line),
                  "Function",
                  0,
                  0,
                  scope!(@opaque is_strict, 1, true),
                  ident!("m", pos!(col + 21, line)),
                  exprs!(pos!(col + 22, line)),
                  while_stmt!(
                    pos!(col + 26, line),
                    number!("1", pos!(col + 33, line)),
                    block!(
                      pos!(col + 36, line),
                      scope!(@lexical is_strict, 0, true),
                      break_stmt!(pos!(col + 37, line))
                    )
                  )
                )
              )
            )
          )
        );
      },
      "while (1) {!function m() {while (1) {break;}}}",
      1,
    )
  }

  #[test]
  fn parse_break_statement_in_while_with_label() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return label!(
          pos!(col, line),
          ident!("X", pos!(col, line)),
          while_stmt!(
            pos!(col + 3, line),
            number!("1", pos!(col + 10, line)),
            block!(
              pos!(col + 13, line),
              scope!(@lexical is_strict, 0, true),
              break_stmt!(pos!(col + 14, line), "X")
            )
          )
        );
      },
      "X: while (1) {break X;}",
      1,
    )
  }

  #[test]
  fn parse_break_statement_in_while_with_nested_label() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return label!(
          pos!(col, line),
          ident!("X", pos!(col, line)),
          while_stmt!(
            pos!(col + 3, line),
            number!("1", pos!(col + 10, line)),
            block!(
              pos!(col + 13, line),
              scope!(@lexical is_strict, 1, true),
              stmts!(
                pos!(col + 14, line),
                stmt!(
                  pos!(col + 14, line),
                  unary!(
                    "OpNot",
                    "Pre",
                    pos!(col + 14, line),
                    fnexpr!(
                      pos!(col + 15, line),
                      "Function",
                      0,
                      0,
                      scope!(@opaque is_strict, 1, true),
                      ident!("m", pos!(col + 24, line)),
                      exprs!(pos!(col + 25, line)),
                      label!(
                        pos!(col + 29, line),
                        ident!("X", pos!(col + 29, line)),
                        label!(
                          pos!(col + 32, line),
                          ident!("Y", pos!(col + 32, line)),
                          while_stmt!(
                            pos!(col + 35, line),
                            number!("1", pos!(col + 42, line)),
                            block!(
                              pos!(col + 45, line),
                              scope!(@lexical is_strict, 0, true),
                              stmts!(
                                pos!(col + 46, line),
                                break_stmt!(pos!(col + 46, line), "X"),
                                break_stmt!(pos!(col + 55, line), "Y")
                              )
                            )
                          )
                        ),
                      ),
                    )
                  )
                ),
                break_stmt!(pos!(col + 66, line), "X"),
              )
            )
          )
        );
      },
      "X: while (1) {!function m() {X: Y: while (1) {break X; break Y;}};break X;}",
      1,
    )
  }

  #[test]
  fn break_statement_undefined_label_early_error() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 20, 21, "X: while (1) {break Y;}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 48, 49, "X: while (1) {function m() {Y: while (1) {break X;}}}");
  }

  #[test]
  fn break_statement_outside_iteration_or_switch_early_error() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "break");
    basic_env_expression_eary_error_test(EnvFlag::all(), 26, 31, "while (1) {function m () {break;}}");
  }

  #[test]
  fn parse_continue_statement_in_while() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return while_stmt!(
          pos!(col, line),
          number!("1", pos!(col + 7, line)),
          block!(
            pos!(col + 10, line),
            scope!(@lexical is_strict, 0, true),
            continue_stmt!(pos!(col + 11, line))
          )
        );
      },
      "while (1) {continue;}",
      1,
    )
  }

  #[test]
  fn parse_continue_statement_in_do_while() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return do_while!(
          pos!(col, line),
          number!("1", pos!(col + 22, line)),
          block!(
            pos!(col + 3, line),
            scope!(@lexical is_strict, 0, true),
            continue_stmt!(pos!(col + 4, line))
          )
        );
      },
      "do {continue;} while (1)",
      1,
    )
  }

  #[test]
  fn parse_continue_statement_in_for() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return for_stmt!(
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 5, line),
            ident!("i", pos!(col + 9, line)),
            number!("0", pos!(col + 13, line))
          ),
          binary!(
            "OpLessThan",
            pos!(col + 16, line),
            ident!("i", pos!(col + 16, line)),
            number!("10", pos!(col + 20, line))
          ),
          unary!("OpIncrement", "Post", pos!(col + 24, line), ident!("i", pos!(col + 24, line))),
          block!(
            pos!(col + 29, line),
            scope!(@lexical is_strict, 0, true),
            continue_stmt!(pos!(col + 30, line))
          )
        );
      },
      "for (let i = 0; i < 10; i++) {continue;}",
      1,
    )
  }

  #[test]
  fn parse_nested_continue_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return while_stmt!(
          pos!(col, line),
          number!("1", pos!(col + 7, line)),
          block!(
            pos!(col + 10, line),
            scope!(@lexical is_strict, 1, true),
            stmt!(
              pos!(col + 11, line),
              unary!(
                "OpNot",
                "Pre",
                pos!(col + 11, line),
                fnexpr!(
                  pos!(col + 12, line),
                  "Function",
                  0,
                  0,
                  scope!(@opaque is_strict, 1, true),
                  ident!("m", pos!(col + 21, line)),
                  exprs!(pos!(col + 22, line)),
                  while_stmt!(
                    pos!(col + 26, line),
                    number!("1", pos!(col + 33, line)),
                    block!(
                      pos!(col + 36, line),
                      scope!(@lexical is_strict, 0, true),
                      continue_stmt!(pos!(col + 37, line))
                    )
                  )
                )
              )
            )
          )
        );
      },
      "while (1) {!function m() {while (1) {continue;}}}",
      1,
    )
  }

  #[test]
  fn parse_continue_statement_in_while_with_label() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return label!(
          pos!(col, line),
          ident!("X", pos!(col, line)),
          while_stmt!(
            pos!(col + 3, line),
            number!("1", pos!(col + 10, line)),
            block!(
              pos!(col + 13, line),
              scope!(@lexical is_strict, 0, true),
              continue_stmt!(pos!(col + 14, line), "X")
            )
          )
        );
      },
      "X: while (1) {continue X;}",
      1,
    )
  }

  #[test]
  fn parse_continue_statement_in_while_with_nested_label() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return label!(
          pos!(col, line),
          ident!("X", pos!(col, line)),
          while_stmt!(
            pos!(col + 3, line),
            number!("1", pos!(col + 10, line)),
            block!(
              pos!(col + 13, line),
              scope!(@lexical is_strict, 1, true),
              stmts!(
                pos!(col + 14, line),
                stmt!(
                  pos!(col + 14, line),
                  unary!(
                    "OpNot",
                    "Pre",
                    pos!(col + 14, line),
                    fnexpr!(
                      pos!(col + 15, line),
                      "Function",
                      0,
                      0,
                      scope!(@opaque is_strict, 1, true),
                      ident!("m", pos!(col + 24, line)),
                      exprs!(pos!(col + 25, line)),
                      label!(
                        pos!(col + 29, line),
                        ident!("X", pos!(col + 29, line)),
                        label!(
                          pos!(col + 32, line),
                          ident!("Y", pos!(col + 32, line)),
                          while_stmt!(
                            pos!(col + 35, line),
                            number!("1", pos!(col + 42, line)),
                            block!(
                              pos!(col + 45, line),
                              scope!(@lexical is_strict, 0, true),
                              stmts!(
                                pos!(col + 46, line),
                                continue_stmt!(pos!(col + 46, line), "X"),
                                continue_stmt!(pos!(col + 58, line), "Y")
                              )
                            )
                          )
                        ),
                      ),
                    )
                  )
                ),
                continue_stmt!(pos!(col + 72, line), "X"),
              )
            )
          )
        );
      },
      "X: while (1) {!function m() {X: Y: while (1) {continue X; continue Y;}};continue X;}",
      1,
    )
  }

  #[test]
  fn continue_statement_undefined_label_early_error() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 23, 24, "X: while (1) {continue Y;}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 51, 52, "X: while (1) {function m() {Y: while (1) {continue X;}}}");
  }

  #[test]
  fn continue_statement_outside_iteration_or_switch_early_error() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 8, "continue");
    basic_env_expression_eary_error_test(EnvFlag::all(), 26, 34, "while (1) {function m () {continue;}}");
  }

  #[test]
  fn parse_return_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return stmt!(
          pos!(col, line),
          unary!(
            "OpNot",
            "Pre",
            pos!(col, line),
            fnexpr!(
              pos!(col + 1, line),
              "Function",
              0,
              0,
              scope!(@opaque is_strict, 0, true),
              exprs!(pos!(col + 9, line)),
              return_stmt!(pos!(col + 13, line), number!("1", pos!(col + 20, line)))
            )
          )
        );
      },
      "!function() {return 1}",
      1,
    )
  }

  #[test]
  fn parse_return_statement_without_expr() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return stmt!(
          pos!(col, line),
          unary!(
            "OpNot",
            "Pre",
            pos!(col, line),
            fnexpr!(
              pos!(col + 1, line),
              "Function",
              0,
              0,
              scope!(@opaque is_strict, 0, true),
              exprs!(pos!(col + 9, line)),
              return_stmt!(pos!(col + 13, line))
            )
          )
        );
      },
      "!function() {return}",
      1,
    )
  }

  #[test]
  fn parse_arrow_return_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, is_skip)| {
        return var!(
          "Let",
          pos!(col + 0, line),
          ident!("a", pos!(col + 4, line)),
          fnexpr!(
            pos!(col + 8, line),
            "ArrowFunction",
            if is_skip { col + 21 } else { 0 },
            if is_skip { col + 22 } else { 0 },
            scope!(@transparent is_strict, 0, true),
            exprs!(pos!(col + 8, line)),
            if !is_skip { return_stmt!(pos!(col + 15, line)) } else { void!() }
          )
        );
      },
      "let a = () => {return}",
      1,
    )
  }

  #[test]
  fn parse_method_return_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, is_skip)| {
        return stmt!(
          pos!(col, line),
          objectlit!(
            ObjectLitType::NONE,
            pos!(col + 1, line),
            object_props!(
              pos!(col + 2, line),
              ident!("a", pos!(col + 2, line)),
              fnexpr!(
                pos!(col + 2, line),
                "Function",
                if is_skip { col + 7 } else { 0 },
                if is_skip { col + 13 } else { 0 },
                scope!(@opaque is_strict, 0, true),
                ident!("a", pos!(col + 2, line)),
                exprs!(pos!(col + 4, line)),
                if !is_skip { return_stmt!(pos!(col + 7, line)) } else { void!() }
              )
            )
          )
        );
      },
      "({a() {return}})",
      1,
    )
  }

  #[test]
  fn return_statement_outside_function_early_error() {
    const ENV: [(Option<&str>, &str); 3] = [(Some(""), ""), (None, ""), (None, "")];
    basic_env_expression_eary_error_test(EnvFlag::BASIC, 0, 6, "return");
  }

  #[test]
  fn parse_throw_statement() {
    stmt_test(
      |(col, line, is_strict, is_skip)| {
        return throw_stmt!(pos!(col, line), number!("1", pos!(col + 6, line)));
      },
      "throw 1",
    )
  }

  #[test]
  fn throw_without_expr_parse_error() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "throw");
    basic_env_expression_eary_error_test(EnvFlag::all(), 0, 5, "throw;");
    basic_env_expression_eary_error_test(
      EnvFlag::all(),
      0,
      5,
      "throw
let a = 0",
    );
  }

  #[test]
  fn parse_try_catch_statement() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return try_catch_stmt!(
          pos!(col, line),
          block!(
            pos!(col + 4, line),
            scope!(@lexical is_strict, 0, true),
            throw_stmt!(pos!(col + 5, line), ident!("a", pos!(col + 11, line)))
          ),
          catch_block!(
            pos!(col + 14, line),
            ident!("e", pos!(col + 20, line)),
            block!(pos!(col + 23, line), scope!(@lexical is_strict, 0, true), empty!())
          ),
          block!(pos!(col + 34, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "try {throw a} catch(e) {} finally {}",
      3,
    )
  }

  #[test]
  fn parse_try_catch_statement_without_finally() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return try_catch_stmt!(
          pos!(col, line),
          block!(
            pos!(col + 4, line),
            scope!(@lexical is_strict, 0, true),
            throw_stmt!(pos!(col + 5, line), ident!("a", pos!(col + 11, line)))
          ),
          catch_block!(
            pos!(col + 14, line),
            ident!("e", pos!(col + 20, line)),
            block!(pos!(col + 23, line), scope!(@lexical is_strict, 0, true), empty!())
          )
        );
      },
      "try {throw a} catch(e) {}",
      2,
    )
  }

  #[test]
  fn parse_try_catch_statement_without_catch() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return try_catch_stmt!(
          pos!(col, line),
          block!(
            pos!(col + 4, line),
            scope!(@lexical is_strict, 0, true),
            throw_stmt!(pos!(col + 5, line), ident!("a", pos!(col + 11, line)))
          ),
          block!(pos!(col + 22, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "try {throw a} finally {}",
      2,
    )
  }

  #[test]
  fn parse_try_catch_statement_without_catch_param() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return try_catch_stmt!(
          pos!(col, line),
          block!(
            pos!(col + 4, line),
            scope!(@lexical is_strict, 0, true),
            throw_stmt!(pos!(col + 5, line), ident!("a", pos!(col + 11, line)))
          ),
          catch_block!(
            pos!(col + 14, line),
            block!(pos!(col + 20, line), scope!(@lexical is_strict, 0, true), empty!())
          )
        );
      },
      "try {throw a} catch {}",
      2,
    )
  }

  #[test]
  fn parse_try_catch_statement_with_duplicated_var_decl() {
    stmt_test_with_scope(
      |(col, line, is_strict, _)| {
        return try_catch_stmt!(
          pos!(col, line),
          block!(
            pos!(col + 4, line),
            scope!(@lexical is_strict, 0, true),
            throw_stmt!(pos!(col + 5, line), ident!("a", pos!(col + 11, line)))
          ),
          catch_block!(
            pos!(col + 14, line),
            ident!("e", pos!(col + 20, line)),
            block!(
              pos!(col + 23, line),
              scope!(@lexical is_strict, 0, true),
              var!(
                "Var",
                pos!(col + 24, line),
                ident!("e", pos!(col + 28, line)),
                number!("0", pos!(col + 32, line))
              )
            )
          )
        );
      },
      "try {throw a} catch(e) {var e = 0;}",
      2,
    )
  }

  #[test]
  fn catch_param_has_duplicated_param_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 17, 18, "try {} catch([a, a]) {}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 25, 26, "try {} catch({a: {b: c}, c}) {}");
  }

  #[test]
  fn catch_param_has_duplicated_lexical_decl_in_block_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::all(), 21, 22, "try {} catch(a) {let a = 0;}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 23, 24, "try {} catch([a]) {let a = 0;}");
    basic_env_expression_eary_error_test(EnvFlag::all(), 23, 24, "try {} catch([a]) {var a = 0;}");
  }

  #[test]
  fn parse_debugger_statement() {
    stmt_test(
      |(col, line, _, _)| {
        return debugger_stmt!(pos!(col, line));
      },
      "debugger",
    );
  }

  #[test]
  fn parse_with_statement() {
    parser_ast_test_with_options(
      |(col, line, is_strict, _)| {
        return with_stmt!(
          pos!(col, line),
          ident!("a", pos!(col + 5, line)),
          block!(pos!(col + 8, line), scope!(@lexical is_strict, 0, true), empty!())
        );
      },
      "with(a) {}",
      ParserOption::default(),
      1,
      EnvFlag::BASIC | EnvFlag::FUNCTION_WRAPPER,
      0,
      ParserType::Script,
      true,
    );
  }

  #[test]
  fn with_statement_in_strict_mode_early_error_test() {
    basic_env_expression_eary_error_test(EnvFlag::STRICT_MODE, 0, 4, "with (a) {}");
  }

  #[test]
  fn parse_script_direct_eval_super() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return callexpr!("Super", pos!(col, line), exprs!(pos!(col + 5, line)));
      },
      "super()",
      ParserOption::default().with_allow_super(),
      0,
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
      0,
    );
  }

  #[test]
  fn parse_script_direct_new_target() {
    single_expression_test_with_options(
      |(col, line, _, _)| {
        return prop!("Dot", "NewTarget", pos!(col, line));
      },
      "new.target",
      ParserOption::default().with_allow_new_target(),
      0,
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
      0,
    );
  }

  #[test]
  fn parse_import_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return import_stmt!(
          pos!(col, line),
          import_binding!(pos!(col + 7, line), ident!("foo", pos!(col + 7, line))),
          str!("bar", pos!(col + 16, line))
        );
      },
      "import foo from 'bar'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_namespace_import_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return import_stmt!(
          pos!(col, line),
          import_binding!(
            pos!(col + 7, line),
            import_specifier!(pos!(col + 7, line), true, ident!("foo", pos!(col + 12, line)))
          ),
          str!("bar", pos!(col + 21, line))
        );
      },
      "import * as foo from 'bar'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_named_import_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return import_stmt!(
          pos!(col, line),
          import_binding!(
            pos!(col + 7, line),
            named_import_list!(
              pos!(col + 7, line),
              import_specifier!(pos!(col + 8, line), false, ident!("a", pos!(col + 8, line))),
              import_specifier!(pos!(col + 11, line), false, ident!("b", pos!(col + 11, line))),
              import_specifier!(pos!(col + 14, line), false, ident!("c", pos!(col + 14, line)))
            )
          ),
          str!("bar", pos!(col + 22, line))
        );
      },
      "import {a, b, c} from 'bar'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_import_stmt_and_namespace_import() {
    module_stmt_test(
      |(col, line, _, _)| {
        return import_stmt!(
          pos!(col, line),
          import_binding!(
            pos!(col + 7, line),
            ident!("foo", pos!(col + 7, line)),
            import_specifier!(pos!(col + 12, line), true, ident!("bar", pos!(col + 17, line)))
          ),
          str!("qux", pos!(col + 26, line))
        );
      },
      "import foo, * as bar from 'qux'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_import_stmt_and_named_import() {
    module_stmt_test(
      |(col, line, _, _)| {
        return import_stmt!(
          pos!(col, line),
          import_binding!(
            pos!(col + 7, line),
            ident!("foo", pos!(col + 7, line)),
            named_import_list!(
              pos!(col + 12, line),
              import_specifier!(pos!(col + 13, line), false, ident!("a", pos!(col + 13, line))),
              import_specifier!(pos!(col + 16, line), false, ident!("b", pos!(col + 16, line))),
              import_specifier!(pos!(col + 19, line), false, ident!("c", pos!(col + 19, line)))
            )
          ),
          str!("qux", pos!(col + 27, line))
        );
      },
      "import foo, {a, b, c} from 'qux'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn import_stmt_has_duplicate_name() {
    basic_env_expression_eary_error_test_with_parser_type(
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
      14,
      15,
      "import {a, b, a} from 'bar'",
      ParserType::Module,
    );

    basic_env_expression_eary_error_test_with_parser_type(
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
      15,
      16,
      "import a, * as a from 'bar'",
      ParserType::Module,
    );

    basic_env_expression_eary_error_test_with_parser_type(
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
      18,
      21,
      "import foo, {bar, foo} from 'bar'",
      ParserType::Module,
    );
  }

  #[test]
  fn parse_asterisk_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!("namespace", pos!(col, line), import_specifier!(pos!(col + 7, line), true));
      },
      "export *",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_asterisk_from_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          import_specifier!(pos!(col + 7, line), true),
          str!("bar", pos!(col + 14, line))
        );
      },
      "export * from 'bar'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_asterisk_as_from_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          import_specifier!(pos!(col + 7, line), true, ident!("foo", pos!(col + 12, line))),
          str!("bar", pos!(col + 21, line))
        );
      },
      "export * as foo from 'bar'",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_named_export_list_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          named_import_list!(
            pos!(col + 7, line),
            import_specifier!(pos!(col + 8, line), false, ident!("a", pos!(col + 8, line))),
            import_specifier!(pos!(col + 11, line), false, ident!("b", pos!(col + 11, line))),
            import_specifier!(pos!(col + 14, line), false, ident!("c", pos!(col + 14, line))),
          )
        );
      },
      "export {a, b, c}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_alias_named_export_list_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          named_import_list!(
            pos!(col + 7, line),
            import_specifier!(
              pos!(col + 8, line),
              false,
              ident!("foo", pos!(col + 8, line)),
              ident!("a", pos!(col + 15, line))
            ),
            import_specifier!(
              pos!(col + 18, line),
              false,
              ident!("bar", pos!(col + 18, line)),
              ident!("b", pos!(col + 25, line))
            ),
            import_specifier!(
              pos!(col + 28, line),
              false,
              ident!("qux", pos!(col + 28, line)),
              ident!("c", pos!(col + 35, line))
            ),
          )
        );
      },
      "export {foo as a, bar as b, qux as c}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_var_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          var!(
            "Var",
            pos!(col + 7, line),
            ident!("a", pos!(col + 11, line)),
            number!("1", pos!(col + 15, line))
          )
        );
      },
      "export var a = 1",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_let_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          var!(
            "Let",
            pos!(col + 7, line),
            ident!("a", pos!(col + 11, line)),
            number!("1", pos!(col + 15, line))
          )
        );
      },
      "export let a = 1",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_const_export_stmt() {
    module_stmt_test(
      |(col, line, _, _)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          var!(
            "Const",
            pos!(col + 7, line),
            ident!("a", pos!(col + 13, line)),
            number!("1", pos!(col + 17, line))
          )
        );
      },
      "export const a = 1",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_function_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          fnexpr!(
            pos!(col + 7, line),
            "Function",
            if !is_skip { 0 } else { col + 21 },
            if !is_skip { 0 } else { col + 21 },
            scope!(@opaque is_strict, 0, true),
            ident!("a", pos!(col + 16, line)),
            exprs!(pos!(col + 17, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export function a() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_async_function_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          afnexpr!(
            pos!(col + 7, line),
            "Function",
            if !is_skip { 0 } else { col + 27 },
            if !is_skip { 0 } else { col + 27 },
            scope!(@opaque is_strict, 0, true),
            ident!("a", pos!(col + 22, line)),
            exprs!(pos!(col + 23, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export async function a() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_async_generator_function_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          afnexpr!(
            pos!(col + 7, line),
            "Generator",
            if !is_skip { 0 } else { col + 28 },
            if !is_skip { 0 } else { col + 28 },
            scope!(@opaque is_strict, 0, true),
            ident!("a", pos!(col + 23, line)),
            exprs!(pos!(col + 24, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export async function *a() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_generator_function_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "namespace",
          pos!(col, line),
          fnexpr!(
            pos!(col + 7, line),
            "Generator",
            if !is_skip { 0 } else { col + 22 },
            if !is_skip { 0 } else { col + 22 },
            scope!(@opaque is_strict, 0, true),
            ident!("a", pos!(col + 17, line)),
            exprs!(pos!(col + 18, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export function *a() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_function_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "default",
          pos!(col, line),
          fnexpr!(
            pos!(col + 15, line),
            "Function",
            if !is_skip { 0 } else { col + 27 },
            if !is_skip { 0 } else { col + 27 },
            scope!(@opaque is_strict, 0, true),
            exprs!(pos!(col + 23, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export default function() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_async_function_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "default",
          pos!(col, line),
          afnexpr!(
            pos!(col + 15, line),
            "Function",
            if !is_skip { 0 } else { col + 33 },
            if !is_skip { 0 } else { col + 33 },
            scope!(@opaque is_strict, 0, true),
            exprs!(pos!(col + 29, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export default async function() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_async_generator_function_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "default",
          pos!(col, line),
          afnexpr!(
            pos!(col + 15, line),
            "Generator",
            if !is_skip { 0 } else { col + 34 },
            if !is_skip { 0 } else { col + 34 },
            scope!(@opaque is_strict, 0, true),
            exprs!(pos!(col + 30, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export default async function*() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_generator_function_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "default",
          pos!(col, line),
          fnexpr!(
            pos!(col + 15, line),
            "Generator",
            if !is_skip { 0 } else { col + 28 },
            if !is_skip { 0 } else { col + 28 },
            scope!(@opaque is_strict, 0, true),
            exprs!(pos!(col + 24, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export default function*() {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_class_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!("default", pos!(col, line), class(pos!(col + 15, line), None, None, &[], &[]));
      },
      "export default class {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_arrow_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!(
          "default",
          pos!(col, line),
          fnexpr!(
            pos!(col + 15, line),
            "ArrowFunction",
            if !is_skip { 0 } else { col + 23 },
            if !is_skip { 0 } else { col + 23 },
            scope!(@transparent is_strict, 0, true),
            exprs!(pos!(col + 15, line)),
            if !is_skip { empty!() } else { void!() }
          )
        );
      },
      "export default () => {}",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }

  #[test]
  fn parse_literal_default_export_stmt() {
    module_stmt_test(
      |(col, line, is_strict, is_skip)| {
        return export_stmt!("default", pos!(col, line), number!("1", pos!(col + 15, line)));
      },
      "export default 1",
      EnvFlag::BASIC | EnvFlag::STRICT_MODE,
    );
  }
}
