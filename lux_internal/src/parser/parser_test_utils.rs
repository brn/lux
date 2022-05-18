use super::source_position::*;

#[derive(Clone)]
pub struct TestableAst {
  name: String,
  attr: String,
  pos: RuntimeSourcePosition,
  children: Vec<Box<TestableAst>>,
}

impl TestableAst {
  pub fn ast(name: &str, attr: &str, pos: RuntimeSourcePosition) -> Self {
    return TestableAst {
      name: name.to_string(),
      attr: attr.to_string(),
      pos,
      children: Vec::new(),
    };
  }

  pub fn push(&mut self, ast: TestableAst) {
    self.children.push(Box::new(ast));
  }

  pub fn to_string(&self) -> String {
    let mut indent = "".to_string();
    return self.to_string_internal(&mut indent);
  }

  pub fn to_string_internal(&self, indent: &mut String) -> String {
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

pub fn class(
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
  pub struct ObjectLitType: u8 {
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
