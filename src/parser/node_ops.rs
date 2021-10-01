use super::ast::*;
use super::scope::*;
use super::source_position::*;
use super::token::*;
use crate::structs::FixedU16CodePointArray;
use crate::utility::*;

macro_rules! new_node {
  ($name:tt, $region:expr, $pos:expr, $($args:expr),+$(,)?) => {{
    let mut node = $name::new($region, $($args),*);
    if let Some(p) = $pos {
      node.set_source_position(p);
    }
    node
  }};
  ($name:tt, $region:expr, $pos:expr) => {{
    let mut node = $name::new($region);
    if let Some(p) = $pos {
      node.set_source_position(p);
    }
    node
  }};
}

pub trait NodeOps {
  fn region(&mut self) -> &mut Region;

  fn new_empty(&mut self, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(Empty, self.region(), pos).into();
  }

  fn new_function(
    &mut self,
    name: Option<Expr>,
    attr: FunctionAttribute,
    scope: Exotic<Scope>,
    formal_parameters: Expr,
    function_body: Option<Ast>,
    function_body_start: u32,
    function_body_end: u32,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Ast {
    return new_node!(
      Function,
      self.region(),
      pos,
      name,
      attr,
      scope,
      formal_parameters,
      function_body,
      function_body_start,
      function_body_end
    )
    .into();
  }

  fn new_binary_expr(&mut self, op: Token, lhs: Expr, rhs: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(BinaryExpression, self.region(), pos, op, lhs, rhs).into();
  }

  fn new_expressions(&mut self, items: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(Expressions, self.region(), pos, items).into();
  }

  fn new_call_expression(
    &mut self,
    receiver: CallReceiverType,
    callee: Option<Expr>,
    parameters: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(CallExpression, self.region(), pos, receiver, callee, parameters).into();
  }

  fn new_conditional_expression(&mut self, condition: Expr, then_expr: Expr, else_expr: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(ConditionalExpression, self.region(), pos, condition, then_expr, else_expr).into();
  }

  fn new_literal(&mut self, literal_type: Token, literal_value: LiteralValue, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(Literal, self.region(), pos, literal_type, literal_value).into();
  }

  fn new_object_property_expression(
    &mut self,
    key: Expr,
    value: Option<Expr>,
    init: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(ObjectPropertyExpression, self.region(), pos, key, value, init).into();
  }

  fn new_property_access_expression(
    &mut self,
    property_access_type: PropertyAccessType,
    receiver_type: CallReceiverType,
    receiver: Option<Expr>,
    property: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(
      PropertyAccessExpression,
      self.region(),
      pos,
      property_access_type,
      receiver_type,
      receiver,
      property
    )
    .into();
  }

  fn new_structural_literal(
    &mut self,
    literal_flag: StructuralLiteralFlags,
    properties: Vec<Expr>,
    has_spread: bool,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(StructuralLiteral, self.region(), pos, literal_flag, properties).into();
  }

  fn new_unary_expression(
    &mut self,
    position: UnaryExpressionOperandPosition,
    op: Token,
    target: Expr,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(UnaryExpression, self.region(), pos, position, op, target).into();
  }

  fn new_import_specifier(
    &mut self,
    is_namespace: bool,
    name: Option<Expr>,
    as_expr: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(ImportSpecifier, self.region(), pos, is_namespace, name, as_expr).into();
  }

  fn new_import_binding(
    &mut self,
    default_binding: Option<Expr>,
    namespace_import: Option<Expr>,
    named_import_list: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return new_node!(
      ImportBinding,
      self.region(),
      pos,
      default_binding,
      namespace_import,
      named_import_list
    )
    .into();
  }

  fn new_named_import_list(&mut self, list: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(NamedImportList, self.region(), pos, list).into();
  }

  fn new_new_expression(&mut self, callee: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(NewExpression, self.region(), pos, callee).into();
  }

  fn new_template_literal(&mut self, properties: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(TemplateLiteral, self.region(), pos, properties).into();
  }

  fn new_import_meta(&mut self, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return new_node!(ImportMetaExpression, self.region(), pos).into();
  }

  fn is_binary_expr(&self, expr: Expr) -> bool {
    return match expr {
      Expr::BinaryExpression(_) => true,
      _ => false,
    };
  }

  fn is_initializer(&self, expr: Expr) -> bool {
    return match expr {
      Expr::BinaryExpression(node) => {
        node.op() == Token::OpAssign
          && match node.lhs() {
            Expr::Literal(n) => n.is_identifier(),
            _ => false,
          }
      }
      _ => false,
    };
  }

  fn is_call_expr(&self, expr: Expr) -> bool {
    return match expr {
      Expr::CallExpression(node) => node.receiver().contains(CallReceiverType::Expr),
      _ => false,
    };
  }

  fn is_super_call(&self, expr: Expr) -> bool {
    return match expr {
      Expr::CallExpression(node) => node.receiver().contains(CallReceiverType::Super),
      _ => false,
    };
  }

  fn is_new_call(&self, expr: Expr) -> bool {
    return match expr {
      Expr::CallExpression(node) => node.receiver().contains(CallReceiverType::New),
      _ => false,
    };
  }

  fn is_spread_expression(&self, expr: Expr) -> bool {
    return match expr {
      Expr::UnaryExpression(node) => node.op() == Token::Spread,
      _ => false,
    };
  }

  fn is_structural_literal(&self, expr: Expr) -> bool {
    return match expr {
      Expr::StructuralLiteral(_) => true,
      _ => false,
    };
  }

  fn is_literal(&self, expr: Expr) -> bool {
    return match expr {
      Expr::Literal(_) => true,
      _ => false,
    };
  }

  fn is_string_literal(&self, expr: Expr) -> bool {
    return match expr {
      Expr::Literal(lit) => lit.literal_type() == Token::StringLiteral,
      _ => false,
    };
  }

  fn is_identifier(&self, expr: Expr) -> bool {
    return match expr {
      Expr::Literal(lit) => lit.is_identifier(),
      _ => false,
    };
  }

  fn push_expr(&self, list: &mut Vec<Expr>, item: Expr) {
    list.push(item);
  }

  fn push_stmt(&self, list: &mut Vec<Stmt>, item: Stmt) {
    list.push(item);
  }

  fn exprs_push(&self, node: Expr, item: Expr) -> bool {
    if let Ok(mut exprs) = Node::<Expressions>::try_from(node) {
      exprs.push(item);
      return true;
    }
    return false;
  }

  fn new_statements(&mut self, items: Vec<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(Statements, self.region(), pos, items).into();
  }

  fn new_statement(&mut self, expr: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(Statement, self.region(), pos, expr).into();
  }

  fn new_import_decl(&mut self, import_binding: Option<Expr>, module_specifier: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ImportDeclaration, self.region(), pos, import_binding, module_specifier).into();
  }

  fn new_export_decl(
    &mut self,
    export_type: ExportDeclarationType,
    export_clause: Option<Ast>,
    from_clause: Option<Ast>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return new_node!(ExportDeclaration, self.region(), pos, export_type, export_clause, from_clause).into();
  }

  fn new_class_field(&mut self, flags: ClassFieldFlag, value: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ClassField, self.region(), pos, flags, value).into();
  }

  fn new_class(
    &mut self,
    name: Option<Expr>,
    heritage: Option<Expr>,
    methods: Vec<Stmt>,
    fields: Vec<Stmt>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Ast {
    return new_node!(Class, self.region(), pos, name, heritage, methods, fields).into();
  }

  fn new_block(&mut self, stmt: Stmt, scope: Exotic<Scope>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(BlockStatement, self.region(), pos, stmt, scope).into();
  }

  fn new_var(
    &mut self,
    decl_type: VariableDeclarationType,
    binding: Expr,
    initialzier: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return new_node!(VariableDeclaration, self.region(), pos, decl_type, binding, initialzier).into();
  }

  fn new_vars(&mut self, vars: Vec<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(VariableDeclarations, self.region(), pos, vars).into();
  }

  fn new_labelled_stmt(&mut self, identifier: Expr, stmt: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(LabelledStatement, self.region(), pos, identifier, stmt).into();
  }

  fn new_if_stmt(&mut self, condition: Expr, then_stmt: Stmt, else_stmt: Option<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(IfStatement, self.region(), pos, condition, then_stmt, else_stmt).into();
  }

  fn is_labelled_function(&self, labelled_stmt: Stmt) -> bool {
    return match labelled_stmt {
      Stmt::LabelledStatement(node) => match node.stmt() {
        Stmt::Function(_) => true,
        _ => false,
      },
      _ => false,
    };
  }

  fn new_switch_stmt(&mut self, scope: Exotic<Scope>, condition: Expr, cases: Vec<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(SwitchStatement, self.region(), pos, scope, condition, cases).into();
  }

  fn new_switch_case(&mut self, condition: Option<Expr>, stmt: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(SwitchCase, self.region(), pos, condition, stmt).into();
  }

  fn new_while_stmt(&mut self, condition: Expr, body: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(WhileStatement, self.region(), pos, condition, body).into();
  }

  fn new_do_while_stmt(&mut self, condition: Expr, body: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(DoWhileStatement, self.region(), pos, condition, body).into();
  }

  fn new_for_stmt(
    &mut self,
    declarations: Ast,
    condition: Expr,
    computation: Expr,
    body: Stmt,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return new_node!(ForStatement, self.region(), pos, declarations, condition, computation, body).into();
  }

  fn new_for_in_stmt(&mut self, lhs: Ast, rhs: Expr, body: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ForInStatement, self.region(), pos, lhs, rhs, body).into();
  }

  fn new_for_of_stmt(&mut self, is_await: bool, lhs: Ast, rhs: Expr, body: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ForOfStatement, self.region(), pos, is_await, lhs, rhs, body).into();
  }

  fn is_valid_for_of_in_lhs(&self, var: Ast) -> bool {
    if let Ok(stmt) = Stmt::try_from(var) {
      return match stmt {
        Stmt::VariableDeclaration(node) => node.initializer().is_none(),
        Stmt::VariableDeclarations(_) => false,
        _ => true,
      };
    }
    return true;
  }

  fn new_break_stmt(&mut self, identifier: Option<FixedU16CodePointArray>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(BreakStatement, self.region(), pos, identifier).into();
  }

  fn new_continue_stmt(&mut self, identifier: Option<FixedU16CodePointArray>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ContinueStatement, self.region(), pos, identifier).into();
  }

  fn new_return_stmt(&mut self, expr: Option<Expr>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ReturnStatement, self.region(), pos, expr).into();
  }

  fn new_throw_stmt(&mut self, expr: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(ThrowStatement, self.region(), pos, expr).into();
  }

  fn new_catch_block(&mut self, param: Option<Expr>, block: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(CatchBlock, self.region(), pos, param, block).into();
  }

  fn new_try_catch_stmt(
    &mut self,
    try_block: Stmt,
    catch_block: Option<Stmt>,
    finally_block: Option<Stmt>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return new_node!(TryCatchStatement, self.region(), pos, try_block, catch_block, finally_block).into();
  }

  fn new_with_stmt(&mut self, expr: Expr, body: Stmt, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(WithStatement, self.region(), pos, expr, body).into();
  }

  fn new_debugger_stmt(&mut self, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return new_node!(DebuggerStatement, self.region(), pos).into();
  }
}
