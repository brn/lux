use super::ast::*;
use super::node_ops::*;
use super::scope::Scope;
use super::source_position::RuntimeSourcePosition;
use super::token::Token;
use crate::utility::*;

pub struct SkipTreeBuilder {
  region: Region,
  skip_array: Expr,
  skip_object: Expr,
  skip_call: Expr,
  skip_new_call: Expr,
  skip_super_call: Expr,
  skip_identifier: Expr,
  skip_literal: Expr,
  skip_string: Expr,
  skip_binary_expr: Expr,
  skip_exprs: Expr,
  skip_expr: Expr,
  skip_template: Expr,
  skip_import_meta: Expr,
  skip_stmt: Stmt,
  skip_class_field: Stmt,
  skip_class: Ast,
  skip_function: Ast,
  skip_vars: Stmt,
  skip_var: Stmt,
}

impl SkipTreeBuilder {
  pub fn new(mut region: Region) -> Self {
    return SkipTreeBuilder {
      region: region.clone(),
      skip_call: SkipExpr::new(&mut region, SkipExprType::CALL).into(),
      skip_new_call: SkipExpr::new(&mut region, SkipExprType::NEW_RECEIVER_CALL).into(),
      skip_super_call: SkipExpr::new(&mut region, SkipExprType::SUPER_RECEIVER_CALL).into(),
      skip_array: SkipExpr::new(&mut region, SkipExprType::ARRAY).into(),
      skip_object: SkipExpr::new(&mut region, SkipExprType::OBJECT).into(),
      skip_identifier: SkipExpr::new(&mut region, SkipExprType::IDENTIFIER).into(),
      skip_literal: SkipExpr::new(&mut region, SkipExprType::LITERAL).into(),
      skip_string: SkipExpr::new(&mut region, SkipExprType::STRING_LITERAL).into(),
      skip_binary_expr: SkipExpr::new(&mut region, SkipExprType::BINARY_EXPR).into(),
      skip_expr: SkipExpr::new(&mut region, SkipExprType::EXPR).into(),
      skip_template: SkipExpr::new(&mut region, SkipExprType::TEMPLATE).into(),
      skip_import_meta: SkipExpr::new(&mut region, SkipExprType::IMPORT_META).into(),
      skip_stmt: SkipStmt::new(&mut region, SkipStmtType::STMT).into(),
      skip_exprs: SkipExpr::new(&mut region, SkipExprType::EXPRS).into(),
      skip_class_field: SkipStmt::new(&mut region, SkipStmtType::CLASS_FIELD).into(),
      skip_class: SkipAny::new(&mut region, SkipAnyType::CLASS).into(),
      skip_function: SkipAny::new(&mut region, SkipAnyType::FUNCTION).into(),
      skip_vars: SkipStmt::new(&mut region, SkipStmtType::VARS).into(),
      skip_var: SkipStmt::new(&mut region, SkipStmtType::VAR).into(),
    };
  }
}

impl NodeOps for SkipTreeBuilder {
  fn region(&mut self) -> &mut Region {
    return &mut self.region;
  }

  fn new_empty(&mut self, _: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_expr;
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
    return self.skip_function;
  }

  fn new_binary_expr(&mut self, op: Token, lhs: Expr, rhs: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_binary_expr;
  }

  fn new_expressions(&mut self, items: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_exprs;
  }

  fn new_call_expression(
    &mut self,
    receiver: CallReceiverType,
    callee: Option<Expr>,
    parameters: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return if receiver.contains(CallReceiverType::New) {
      self.skip_new_call
    } else if receiver.contains(CallReceiverType::Super) {
      self.skip_super_call
    } else {
      self.skip_call
    };
  }

  fn new_conditional_expression(&mut self, condition: Expr, then_expr: Expr, else_expr: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_expr;
  }

  fn new_literal(&mut self, literal_type: Token, literal_value: LiteralValue, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return match literal_type {
      Token::Identifier => self.skip_identifier,
      Token::StringLiteral => self.skip_string,
      _ => self.skip_literal,
    };
  }

  fn new_object_property_expression(
    &mut self,
    key: Expr,
    value: Option<Expr>,
    init: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return self.skip_expr;
  }

  fn new_property_access_expression(
    &mut self,
    property_access_type: PropertyAccessType,
    receiver_type: CallReceiverType,
    receiver: Option<Expr>,
    property: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return self.skip_expr;
  }

  fn new_structural_literal(
    &mut self,
    literal_flag: StructuralLiteralFlags,
    properties: Vec<Expr>,
    has_spread: bool,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    if literal_flag.contains(StructuralLiteralFlags::ARRAY) {
      return self.skip_array;
    }
    return self.skip_object;
  }

  fn new_unary_expression(
    &mut self,
    position: UnaryExpressionOperandPosition,
    op: Token,
    target: Expr,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return self.skip_expr;
  }

  fn new_import_specifier(
    &mut self,
    is_namespace: bool,
    name: Option<Expr>,
    as_expr: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return self.skip_expr;
  }

  fn new_import_binding(
    &mut self,
    default_binding: Option<Expr>,
    namespace_import: Option<Expr>,
    named_import_list: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Expr {
    return self.skip_expr;
  }

  fn new_named_import_list(&mut self, list: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_expr;
  }

  fn new_new_expression(&mut self, callee: Expr, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_expr;
  }

  fn new_template_literal(&mut self, properties: Vec<Expr>, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_template;
  }

  fn is_binary_expr(&self, expr: Expr) -> bool {
    return match expr {
      Expr::BinaryExpression(_) => true,
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
      Expr::SkipExpr(expr) => expr.is_literal(),
      _ => false,
    };
  }

  fn is_string_literal(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(e) => e.is_string_literal(),
      _ => false,
    };
  }

  fn is_identifier(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(e) => e.is_identifier(),
      _ => false,
    };
  }

  fn is_call_expr(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(expr) => expr.is_call(),
      _ => false,
    };
  }

  fn is_super_call(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(expr) => expr.is_super_receiver_call(),
      _ => false,
    };
  }

  fn is_new_call(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(expr) => expr.is_new_receiver_call(),
      _ => false,
    };
  }

  fn is_spread_expression(&self, expr: Expr) -> bool {
    return match expr {
      Expr::SkipExpr(expr) => expr.is_spread(),
      _ => false,
    };
  }

  fn push_expr(&self, list: &mut Vec<Expr>, item: Expr) {}

  fn push_stmt(&self, list: &mut Vec<Stmt>, item: Stmt) {}

  fn exprs_push(&self, node: Expr, item: Expr) -> bool {
    return match node {
      Expr::SkipExpr(expr) => expr.is_exprs() || expr.is_template(),
      _ => false,
    };
  }

  fn new_statements(&mut self, items: Vec<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return self.skip_stmt;
  }

  fn new_statement(&mut self, expr: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return self.skip_stmt;
  }

  fn new_import_decl(&mut self, import_binding: Option<Expr>, module_specifier: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return self.skip_stmt;
  }

  fn new_import_meta(&mut self, pos: Option<&RuntimeSourcePosition>) -> Expr {
    return self.skip_import_meta;
  }

  fn new_export_decl(
    &mut self,
    export_type: ExportDeclarationType,
    export_clause: Option<Ast>,
    from_clause: Option<Ast>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return self.skip_stmt;
  }

  fn new_class_field(&mut self, flags: ClassFieldFlag, value: Expr, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return self.skip_class_field;
  }

  fn new_class(
    &mut self,
    name: Option<Expr>,
    heritage: Option<Expr>,
    fields: Vec<Stmt>,
    methods: Vec<Stmt>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Ast {
    return self.skip_class;
  }

  fn new_var(
    &mut self,
    decl_type: VariableDeclarationType,
    binding: Expr,
    initialzier: Option<Expr>,
    pos: Option<&RuntimeSourcePosition>,
  ) -> Stmt {
    return self.skip_var;
  }

  fn new_vars(&mut self, vars: Vec<Stmt>, pos: Option<&RuntimeSourcePosition>) -> Stmt {
    return self.skip_vars;
  }
}
