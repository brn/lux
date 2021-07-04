use super::source_position::SourcePosition;
use super::token::Token;
use crate::property::Property;
use crate::structs::FixedU16CodePointArray;
use crate::utility::*;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};

const EXPRESSION_BIT: usize = 1;
const STATEMENT_BIT: usize = EXPRESSION_BIT + 1;
const IS_VALID_LHS: usize = STATEMENT_BIT + 1;

pub trait AstNode {
  fn is_expr(&self) -> bool;
  fn is_stmt(&self) -> bool {
    return !self.is_expr();
  }
}

pub struct AstMetadata {
  flags: Bitset<u8>,
  source_position: SourcePosition,
}
impl AstMetadata {
  fn new() -> Self {
    return AstMetadata {
      flags: Bitset::<u8>::new(),
      source_position: SourcePosition::new(),
    };
  }
}

pub trait AstMetaInfo {
  fn source_position(&self) -> &SourcePosition;

  fn source_position_mut(&mut self) -> &mut SourcePosition;

  fn is_expr(&self) -> bool;

  fn is_stmt(&self) -> bool {
    return !self.is_expr();
  }

  fn is_valid_lhs(&self) -> bool;

  fn set_valid_lhs(&mut self);

  fn unset_valid_lhs(&mut self);

  fn set_start_positon(&mut self, col: u32, line: u32) {
    self.source_position_mut().set_start_col(col);
    self.source_position_mut().set_start_line_number(line);
  }

  fn set_end_positon(&mut self, col: u32, line: u32) {
    self.source_position_mut().set_end_col(col);
    self.source_position_mut().set_end_line_number(line);
  }

  fn set_start_pos(&mut self, pos: u32) {
    self.source_position_mut().set_start_col(pos);
  }

  fn set_end_pos(&mut self, pos: u32) {
    self.source_position_mut().set_end_col(pos);
  }

  fn set_start_line_number(&mut self, line: u32) {
    self.source_position_mut().set_start_line_number(line);
  }

  fn set_end_line_number(&mut self, line: u32) {
    self.source_position_mut().set_end_line_number(line);
  }

  fn to_string(&self) -> String {
    let mut indent = "".to_string();
    let mut result = "".to_string();
    //    self::to_string_internal(self, &mut indent, &mut result);
    return result;
  }

  fn to_string_tree(&self) -> String {
    let mut indent = "".to_string();
    let mut result = "".to_string();
    //    self::to_string_tree_internal(self, &mut indent, &mut result);
    return result;
  }
}

macro_rules! _ast_enum {
  ($name:ident { $($item:ident(Node<$type:ty>),)* }) => {
    pub enum $name {
      $(
        $item(Node<$type>),
      )*
    }

    impl AstMetaInfo for $name {
      fn source_position(&self) -> &SourcePosition {
        return match self {
          $(
            &$name::$item(ref node) => node.source_position(),
          )*
        }
      }

      fn source_position_mut(&mut self) -> &mut SourcePosition {
        return match self {
          $(
            &mut $name::$item(ref mut node) => node.source_position_mut(),
          )*
        }
      }

      fn is_expr(&self) -> bool {
        return match self {
          $(
            &$name::$item(ref node) => node.is_expr(),
          )*
        }
      }

      fn is_valid_lhs(&self) -> bool {
        return match self {
          $(
            &$name::$item(ref node) => node.is_valid_lhs(),
          )*
        }
      }

      fn set_valid_lhs(&mut self) {
        match self {
          $(
            &mut $name::$item(ref mut node) => node.set_valid_lhs(),
          )*
        }
      }

      fn unset_valid_lhs(&mut self) {
        match self {
          $(
            &mut $name::$item(ref mut node) => node.unset_valid_lhs(),
          )*
        }
      }
    }
  }
}

_ast_enum! {
  Expr {
    FunctionExpression(Node<FunctionExpression>),
    BinaryExpression(Node<BinaryExpression>),
    Elision(Node<Elision>),
    Expressions(Node<Expressions>),
    CallExpression(Node<CallExpression>),
    ConditionalExpression(Node<ConditionalExpression>),
    Literal(Node<Literal>),
    ObjectPropertyExpression(Node<ObjectPropertyExpression>),
    PropertyAccessExpression(Node<PropertyAccessExpression>),
    StructualLiteral(Node<StructualLiteral>),
    UnaryExpression(Node<UnaryExpression>),
    ImportSpecifier(Node<ImportSpecifier>),
    ImportBinding(Node<ImportBinding>),
    NamedImportList(Node<NamedImportList>),
    NewExpression(Node<NewExpression>),
    TemplateLiteral(Node<TemplateLiteral>),
  }
}
macro_rules! impl_expr {
  ($name:tt) => {
    impl AstNode for $name {
      fn is_expr(&self) -> bool {
        return true;
      }
    }
    impl From<Node<$name>> for Expr {
      fn from(a: Node<$name>) -> Expr {
        return Expr::$name(a);
      }
    }

    impl From<Node<$name>> for Ast {
      fn from(a: Node<$name>) -> Ast {
        return Ast::Expr(Expr::$name(a));
      }
    }

    impl TryFrom<Expr> for Node<$name> {
      type Error = ();
      fn try_from(a: Expr) -> Result<Node<$name>, ()> {
        match a {
          Expr::$name(r) => Ok(r),
          _ => Err(()),
        }
      }
    }

    impl TryFrom<Ast> for Node<$name> {
      type Error = ();
      fn try_from(a: Ast) -> Result<Node<$name>, ()> {
        match a {
          Ast::Expr(r) => Node::<$name>::try_from(r),
          _ => Err(()),
        }
      }
    }
  };
}

_ast_enum! {
  Stmt {
    ImportDeclaration(Node<ImportDeclaration>),
    ExportDeclaration(Node<ExportDeclaration>),
    Statements(Node<Statements>),
    Statement(Node<Statement>),
  }
}
macro_rules! impl_stmt {
  ($name:tt) => {
    impl AstNode for $name {
      fn is_expr(&self) -> bool {
        return false;
      }
    }
    impl From<Node<$name>> for Stmt {
      fn from(a: Node<$name>) -> Stmt {
        return Stmt::$name(a);
      }
    }

    impl From<Node<$name>> for Ast {
      fn from(a: Node<$name>) -> Ast {
        return Ast::Stmt(Stmt::$name(a));
      }
    }

    impl TryFrom<Stmt> for Node<$name> {
      type Error = ();
      fn try_from(a: Stmt) -> Result<Node<$name>, ()> {
        match a {
          Stmt::$name(r) => Ok(r),
          _ => Err(()),
        }
      }
    }

    impl TryFrom<Ast> for Node<$name> {
      type Error = ();
      fn try_from(a: Ast) -> Result<Node<$name>, ()> {
        match a {
          Ast::Stmt(r) => Node::<$name>::try_from(r),
          _ => Err(()),
        }
      }
    }
  };
}

pub enum Ast {
  Expr(Expr),
  Stmt(Stmt),
}
impl AstMetaInfo for Ast {
  fn source_position(&self) -> &SourcePosition {
    return match self {
      &Ast::Expr(ref expr) => expr.source_position(),
      &Ast::Stmt(ref stmt) => stmt.source_position(),
    };
  }

  fn source_position_mut(&mut self) -> &mut SourcePosition {
    return match self {
      &mut Ast::Expr(ref mut expr) => expr.source_position_mut(),
      &mut Ast::Stmt(ref mut stmt) => stmt.source_position_mut(),
    };
  }

  fn is_expr(&self) -> bool {
    return match self {
      &Ast::Expr(_) => true,
      &Ast::Stmt(_) => false,
    };
  }

  fn is_valid_lhs(&self) -> bool {
    return match self {
      &Ast::Expr(ref expr) => expr.is_valid_lhs(),
      &Ast::Stmt(_) => false,
    };
  }

  fn set_valid_lhs(&mut self) {
    match self {
      &mut Ast::Expr(ref mut expr) => expr.set_valid_lhs(),
      _ => {}
    };
  }

  fn unset_valid_lhs(&mut self) {
    match self {
      &mut Ast::Expr(ref mut expr) => expr.unset_valid_lhs(),
      _ => {}
    };
  }
}
impl From<Expr> for Ast {
  fn from(a: Expr) -> Ast {
    return Ast::Expr(a);
  }
}
impl From<Stmt> for Ast {
  fn from(a: Stmt) -> Ast {
    return Ast::Stmt(a);
  }
}
impl std::convert::TryFrom<Ast> for Stmt {
  type Error = ();
  fn try_from(a: Ast) -> Result<Stmt, ()> {
    return match a {
      Ast::Stmt(st) => Ok(st),
      _ => Err(()),
    };
  }
}
impl std::convert::TryFrom<Ast> for Expr {
  type Error = ();
  fn try_from(a: Ast) -> Result<Expr, ()> {
    return match a {
      Ast::Expr(exp) => Ok(exp),
      _ => Err(()),
    };
  }
}

pub struct AstLayout<T: AstNode> {
  meta: AstMetadata,
  kind: T,
}
impl<T: AstNode> Deref for AstLayout<T> {
  type Target = T;
  fn deref(&self) -> &Self::Target {
    return &self.kind;
  }
}
impl<T: AstNode> DerefMut for AstLayout<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return &mut self.kind;
  }
}

pub struct Node<T: AstNode>(Exotic<AstLayout<T>>);
impl<T: AstNode> Node<T> {
  pub fn new(region: &mut Region, object: T) -> Node<T> {
    return Node(region.alloc(AstLayout {
      meta: AstMetadata::new(),
      kind: object,
    }));
  }
}

impl<T: AstNode> AstMetaInfo for Node<T> {
  fn source_position(&self) -> &SourcePosition {
    return &self.0.as_ref().meta.source_position;
  }

  fn source_position_mut(&mut self) -> &mut SourcePosition {
    return &mut self.0.as_mut().meta.source_position;
  }

  fn is_expr(&self) -> bool {
    return self.0.as_ref().kind.is_expr();
  }

  fn is_valid_lhs(&self) -> bool {
    return self.0.as_ref().meta.flags.get(IS_VALID_LHS);
  }

  fn set_valid_lhs(&mut self) {
    self.0.as_mut().meta.flags.set(IS_VALID_LHS);
  }

  fn unset_valid_lhs(&mut self) {
    self.0.as_mut().meta.flags.unset(IS_VALID_LHS);
  }
}
impl<T: AstNode> Deref for Node<T> {
  type Target = AstLayout<T>;
  fn deref(&self) -> &Self::Target {
    return self.0.as_ref();
  }
}
impl<T: AstNode> DerefMut for Node<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    return self.0.as_mut();
  }
}

pub struct Expressions {
  items: Vec<Expr>,
}
impl_expr!(Expressions);

impl Expressions {
  #[inline]
  pub fn new(region: &mut Region) -> Node<Expressions> {
    return Node::<Expressions>::new(
      region,
      Expressions {
        items: Vec::<Expr>::new(),
      },
    );
  }
}

pub struct Statements {
  items: Vec<Stmt>,
}
impl_stmt!(Statements);

impl Statements {
  #[inline]
  pub fn new(region: &mut Region) -> Node<Statements> {
    return Node::<Statements>::new(
      region,
      Statements {
        items: Vec::<Stmt>::new(),
      },
    );
  }
}

#[derive(Property)]
pub struct Statement {
  #[property(get(type = "ref"))]
  expr: Expr,
}
impl_stmt!(Statement);

impl Statement {
  #[inline]
  pub fn new(region: &mut Region, expr: Expr) -> Node<Statement> {
    return Node::<Statement>::new(region, Statement { expr });
  }
}

#[derive(Property)]
pub struct BinaryExpression {
  #[property(get(type = "copy"))]
  op: Token,

  #[property(get(type = "ref"), set(type = "own"))]
  lhs: Expr,

  #[property(get(type = "ref"), set(type = "own"))]
  rhs: Expr,
}
impl_expr!(BinaryExpression);

impl BinaryExpression {
  #[inline]
  pub fn new(region: &mut Region, op: Token, lhs: Expr, rhs: Expr) -> Node<BinaryExpression> {
    return Node::<BinaryExpression>::new(region, BinaryExpression { op, lhs, rhs });
  }
}

#[derive(Copy, Clone)]
pub enum UnaryExpressionOperandPosition {
  Pre,
  Post,
}

#[derive(Property)]
pub struct UnaryExpression {
  #[property(get(type = "copy"))]
  position: UnaryExpressionOperandPosition,

  #[property(get(type = "copy"), set(type = "own"))]
  op: Token,

  #[property(get(type = "ref"), set(type = "own"))]
  target: Expr,
}
impl_expr!(UnaryExpression);

impl UnaryExpression {
  #[inline]
  pub fn new(
    region: &mut Region,
    position: UnaryExpressionOperandPosition,
    op: Token,
    target: Expr,
  ) -> Node<UnaryExpression> {
    return Node::<UnaryExpression>::new(region, UnaryExpression { position, op, target });
  }
}

#[derive(Property)]
pub struct ConditionalExpression {
  #[property(get(type = "ref"), set(type = "own"))]
  condition: Expr,

  #[property(get(type = "ref"), set(type = "own"))]
  then_expr: Expr,

  #[property(get(type = "ref"), set(type = "own"))]
  else_expr: Expr,
}
impl_expr!(ConditionalExpression);
impl ConditionalExpression {
  #[inline]
  pub fn new(region: &mut Region, condition: Expr, then_expr: Expr, else_expr: Expr) -> Node<ConditionalExpression> {
    return Node::<ConditionalExpression>::new(
      region,
      ConditionalExpression {
        condition,
        then_expr,
        else_expr,
      },
    );
  }
}

#[derive(Property)]
pub struct NewExpression {
  #[property(get(type = "ref"), set(type = "own"))]
  callee: Expr,
}
impl_expr!(NewExpression);

impl NewExpression {
  #[inline]
  pub fn new(region: &mut Region, callee: Expr) -> Node<NewExpression> {
    return Node::<NewExpression>::new(region, NewExpression { callee });
  }
}

pub enum CallReceiverType {
  Expr = 0,
  New = 0x4,
  Super = 0x8,
  Template = 0x10,
  None = 0x20,
}

pub struct CallExpression {
  callee: Expr,
  receiver: CallReceiverType,
  parameters: Option<Node<Expressions>>,
}
impl_expr!(CallExpression);

pub struct PropertyAccessExpression {
  flags: Bitset<u8>,
  receiver: Option<Expr>,
  property: Option<Expr>,
}
impl_expr!(PropertyAccessExpression);

pub enum FunctionType {
  Scoped,
  NonScoped,
  Generator,
}

pub struct FunctionExpression {
  function_type: FunctionType,
  formal_parameters: Node<Expressions>,
}
impl_expr!(FunctionExpression);

pub struct ObjectPropertyExpression {
  key: Expr,
  value: Expr,
  init: Option<Expr>,
}
impl_expr!(ObjectPropertyExpression);

pub struct Elision;
impl_expr!(Elision);

pub struct StructualLiteral {
  properties: Node<Expressions>,
}
impl_expr!(StructualLiteral);

pub struct Literal {
  literal_type: Token,
  value: FixedU16CodePointArray,
}
impl_expr!(Literal);

pub struct TemplateLiteral {
  parts: Node<Expressions>,
}
impl_expr!(TemplateLiteral);

pub struct ImportSpecifier {
  is_namespace: bool,
  name: Option<Expr>,
  as_expr: Option<Expr>,
}
impl_expr!(ImportSpecifier);

pub struct NamedImportList {
  list: Node<Expressions>,
}
impl_expr!(NamedImportList);

pub struct ImportBinding {
  default_binding: Option<Expr>,
  namespace_import: Option<Expr>,
  named_import_list: Option<Expr>,
}
impl_expr!(ImportBinding);

pub struct ImportDeclaration {
  import_binding: Option<Expr>,
  module_specifier: Expr,
}
impl_stmt!(ImportDeclaration);

pub struct ExportDeclaration {
  flags: Bitset<u8>,
  export_clause: Option<Expr>,
  from_clause: Option<Expr>,
}
impl_stmt!(ExportDeclaration);
