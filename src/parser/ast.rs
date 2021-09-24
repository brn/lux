use super::scope::Scope;
use super::source_position::RuntimeSourcePosition;
use super::source_position::*;
use super::token::Token;
use crate::property::Property;
use crate::structs::{FixedU16CodePointArray, Repr};
use crate::utility::*;
use std::ops::{Deref, DerefMut};

pub use std::convert::TryFrom;

pub trait AstNode {
  fn is_expr(&self) -> bool;
  fn is_stmt(&self) -> bool {
    return !self.is_expr();
  }

  fn to_string(&self, indent: &mut String, result: &mut String, source_positon: &RuntimeSourcePosition);

  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_positon: &RuntimeSourcePosition);
}

pub trait Is<T> {
  fn is(value: T) -> bool;
}

bitflags! {
  pub struct AstMetaFlag: u8 {
    const NONE = 0;
    const PARENTHESIZED = 1;
  }
}

#[derive(Copy, Clone)]
pub struct AstMetadata {
  flag: AstMetaFlag,
  source_position: RuntimeSourcePosition,
}
impl AstMetadata {
  fn new() -> Self {
    return AstMetadata {
      flag: AstMetaFlag::NONE,
      source_position: RuntimeSourcePosition::new(0, 0),
    };
  }
}

pub trait AstStringify {
  fn to_string(&self) -> String;

  fn to_string_tree(&self) -> String;
}

trait AstStringifyInternal {
  fn to_string_internal(&self, indent: &mut String, result: &mut String);

  fn to_string_tree_internal(&self, indent: &mut String, result: &mut String);
}

pub trait AstMetaInfo {
  fn source_position(&self) -> &RuntimeSourcePosition;

  fn source_position_mut(&mut self) -> &mut RuntimeSourcePosition;

  fn is_expr(&self) -> bool;

  fn is_stmt(&self) -> bool {
    return !self.is_expr();
  }

  fn set_source_position(&mut self, pos: &RuntimeSourcePosition) {
    (*self.source_position_mut()) = *pos;
  }

  fn set_position(&mut self, pos: &RuntimeSourcePosition) {
    self.source_position_mut().set_col(pos.col());
    self.source_position_mut().set_line_number(pos.line_number());
  }

  fn set_col(&mut self, pos: u32) {
    self.source_position_mut().set_col(pos);
  }

  fn set_line_number(&mut self, line: u32) {
    self.source_position_mut().set_line_number(line);
  }
}

macro_rules! _ast_enum {
  (@expr, $name:ident { $($item:ident(Node<$type:ty>),)* }) => {
    _ast_enum!($name { $($item(Node<$type>),)* });
    impl $name {
      pub fn set_parenthesized(&mut self) {
        return match self {
          $(
            &mut $name::$item(mut node) => node.set_parenthesized(),
          )*
        }
      }

      pub fn is_parenthesized(&self) -> bool {
        return match self {
          $(
            &$name::$item(node) => node.is_parenthesized(),
          )*
        }
      }
    }
  };
  (@both, $name:ident { $($item:ident(Node<$type:ty>),)* }) => {
    _ast_enum!(@expr, $name { $($item(Node<$type>),)* });

    impl TryFrom<$name> for Expr {
      type Error = ();
      fn try_from(a: $name) -> Result<Expr, ()> {
        match a {
          $(
            $name::$item(node) => Ok(Expr::from(node)),
          )*
          _ => Err(()),
        }
      }
    }

    impl TryFrom<$name> for Stmt {
      type Error = ();
      fn try_from(a: $name) -> Result<Stmt, ()> {
        match a {
          $(
            $name::$item(node) => Ok(Stmt::from(node)),
          )*
          _ => Err(()),
        }
      }
    }
  };
  ($name:ident { $($item:ident(Node<$type:ty>),)* }) => {
    #[derive(Copy, Clone, Debug)]
    pub enum $name {
      $(
        $item(Node<$type>),
      )*
    }

    impl AstStringifyInternal for $name {
      fn to_string_internal(&self, indent: &mut String, result: &mut String) {
        return match self {
          $(
            &$name::$item(ref node) => node.to_string_internal(indent, result),
          )*
        }
      }

      fn to_string_tree_internal(&self, indent: &mut String, result: &mut String) {
        return match self {
          $(
            &$name::$item(ref node) => node.to_string_tree_internal(indent, result),
          )*
        }
      }
    }

    impl AstStringify for $name {
      fn to_string(&self) -> String {
        return match self {
          $(
            &$name::$item(ref node) => node.to_string(),
          )*
        }
      }

      fn to_string_tree(&self) -> String {
        return match self {
          $(
            &$name::$item(ref node) => node.to_string_tree(),
          )*
        }
      }
    }

    impl AstMetaInfo for $name {
      fn source_position(&self) -> &RuntimeSourcePosition {
        return match self {
          $(
            &$name::$item(ref node) => node.source_position(),
          )*
        }
      }

      fn source_position_mut(&mut self) -> &mut RuntimeSourcePosition {
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
    }
  }
}

_ast_enum! {
  @expr,
  Expr {
    Empty(Node<Empty>),
    Function(Node<Function>),
    BinaryExpression(Node<BinaryExpression>),
    Elision(Node<Elision>),
    Expressions(Node<Expressions>),
    CallExpression(Node<CallExpression>),
    ConditionalExpression(Node<ConditionalExpression>),
    Literal(Node<Literal>),
    ObjectPropertyExpression(Node<ObjectPropertyExpression>),
    PropertyAccessExpression(Node<PropertyAccessExpression>),
    StructuralLiteral(Node<StructuralLiteral>),
    UnaryExpression(Node<UnaryExpression>),
    ImportSpecifier(Node<ImportSpecifier>),
    ImportBinding(Node<ImportBinding>),
    NamedImportList(Node<NamedImportList>),
    NewExpression(Node<NewExpression>),
    TemplateLiteral(Node<TemplateLiteral>),
    Class(Node<Class>),
    ImportMetaExpression(Node<ImportMetaExpression>),
    SkipExpr(Node<SkipExpr>),
    SkipAny(Node<SkipAny>),
  }
}
macro_rules! impl_expr {
  ($name:tt, $to_string:item, $to_string_tree:item) => {
    impl AstNode for $name {
      fn is_expr(&self) -> bool {
        return true;
      }

      $to_string

      $to_string_tree
    }

    impl Node<$name> {
      fn set_parenthesized(&mut self) {
        self.0.meta.flag |= AstMetaFlag::PARENTHESIZED;
      }

      fn is_parenthesized(&self) -> bool {
        return self.0.meta.flag.contains(AstMetaFlag::PARENTHESIZED);
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

    impl Is<Expr> for Node<$name> {
      fn is(value: Expr) -> bool {
        match value {
          Expr::$name(_) => true,
          _ => false
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

    impl Is<Ast> for Node<$name> {
      fn is(value: Ast) -> bool {
        match value {
          Ast::Expr(expr) => Node::<$name>::is(expr),
          _ => false
        }
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
  };
}

_ast_enum! {
  Stmt {
    Empty(Node<Empty>),
    ImportDeclaration(Node<ImportDeclaration>),
    ExportDeclaration(Node<ExportDeclaration>),
    Statements(Node<Statements>),
    Statement(Node<Statement>),
    Function(Node<Function>),
    BlockStatement(Node<BlockStatement>),
    ForStatement(Node<ForStatement>),
    ForInStatement(Node<ForInStatement>),
    ForOfStatement(Node<ForOfStatement>),
    WhileStatement(Node<WhileStatement>),
    DoWhileStatement(Node<DoWhileStatement>),
    IfStatement(Node<IfStatement>),
    SwitchStatement(Node<SwitchStatement>),
    SwitchCase(Node<SwitchCase>),
    BreakStatement(Node<BreakStatement>),
    ContinueStatement(Node<ContinueStatement>),
    LabelledStatement(Node<LabelledStatement>),
    WithStatement(Node<WithStatement>),
    VariableDeclaration(Node<VariableDeclaration>),
    VariableDeclarations(Node<VariableDeclarations>),
    ClassField(Node<ClassField>),
    Class(Node<Class>),
    SkipStmt(Node<SkipStmt>),
    SkipAny(Node<SkipAny>),
  }
}

macro_rules! impl_stmt {
  ($name:tt, $to_string:item, $to_string_tree:item) => {
    impl AstNode for $name {
      fn is_expr(&self) -> bool {
        return false;
      }

      $to_string

      $to_string_tree
    }

    impl From<Node<$name>> for Ast {
      fn from(a: Node<$name>) -> Ast {
        return Ast::Stmt(Stmt::$name(a));
      }
    }

    impl Is<Ast> for Node<$name> {
      fn is(value: Ast) -> bool {
        match value {
          Ast::Stmt(stmt) => Node::<$name>::is(stmt),
          _ => false,
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

    impl From<Node<$name>> for Stmt {
      fn from(a: Node<$name>) -> Stmt {
        return Stmt::$name(a);
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

    impl Is<Stmt> for Node<$name> {
      fn is(value: Stmt) -> bool {
        match value {
          Stmt::$name(_) => true,
          _ => false,
        }
      }
    }
  };
}

_ast_enum! {
  @both,
  AnyAst {
    Empty(Node<Empty>),
    Function(Node<Function>),
    Class(Node<Class>),
    SkipAny(Node<SkipAny>),
  }
}

macro_rules! impl_both {
  ($name:tt, $to_string:item, $to_string_tree:item) => {
    impl AstNode for $name {
      fn is_expr(&self) -> bool {
        return true;
      }

      $to_string

      $to_string_tree
    }

    impl Is<AnyAst> for Node<$name> {
      fn is(value: AnyAst) -> bool {
        match value {
          AnyAst::$name(_) => true,
          _ => false,
        }
      }
    }

    impl Is<Ast> for Node<$name> {
      fn is(value: Ast) -> bool {
        match value {
          Ast::Any(node) => Node::<$name>::is(node),
          _ => false,
        }
      }
    }

    impl Is<Stmt> for Node<$name> {
      fn is(value: Stmt) -> bool {
        match value {
          Stmt::$name(_) => true,
          _ => false,
        }
      }
    }

    impl Is<Expr> for Node<$name> {
      fn is(value: Expr) -> bool {
        match value {
          Expr::$name(_) => true,
          _ => false,
        }
      }
    }

    impl Node<$name> {
      fn set_parenthesized(&mut self) {
        self.0.meta.flag |= AstMetaFlag::PARENTHESIZED;
      }

      fn is_parenthesized(&self) -> bool {
        return self.0.meta.flag.contains(AstMetaFlag::PARENTHESIZED);
      }
    }

    impl TryFrom<AnyAst> for Node<$name> {
      type Error = ();
      fn try_from(a: AnyAst) -> Result<Node<$name>, ()> {
        match a {
          AnyAst::$name(n) => Ok(n),
          _ => Err(()),
        }
      }
    }

    impl TryFrom<Ast> for Node<$name> {
      type Error = ();
      fn try_from(a: Ast) -> Result<Node<$name>, ()> {
        match a {
          Ast::Any(r) => Node::<$name>::try_from(r),
          _ => Err(()),
        }
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

    impl TryFrom<Expr> for Node<$name> {
      type Error = ();
      fn try_from(a: Expr) -> Result<Node<$name>, ()> {
        match a {
          Expr::$name(r) => Ok(r),
          _ => Err(()),
        }
      }
    }

    impl From<Node<$name>> for Expr {
      fn from(a: Node<$name>) -> Expr {
        return Expr::$name(a);
      }
    }

    impl From<Node<$name>> for Stmt {
      fn from(a: Node<$name>) -> Stmt {
        return Stmt::$name(a);
      }
    }

    impl From<Node<$name>> for AnyAst {
      fn from(a: Node<$name>) -> AnyAst {
        return AnyAst::$name(a);
      }
    }

    impl From<Node<$name>> for Ast {
      fn from(a: Node<$name>) -> Ast {
        return Ast::Any(a.into());
      }
    }
  }
}

#[derive(Copy, Clone, Debug)]
pub enum Ast {
  Expr(Expr),
  Stmt(Stmt),
  Any(AnyAst),
}

impl AstStringifyInternal for Ast {
  fn to_string_internal(&self, indent: &mut String, result: &mut String) {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_internal(indent, result),
      &Ast::Stmt(ref stmt) => stmt.to_string_internal(indent, result),
      &Ast::Any(ref any) => any.to_string_internal(indent, result),
    };
  }

  fn to_string_tree_internal(&self, indent: &mut String, result: &mut String) {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_tree_internal(indent, result),
      &Ast::Stmt(ref stmt) => stmt.to_string_tree_internal(indent, result),
      &Ast::Any(ref any) => any.to_string_tree_internal(indent, result),
    };
  }
}

impl AstStringify for Ast {
  fn to_string(&self) -> String {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string(),
      &Ast::Stmt(ref stmt) => stmt.to_string(),
      &Ast::Any(ref any) => any.to_string(),
    };
  }

  fn to_string_tree(&self) -> String {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_tree(),
      &Ast::Stmt(ref stmt) => stmt.to_string_tree(),
      &Ast::Any(ref any) => any.to_string_tree(),
    };
  }
}

impl AstMetaInfo for Ast {
  fn source_position(&self) -> &RuntimeSourcePosition {
    return match self {
      &Ast::Expr(ref expr) => expr.source_position(),
      &Ast::Stmt(ref stmt) => stmt.source_position(),
      &Ast::Any(ref any) => any.source_position(),
    };
  }

  fn source_position_mut(&mut self) -> &mut RuntimeSourcePosition {
    return match self {
      &mut Ast::Expr(ref mut expr) => expr.source_position_mut(),
      &mut Ast::Stmt(ref mut stmt) => stmt.source_position_mut(),
      &mut Ast::Any(ref mut any) => any.source_position_mut(),
    };
  }

  fn is_expr(&self) -> bool {
    return match self {
      &Ast::Expr(_) => true,
      &Ast::Stmt(_) => false,
      &Ast::Any(_) => true,
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
impl From<AnyAst> for Ast {
  fn from(a: AnyAst) -> Ast {
    return Ast::Any(a);
  }
}
impl std::convert::TryFrom<Ast> for Stmt {
  type Error = ();
  fn try_from(a: Ast) -> Result<Stmt, ()> {
    return match a {
      Ast::Stmt(st) => Ok(st),
      Ast::Any(st) => Stmt::try_from(st),
      _ => Err(()),
    };
  }
}
impl std::convert::TryFrom<Ast> for Expr {
  type Error = ();
  fn try_from(a: Ast) -> Result<Expr, ()> {
    return match a {
      Ast::Expr(exp) => Ok(exp),
      Ast::Any(exp) => Expr::try_from(exp),
      _ => Err(()),
    };
  }
}
impl std::convert::TryFrom<Ast> for AnyAst {
  type Error = ();
  fn try_from(a: Ast) -> Result<AnyAst, ()> {
    return match a {
      Ast::Any(exp) => Ok(exp),
      _ => Err(()),
    };
  }
}

#[derive(Copy, Clone)]
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
impl<T: AstNode> std::fmt::Debug for Node<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return writeln!(f, "Node<{}>", std::any::type_name::<T>());
  }
}
impl<T: AstNode> Copy for Node<T> {}

impl<T: AstNode> Clone for Node<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T: AstNode> Node<T> {
  pub fn new(region: &mut Region, object: T) -> Node<T> {
    return Node(region.alloc(AstLayout {
      meta: AstMetadata::new(),
      kind: object,
    }));
  }
}

impl<T: AstNode> AstStringify for Node<T> {
  fn to_string(&self) -> String {
    let mut indent = "".to_string();
    let mut result = "".to_string();
    self.kind.to_string(&mut indent, &mut result, self.source_position());
    return result;
  }

  fn to_string_tree(&self) -> String {
    let mut indent = "".to_string();
    let mut result = "".to_string();
    self.kind.to_string_tree(&mut indent, &mut result, self.source_position());
    return result;
  }
}

impl<T: AstNode> AstStringifyInternal for Node<T> {
  fn to_string_internal(&self, indent: &mut String, result: &mut String) {
    self.kind.to_string(indent, result, self.source_position());
  }

  fn to_string_tree_internal(&self, indent: &mut String, result: &mut String) {
    self.kind.to_string_tree(indent, result, self.source_position());
  }
}

impl<T: AstNode> AstMetaInfo for Node<T> {
  fn source_position(&self) -> &RuntimeSourcePosition {
    return &self.0.as_ref().meta.source_position;
  }

  fn source_position_mut(&mut self) -> &mut RuntimeSourcePosition {
    return &mut self.0.as_mut().meta.source_position;
  }

  fn is_expr(&self) -> bool {
    return self.0.as_ref().kind.is_expr();
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

fn to_string_list<T: AstStringifyInternal>(list: &Vec<T>, indent: &mut String, result: &mut String) {
  let mut ni = format!("  {}", indent);
  for n in list {
    n.to_string_tree_internal(&mut ni, result);
  }
}

pub struct Empty;
impl_both!(
  Empty,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Empty {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl Empty {
  #[inline]
  pub fn new(region: &mut Region) -> Node<Empty> {
    return Node::<Empty>::new(region, Empty);
  }
}

pub trait NodeCollection<T> {
  fn list(&self) -> &Vec<T>;
  fn list_mut(&mut self) -> &mut Vec<T>;

  #[inline]
  fn at(&self, index: usize) -> Option<&T> {
    return self.list().get(index);
  }

  #[inline]
  fn at_mut(&mut self, index: usize) -> Option<&mut T> {
    return self.list_mut().get_mut(index);
  }

  #[inline]
  fn push(&mut self, expr: T) {
    self.list_mut().push(expr);
  }

  #[inline]
  fn last(&self) -> Option<&T> {
    return self.list().last();
  }

  #[inline]
  fn last_mut(&mut self) -> Option<&mut T> {
    return self.list_mut().last_mut();
  }

  #[inline]
  fn iter(&self) -> std::slice::Iter<T> {
    return self.list().iter();
  }

  #[inline]
  fn len(&self) -> usize {
    return self.list().len();
  }
}

bitflags! {
  pub struct ExpressionsType: u8 {
    const None = 0;
    const UniqueParameters = 1;
    const Parameters = 2;
  }
}

pub struct Expressions {
  expr_type: ExpressionsType,
  items: Vec<Expr>,
}
impl_expr!(
  Expressions,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Expressions {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.items, indent, result);
  }
);

impl Expressions {
  #[inline]
  pub fn new(region: &mut Region, items: Vec<Expr>) -> Node<Expressions> {
    return Node::<Expressions>::new(
      region,
      Expressions {
        expr_type: ExpressionsType::None,
        items,
      },
    );
  }

  pub fn set_exprs_type(&mut self, exprs_type: ExpressionsType) {
    self.expr_type |= exprs_type;
  }

  pub fn is_parameters(&self) -> bool {
    return self.expr_type.contains(ExpressionsType::Parameters);
  }

  pub fn is_unique_parameters(&self) -> bool {
    return self.expr_type.contains(ExpressionsType::UniqueParameters);
  }
}

impl NodeCollection<Expr> for Expressions {
  fn list(&self) -> &Vec<Expr> {
    return &self.items;
  }
  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return &mut self.items;
  }
}
impl NodeCollection<Expr> for Node<Expressions> {
  fn list(&self) -> &Vec<Expr> {
    return self.0.list();
  }
  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return self.0.list_mut();
  }
}

impl IntoIterator for Expressions {
  type Item = Expr;
  type IntoIter = std::vec::IntoIter<Self::Item>;
  fn into_iter(self) -> Self::IntoIter {
    return self.items.into_iter();
  }
}

pub struct Statements {
  items: Vec<Stmt>,
}
impl_stmt!(
  Statements,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Statements {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.items, indent, result);
  }
);

impl IntoIterator for Statements {
  type Item = Stmt;
  type IntoIter = std::vec::IntoIter<Self::Item>;
  fn into_iter(self) -> Self::IntoIter {
    return self.items.into_iter();
  }
}

impl Statements {
  #[inline]
  pub fn new(region: &mut Region, items: Vec<Stmt>) -> Node<Statements> {
    return Node::<Statements>::new(region, Statements { items });
  }

  #[inline]
  pub fn iter(&self) -> std::slice::Iter<Stmt> {
    return self.items.iter();
  }
}

impl NodeCollection<Stmt> for Statements {
  fn list(&self) -> &Vec<Stmt> {
    return &self.items;
  }
  fn list_mut(&mut self) -> &mut Vec<Stmt> {
    return &mut self.items;
  }
}
impl NodeCollection<Stmt> for Node<Statements> {
  fn list(&self) -> &Vec<Stmt> {
    return self.0.list();
  }
  fn list_mut(&mut self) -> &mut Vec<Stmt> {
    return self.0.list_mut();
  }
}

#[derive(Property)]
pub struct Statement {
  #[property(get(type = "copy"))]
  expr: Expr,
}
impl_stmt!(
  Statement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Statement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.expr.to_string_tree_internal(&mut ni, result);
  }
);

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

  #[property(get(type = "copy"), set(type = "ref"))]
  lhs: Expr,

  #[property(get(type = "copy"), set(type = "ref"))]
  rhs: Expr,
}
impl_expr!(
  BinaryExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[BinaryExpression operand = {} {}]\n",
      indent,
      self.op,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.lhs.to_string_tree_internal(&mut ni, result);
    self.rhs.to_string_tree_internal(&mut ni, result);
  }
);

impl BinaryExpression {
  #[inline]
  pub fn new(region: &mut Region, op: Token, lhs: Expr, rhs: Expr) -> Node<BinaryExpression> {
    return Node::<BinaryExpression>::new(region, BinaryExpression { op, lhs, rhs });
  }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryExpressionOperandPosition {
  Pre,
  Post,
}

#[derive(Property)]
pub struct UnaryExpression {
  #[property(get(type = "copy"))]
  position: UnaryExpressionOperandPosition,

  #[property(get(type = "copy"), set(type = "ref"))]
  op: Token,

  #[property(get(type = "copy"), set(type = "ref"))]
  target: Expr,
}
impl_expr!(
  UnaryExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[UnaryExpression operand = {} position = {:?} {}]\n",
      indent,
      self.op,
      self.position,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.target.to_string_tree_internal(&mut ni, result);
  }
);

impl UnaryExpression {
  #[inline]
  pub fn new(region: &mut Region, position: UnaryExpressionOperandPosition, op: Token, target: Expr) -> Node<UnaryExpression> {
    return Node::<UnaryExpression>::new(region, UnaryExpression { position, op, target });
  }
}

#[derive(Property)]
pub struct ConditionalExpression {
  #[property(get(type = "copy"), set(type = "ref"))]
  condition: Expr,

  #[property(get(type = "copy"), set(type = "ref"))]
  then_expr: Expr,

  #[property(get(type = "copy"), set(type = "ref"))]
  else_expr: Expr,
}
impl_expr!(
  ConditionalExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ConditionalExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.then_expr.to_string_tree_internal(&mut ni, result);
    self.else_expr.to_string_tree_internal(&mut ni, result);
  }
);

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
  #[property(get(type = "copy"), set(type = "ref"))]
  callee: Expr,
}
impl_expr!(
  NewExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[NewExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.callee.to_string_tree_internal(&mut ni, result);
  }
);
impl NewExpression {
  #[inline]
  pub fn new(region: &mut Region, callee: Expr) -> Node<Self> {
    return Node::<NewExpression>::new(region, NewExpression { callee });
  }
}

pub struct ImportMetaExpression;
impl_expr!(
  ImportMetaExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ImportMetaExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl ImportMetaExpression {
  #[inline]
  pub fn new(region: &mut Region) -> Node<Self> {
    return Node::<ImportMetaExpression>::new(region, ImportMetaExpression);
  }
}

bitflags! {
  pub struct CallReceiverType: u16 {
    const Expr = 0;
    const New = 0x8;
    const Super = 0x10;
    const Import = 0x20;
    const Template = 0x40;
    const None = 0x80;
  }
}

#[derive(Property)]
pub struct CallExpression {
  #[property(get(type = "copy"))]
  callee: Option<Expr>,

  #[property(get(type = "copy"))]
  receiver: CallReceiverType,

  #[property(get(type = "copy"))]
  parameters: Option<Expr>,
}
impl_expr!(
  CallExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[CallExpression receiver = {:?} {}]\n",
      indent,
      self.receiver,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(callee) = self.callee {
      callee.to_string_tree_internal(&mut ni, result);
    }
    if let Some(node) = self.parameters {
      node.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl CallExpression {
  #[inline]
  pub fn new(region: &mut Region, receiver: CallReceiverType, callee: Option<Expr>, parameters: Option<Expr>) -> Node<CallExpression> {
    return Node::<CallExpression>::new(
      region,
      CallExpression {
        callee,
        receiver,
        parameters,
      },
    );
  }
}

bitflags! {
pub struct PropertyAccessType: u16 {
  const Dot = 1;
  const Element = 2;
  const OptionalChaining = 4;
}
}

pub struct PropertyAccessExpression {
  flags: Bitset<u16>,
  receiver: Option<Expr>,
  property: Option<Expr>,
}
impl_expr!(
  PropertyAccessExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[PropertyAccessExpression property_access = {} {}]\n",
      indent,
      if self.is_dot_access() {
        "dot"
      } else if self.is_element_access() {
        "element"
      } else if self.is_meta_property() {
        "meta"
      } else if self.is_op_chaining_access() {
        "op_chaining"
      } else {
        "super"
      },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref receiver) = self.receiver {
      receiver.to_string_tree_internal(&mut ni, result);
    }
    if let Some(ref property) = self.property {
      property.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl PropertyAccessExpression {
  #[inline]
  pub fn new(
    region: &mut Region,
    property_access_type: PropertyAccessType,
    receiver_type: CallReceiverType,
    receiver: Option<Expr>,
    property: Option<Expr>,
  ) -> Node<PropertyAccessExpression> {
    return Node::<PropertyAccessExpression>::new(
      region,
      PropertyAccessExpression {
        flags: Bitset::<u16>::with(property_access_type.bits() | receiver_type.bits()),
        receiver,
        property,
      },
    );
  }

  #[inline(always)]
  pub fn is_dot_access(&self) -> bool {
    return (self.flags.bits() & PropertyAccessType::Dot.bits()) > 0;
  }

  #[inline(always)]
  pub fn is_element_access(&self) -> bool {
    return (self.flags.bits() & PropertyAccessType::Element.bits()) > 0;
  }

  #[inline(always)]
  pub fn is_op_chaining_access(&self) -> bool {
    return (self.flags.bits() & PropertyAccessType::OptionalChaining.bits()) > 0;
  }

  #[inline(always)]
  pub fn is_meta_property(&self) -> bool {
    return (self.flags.bits() & CallReceiverType::New.bits()) > 0;
  }

  #[inline(always)]
  pub fn is_super_property(&self) -> bool {
    return (self.flags.bits() & CallReceiverType::Super.bits()) > 0;
  }
}

bitflags! {
  pub struct FunctionAttribute: u8 {
    const NONE = 0;
    const GETTER = 0x1;
    const SETTER = 0x2;
    const ASYNC = 0x4;
    const GENERATOR = 0x8;
    const CONSTRUCTOR = 0x10;
  }
}

#[derive(Property)]
pub struct Function {
  #[property(get(type = "copy"))]
  name: Option<Expr>,

  #[property(skip)]
  attr: FunctionAttribute,

  #[property(get(type = "copy"))]
  formal_parameters: Expr,

  #[property(get(type = "copy"))]
  function_body: Option<Ast>,

  #[property(get(type = "copy"))]
  scope: Exotic<Scope>,

  #[property(get(type = "copy"), set(type = "ref"))]
  function_body_start: u32,

  #[property(get(type = "copy"), set(type = "ref"))]
  function_body_end: u32,
}
impl_both!(
  Function,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[Function type = {}{}{} body_start = {} body_end = {} {:?} {}]\n",
      indent,
      if self.is_arrow_function() {
        "ArrowFunction"
      } else if self.is_generator_function() {
        "Generator"
      } else {
        "Function"
      },
      if self.is_async_function() { " async" } else { "" },
      if self.is_getter() {
        " get"
      } else if self.is_setter() {
        " set"
      } else {
        ""
      },
      self.function_body_start,
      self.function_body_end,
      *self.scope,
      source_position.to_string(),
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref name) = self.name {
      name.to_string_tree_internal(&mut ni, result);
    }
    self.formal_parameters.to_string_tree_internal(&mut ni, result);
    if let Some(body) = self.function_body {
      body.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl Function {
  pub fn new(
    region: &mut Region,
    name: Option<Expr>,
    attr: FunctionAttribute,
    scope: Exotic<Scope>,
    formal_parameters: Expr,
    function_body: Option<Ast>,
    function_body_start: u32,
    function_body_end: u32,
  ) -> Node<Function> {
    return Node::new(
      region,
      Function {
        name,
        attr,
        scope,
        formal_parameters,
        function_body,
        function_body_start,
        function_body_end,
      },
    );
  }

  #[inline(always)]
  pub fn is_arrow_function(&self) -> bool {
    return self.scope.is_transparent();
  }

  #[inline(always)]
  pub fn is_generator_function(&self) -> bool {
    return self.attr.contains(FunctionAttribute::GENERATOR);
  }

  #[inline(always)]
  pub fn is_async_function(&self) -> bool {
    return self.attr.contains(FunctionAttribute::ASYNC);
  }

  #[inline(always)]
  pub fn is_getter(&self) -> bool {
    return self.attr.contains(FunctionAttribute::GETTER);
  }

  #[inline(always)]
  pub fn is_setter(&self) -> bool {
    return self.attr.contains(FunctionAttribute::SETTER);
  }

  #[inline(always)]
  pub fn is_constructor(&self) -> bool {
    return self.attr.contains(FunctionAttribute::CONSTRUCTOR);
  }
}

bitflags! {
  pub struct ClassFieldFlag: u8 {
    const NONE = 0;
    const PUBLIC = 0x1;
    const PRIVATE = 0x2;
    const STATIC = 0x4;
  }
}

#[derive(Property)]
pub struct ClassField {
  #[property(get(type = "copy"))]
  flags: ClassFieldFlag,

  #[property(get(type = "copy"))]
  value: Expr,
}
impl_stmt!(
  ClassField,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[ClassField{}{} {}]\n",
      indent,
      if self.is_static() { " static" } else { "" },
      if self.is_public() {
        " public"
      } else if self.is_private() {
        " private"
      } else {
        ""
      },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.value.to_string_tree_internal(&mut ni, result);
  }
);

impl ClassField {
  pub fn new(region: &mut Region, flags: ClassFieldFlag, value: Expr) -> Node<Self> {
    return Node::<ClassField>::new(region, ClassField { flags, value });
  }

  pub fn is_public(&self) -> bool {
    return self.flags.contains(ClassFieldFlag::PUBLIC);
  }

  pub fn is_private(&self) -> bool {
    return self.flags.contains(ClassFieldFlag::PRIVATE);
  }

  pub fn is_static(&self) -> bool {
    return self.flags.contains(ClassFieldFlag::STATIC);
  }
}

#[derive(Property)]
pub struct Class {
  #[property(get(type = "copy"))]
  name: Option<Expr>,

  #[property(get(type = "copy"))]
  heritage: Option<Expr>,

  #[property(get(type = "ref"))]
  methods: Vec<Stmt>,

  #[property(get(type = "ref"))]
  fields: Vec<Stmt>,
}
impl Class {
  pub fn new(region: &mut Region, name: Option<Expr>, heritage: Option<Expr>, methods: Vec<Stmt>, fields: Vec<Stmt>) -> Node<Self> {
    return Node::<Class>::new(
      region,
      Class {
        name,
        heritage,
        methods,
        fields,
      },
    );
  }
}
impl_both!(
  Class,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Class {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(name) = self.name {
      name.to_string_tree_internal(&mut ni, result);
    }
    if let Some(heritage) = self.heritage {
      heritage.to_string_tree_internal(&mut ni, result);
    }
    to_string_list(&self.fields, indent, result);
    to_string_list(&self.methods, indent, result);
  }
);

#[derive(Property)]
pub struct ObjectPropertyExpression {
  #[property(get(type = "copy"))]
  key: Expr,

  #[property(get(type = "copy"))]
  value: Option<Expr>,

  #[property(get(type = "copy"))]
  init: Option<Expr>,
}
impl_expr!(
  ObjectPropertyExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ObjectPropertyExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.key.to_string_tree_internal(&mut ni, result);
    if let Some(val) = self.value {
      val.to_string_tree_internal(&mut ni, result);
    }
    if let Some(init) = self.init {
      init.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl ObjectPropertyExpression {
  pub fn new(region: &mut Region, key: Expr, value: Option<Expr>, init: Option<Expr>) -> Node<ObjectPropertyExpression> {
    return Node::new(region, ObjectPropertyExpression { key, value, init });
  }
}

pub struct Elision;
impl_expr!(
  Elision,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[Elision {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl Elision {
  pub fn new(region: &mut Region) -> Node<Elision> {
    return Node::new(region, Elision {});
  }
}

bitflags! {
  pub struct StructuralLiteralFlags: u8 {
    const NONE = 0;
    const ARRAY = 1;
    const OBJECT = 2;
    const VALID_PATTERN = 4;
    const VALID_VALUE = 8;
    const HAS_ACCESSOR = 16;
    const HAS_GENERATOR = 32;
    const HAS_SPREAD = 64;
  }
}

#[derive(Property)]
pub struct StructuralLiteral {
  #[property(skip)]
  flag: StructuralLiteralFlags,

  #[property(skip)]
  first_object_property_name_initializer_position: Option<SourcePosition>,

  #[property(get(type = "ref"))]
  properties: Vec<Expr>,
}
impl_expr!(
  StructuralLiteral,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[StructuralLiteral type = {}{} {}]\n",
      indent,
      if self.is_array_literal() { "ArrayLiteral" } else { "ObjectLiteral" },
      format!(
        "{}{}{}",
        if self.has_accessor() { " accessor = true" } else { "" },
        if self.has_generator() { " generator = true" } else { "" },
        if self.has_spread() { " spread = true" } else { "" }
      ),
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(self.list(), indent, result);
  }
);

impl NodeCollection<Expr> for StructuralLiteral {
  fn list(&self) -> &Vec<Expr> {
    return &self.properties;
  }

  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return &mut self.properties;
  }
}

impl NodeCollection<Expr> for Node<StructuralLiteral> {
  fn list(&self) -> &Vec<Expr> {
    return self.0.list();
  }
  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return self.0.list_mut();
  }
}

impl StructuralLiteral {
  #[inline]
  pub fn new(region: &mut Region, literal_flag: StructuralLiteralFlags, properties: Vec<Expr>) -> Node<StructuralLiteral> {
    return Node::<StructuralLiteral>::new(
      region,
      StructuralLiteral {
        flag: StructuralLiteralFlags::VALID_VALUE | StructuralLiteralFlags::VALID_PATTERN | literal_flag,
        first_object_property_name_initializer_position: None,
        properties,
      },
    );
  }

  #[inline(always)]
  pub fn is_array_literal(&self) -> bool {
    return self.flag.contains(StructuralLiteralFlags::ARRAY);
  }

  #[inline(always)]
  pub fn is_object_literal(&self) -> bool {
    return self.flag.contains(StructuralLiteralFlags::OBJECT);
  }

  #[inline(always)]
  pub fn has_accessor(&self) -> bool {
    return self.flag.contains(StructuralLiteralFlags::HAS_ACCESSOR);
  }

  #[inline(always)]
  pub fn has_generator(&self) -> bool {
    return self.flag.contains(StructuralLiteralFlags::HAS_GENERATOR);
  }

  #[inline(always)]
  pub fn has_spread(&self) -> bool {
    return self.flag.contains(StructuralLiteralFlags::HAS_SPREAD);
  }
}

#[derive(Copy, Clone)]
pub enum LiteralValue {
  None,
  String(FixedU16CodePointArray, Token),
  Number(f64),
}

#[derive(Property)]
pub struct Literal {
  #[property(get(type = "copy"))]
  literal_type: Token,

  #[property(get(type = "copy"))]
  value: LiteralValue,
}
impl_expr!(
  Literal,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[Literal type = {:?} value = {} {}]\n",
      indent,
      self.literal_type,
      match self.value {
        LiteralValue::None => self.literal_type.symbol().to_string(),
        LiteralValue::String(s, _) => s.to_utf8(),
        LiteralValue::Number(f) => f.to_string(),
      },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl Literal {
  pub fn new(region: &mut Region, literal_type: Token, value: LiteralValue) -> Node<Literal> {
    return Node::new(region, Literal { literal_type, value });
  }

  pub fn is_identifier(&self) -> bool {
    return self.literal_type == Token::Identifier;
  }
}

pub struct TemplateLiteral {
  parts: Vec<Expr>,
}
impl_expr!(
  TemplateLiteral,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[TemplateLiteral {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.parts, indent, result);
  }
);

impl NodeCollection<Expr> for TemplateLiteral {
  fn list(&self) -> &Vec<Expr> {
    return &self.parts;
  }

  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return &mut self.parts;
  }
}
impl NodeCollection<Expr> for Node<TemplateLiteral> {
  fn list(&self) -> &Vec<Expr> {
    return self.0.list();
  }
  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return self.0.list_mut();
  }
}

impl TemplateLiteral {
  pub fn new(region: &mut Region, parts: Vec<Expr>) -> Node<TemplateLiteral> {
    return Node::new(region, TemplateLiteral { parts });
  }
}

pub struct ImportSpecifier {
  is_namespace: bool,
  name: Option<Expr>,
  as_expr: Option<Expr>,
}
impl_expr!(
  ImportSpecifier,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[ImportSpecifier is_namespace = {} {}]\n",
      indent,
      self.is_namespace,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref name) = self.name {
      name.to_string_tree_internal(&mut ni, result);
    }
    if let Some(ref expr) = self.as_expr {
      expr.to_string_tree_internal(&mut ni, result);
    }
  }
);
impl ImportSpecifier {
  pub fn new(region: &mut Region, is_namespace: bool, name: Option<Expr>, as_expr: Option<Expr>) -> Node<ImportSpecifier> {
    return Node::new(
      region,
      ImportSpecifier {
        is_namespace,
        name,
        as_expr,
      },
    );
  }
}

pub struct NamedImportList {
  list: Vec<Expr>,
}
impl_expr!(
  NamedImportList,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[NamedImportList {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.list, indent, result);
  }
);

impl NodeCollection<Expr> for NamedImportList {
  fn list(&self) -> &Vec<Expr> {
    return &self.list;
  }

  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return &mut self.list;
  }
}
impl NodeCollection<Expr> for Node<NamedImportList> {
  fn list(&self) -> &Vec<Expr> {
    return self.0.list();
  }
  fn list_mut(&mut self) -> &mut Vec<Expr> {
    return self.0.list_mut();
  }
}

impl NamedImportList {
  pub fn new(region: &mut Region, list: Vec<Expr>) -> Node<NamedImportList> {
    return Node::new(region, NamedImportList { list });
  }
}

#[derive(Property)]
pub struct ImportBinding {
  #[property(get(type = "copy"), set(type = "ref"))]
  default_binding: Option<Expr>,

  #[property(get(type = "copy"), set(type = "ref"))]
  namespace_import: Option<Expr>,

  #[property(get(type = "copy"), set(type = "ref"))]
  named_import_list: Option<Expr>,
}
impl_expr!(
  ImportBinding,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ImportBinding {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref default_binding) = self.default_binding {
      default_binding.to_string_tree_internal(&mut ni, result);
    }
    if let Some(ref namespace_import) = self.namespace_import {
      namespace_import.to_string_tree_internal(&mut ni, result);
    }
    if let Some(ref named_import_list) = self.named_import_list {
      named_import_list.to_string_tree_internal(&mut ni, result);
    }
  }
);
impl ImportBinding {
  pub fn new(
    region: &mut Region,
    default_binding: Option<Expr>,
    namespace_import: Option<Expr>,
    named_import_list: Option<Expr>,
  ) -> Node<ImportBinding> {
    return Node::new(
      region,
      ImportBinding {
        default_binding,
        namespace_import,
        named_import_list,
      },
    );
  }
}

#[derive(Property)]
pub struct ImportDeclaration {
  #[property(skip)]
  import_binding: Option<Expr>,

  #[property(get(type = "copy"))]
  module_specifier: Expr,
}
impl_stmt!(
  ImportDeclaration,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ImportDeclaration {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref import_binding) = self.import_binding {
      import_binding.to_string_tree_internal(&mut ni, result);
    }
    self.module_specifier.to_string_tree_internal(&mut ni, result);
  }
);
impl ImportDeclaration {
  pub fn new(region: &mut Region, import_binding: Option<Expr>, module_specifier: Expr) -> Node<ImportDeclaration> {
    return Node::new(
      region,
      ImportDeclaration {
        import_binding,
        module_specifier,
      },
    );
  }

  #[inline(always)]
  pub fn import_binding(&self) -> Option<&Expr> {
    return self.import_binding.as_ref();
  }
}

pub enum ExportDeclarationType {
  NamespaceExport,
  DefaultExport,
}
pub struct ExportDeclaration {
  flags: Bitset<u8>,
  export_clause: Option<Ast>,
  from_clause: Option<Ast>,
}
impl_stmt!(
  ExportDeclaration,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[ExportDeclaration type = {} {}]\n",
      indent,
      if self.is_namespace_export() { "namespace" } else { "default" },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref export_clause) = self.export_clause {
      export_clause.to_string_tree_internal(&mut ni, result);
    }
    if let Some(ref from_clause) = self.from_clause {
      from_clause.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl ExportDeclaration {
  #[inline]
  pub fn new(
    region: &mut Region,
    export_type: ExportDeclarationType,
    export_clause: Option<Ast>,
    from_clause: Option<Ast>,
  ) -> Node<ExportDeclaration> {
    return Node::<ExportDeclaration>::new(
      region,
      ExportDeclaration {
        flags: Bitset::<u8>::with(export_type as u8),
        export_clause,
        from_clause,
      },
    );
  }

  #[inline(always)]
  pub fn is_namespace_export(&self) -> bool {
    return self.flags.get(ExportDeclarationType::NamespaceExport as usize);
  }

  #[inline(always)]
  pub fn set_namespace_export(&mut self) {
    self.flags.set(ExportDeclarationType::NamespaceExport as usize);
  }

  #[inline(always)]
  pub fn is_default_export(&self) -> bool {
    return self.flags.get(ExportDeclarationType::DefaultExport as usize);
  }

  #[inline(always)]
  pub fn set_default_export(&mut self) {
    self.flags.set(ExportDeclarationType::DefaultExport as usize);
  }
}

#[derive(Property)]
pub struct BlockStatement {
  #[property(get(type = "copy"), set(disable))]
  stmt: Stmt,

  #[property(get(type = "copy"), set(disable))]
  scope: Exotic<Scope>,
}
impl_stmt!(
  BlockStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[BlockStatement {:?} {}]\n", indent, *self.scope, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.stmt.to_string_tree_internal(&mut ni, result);
  }
);
impl BlockStatement {
  pub fn new(region: &mut Region, stmt: Stmt, scope: Exotic<Scope>) -> Node<Self> {
    return Node::<Self>::new(region, BlockStatement { stmt, scope });
  }
}

#[derive(Property)]
pub struct ForStatement {
  #[property(get(type = "copy"), set(disable))]
  declarations: Ast,
  #[property(get(type = "copy"), set(disable))]
  condition: Expr,
  #[property(get(type = "copy"), set(disable))]
  computation: Expr,
  #[property(get(type = "copy"), set(disable))]
  body: Stmt,
}
impl_stmt!(
  ForStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ForStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.declarations.to_string_tree_internal(&mut ni, result);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.computation.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl ForStatement {
  pub fn new(region: &mut Region, declarations: Ast, condition: Expr, computation: Expr, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(
      region,
      ForStatement {
        declarations,
        condition,
        computation,
        body,
      },
    );
  }
}

#[derive(Property)]
pub struct ForInStatement {
  #[property(get(type = "copy"), set(disable))]
  lhs: Ast,

  #[property(get(type = "copy"), set(disable))]
  rhs: Expr,

  #[property(get(type = "copy"), set(disable))]
  body: Stmt,
}
impl_stmt!(
  ForInStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ForInStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.lhs.to_string_tree_internal(&mut ni, result);
    self.rhs.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl ForInStatement {
  pub fn new(region: &mut Region, lhs: Ast, rhs: Expr, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, ForInStatement { lhs, rhs, body });
  }
}

#[derive(Property)]
pub struct ForOfStatement {
  #[property(get(type = "copy"), set(disable))]
  lhs: Ast,

  #[property(get(type = "copy"), set(disable))]
  rhs: Expr,

  #[property(get(type = "copy"), set(disable))]
  is_await: bool,

  #[property(get(type = "copy"), set(disable))]
  body: Stmt,
}
impl_stmt!(
  ForOfStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[ForOfStatement{} {}]\n",
      indent,
      if self.is_await { " await" } else { "" },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.lhs.to_string_tree_internal(&mut ni, result);
    self.rhs.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl ForOfStatement {
  pub fn new(region: &mut Region, is_await: bool, lhs: Ast, rhs: Expr, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, ForOfStatement { lhs, rhs, is_await, body });
  }
}

pub struct WhileStatement {
  condition: Expr,
  body: Stmt,
}
impl_stmt!(
  WhileStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[WhileStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl WhileStatement {
  pub fn new(region: &mut Region, condition: Expr, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, WhileStatement { condition, body });
  }
}

pub struct DoWhileStatement {
  condition: Expr,
  body: Stmt,
}
impl_stmt!(
  DoWhileStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[DoWhileStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl DoWhileStatement {
  pub fn new(region: &mut Region, condition: Expr, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, DoWhileStatement { condition, body });
  }
}

#[derive(Property)]
pub struct IfStatement {
  #[property(get(type = "copy"), set(disable))]
  condition: Expr,

  #[property(get(type = "copy"), set(disable))]
  then_stmt: Stmt,

  #[property(get(type = "copy"), set(disable))]
  else_stmt: Option<Stmt>,
}
impl_stmt!(
  IfStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[IfStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.then_stmt.to_string_tree_internal(&mut ni, result);
    if let Some(es) = self.else_stmt {
      es.to_string_tree_internal(&mut ni, result);
    }
  }
);
impl IfStatement {
  pub fn new(region: &mut Region, condition: Expr, then_stmt: Stmt, else_stmt: Option<Stmt>) -> Node<Self> {
    return Node::<Self>::new(
      region,
      IfStatement {
        condition,
        then_stmt,
        else_stmt,
      },
    );
  }
}

#[derive(Property)]
pub struct SwitchStatement {
  #[property(get(type = "copy"), set(disable))]
  scope: Exotic<Scope>,

  #[property(get(type = "copy"), set(disable))]
  condition: Expr,

  #[property(get(type = "ref"), set(disable))]
  cases: Vec<Stmt>,
}
impl_stmt!(
  SwitchStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[SwitchStatement {:?} {}]\n", indent, *self.scope, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    to_string_list(&self.cases, indent, result);
  }
);
impl SwitchStatement {
  pub fn new(region: &mut Region, scope: Exotic<Scope>, condition: Expr, cases: Vec<Stmt>) -> Node<Self> {
    return Node::<Self>::new(region, SwitchStatement { scope, condition, cases });
  }
}

pub struct SwitchCase {
  condition: Option<Expr>,
  body: Stmt,
}
impl_stmt!(
  SwitchCase,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[SwitchCase{} {}]\n",
      indent,
      if !self.condition.is_some() { " default" } else { "" },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(cond) = self.condition {
      cond.to_string_tree_internal(&mut ni, result);
    }
    self.body.to_string_tree_internal(&mut ni, result);
  }
);
impl SwitchCase {
  pub fn new(region: &mut Region, condition: Option<Expr>, body: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, SwitchCase { condition, body });
  }
}

pub struct BreakStatement {
  label_name: FixedU16CodePointArray,
}
impl_stmt!(
  BreakStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[BreakStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

pub struct ContinueStatement {
  label_name: FixedU16CodePointArray,
}
impl_stmt!(
  ContinueStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[ContinueStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

#[derive(Property)]
pub struct LabelledStatement {
  #[property(get(type = "copy"), set(disable))]
  identifier: Expr,
  #[property(get(type = "copy"), set(disable))]
  stmt: Stmt,
}
impl_stmt!(
  LabelledStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[LabelledStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.identifier.to_string_tree_internal(&mut ni, result);
    self.stmt.to_string_tree_internal(&mut ni, result);
  }
);

impl LabelledStatement {
  pub fn new(region: &mut Region, identifier: Expr, stmt: Stmt) -> Node<Self> {
    return Node::<Self>::new(region, LabelledStatement { identifier, stmt });
  }
}

pub struct WithStatement {
  expr: Expr,
  body: Stmt,
}
impl_stmt!(
  WithStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[BlockStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.expr.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

#[derive(Property)]
pub struct VariableDeclarations {
  #[property(get(type = "ref"), set(disable))]
  decls: Vec<Stmt>,
}
impl_stmt!(
  VariableDeclarations,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[VariableDeclarations {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.decls, indent, result);
  }
);
impl VariableDeclarations {
  pub fn new(region: &mut Region, decls: Vec<Stmt>) -> Node<Self> {
    return Node::<VariableDeclarations>::new(region, VariableDeclarations { decls });
  }
}

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum VariableDeclarationType {
  Let,
  Const,
  Var,
}
#[derive(Property)]
pub struct VariableDeclaration {
  #[property(get(type = "copy"), set(disable))]
  decl_type: VariableDeclarationType,
  #[property(get(type = "copy"), set(disable))]
  binding: Expr,
  #[property(get(type = "copy"), set(disable))]
  initializer: Option<Expr>,
}
impl_stmt!(
  VariableDeclaration,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!(
      "{}[VariableDeclaration type = {:?} {}]\n",
      indent,
      self.decl_type,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.binding.to_string_tree_internal(&mut ni, result);
    if let Some(ref init) = self.initializer {
      init.to_string_tree_internal(&mut ni, result);
    }
  }
);
impl VariableDeclaration {
  pub fn new(region: &mut Region, decl_type: VariableDeclarationType, binding: Expr, initializer: Option<Expr>) -> Node<Self> {
    return Node::<VariableDeclaration>::new(
      region,
      VariableDeclaration {
        decl_type,
        binding,
        initializer,
      },
    );
  }
}

bitflags! {
  pub struct SkipExprType: u32 {
    const EXPR = 0;
    const IDENTIFIER = 0x1;
    const LITERAL = 0x2;
    const BINARY_EXPR = 0x4;
    const CALL = 0x8;
    const NEW_RECEIVER_CALL = 0x10;
    const SUPER_RECEIVER_CALL = 0x20;
    const ARRAY = 0x40;
    const OBJECT = 0x80;
    const EXPRS = 0x100;
    const TEMPLATE = 0x200;
    const SPREAD = 0x400;
    const STRING_LITERAL = 0x800;
    const IMPORT_META = 0x1000;
    const CLASS = 0x2000;
    const INITIALIZER = 0x4000;
  }
}
pub struct SkipExpr(SkipExprType);
impl SkipExpr {
  pub fn new(region: &mut Region, skip_expr_type: SkipExprType) -> Node<Self> {
    return Node::<Self>::new(region, SkipExpr(skip_expr_type));
  }

  pub fn is_exprs(&self) -> bool {
    return self.0.contains(SkipExprType::EXPRS);
  }

  pub fn is_binary_expr(&self) -> bool {
    return self.0.contains(SkipExprType::BINARY_EXPR);
  }

  pub fn is_identifier(&self) -> bool {
    return self.0.contains(SkipExprType::IDENTIFIER);
  }

  pub fn is_literal(&self) -> bool {
    return self.0.contains(SkipExprType::LITERAL);
  }

  pub fn is_string_literal(&self) -> bool {
    return self.0.contains(SkipExprType::STRING_LITERAL);
  }

  pub fn is_object(&self) -> bool {
    return self.0.contains(SkipExprType::OBJECT);
  }

  pub fn is_array(&self) -> bool {
    return self.0.contains(SkipExprType::OBJECT);
  }

  pub fn is_call(&self) -> bool {
    return self.0.contains(SkipExprType::CALL);
  }

  pub fn is_new_receiver_call(&self) -> bool {
    return self.0.contains(SkipExprType::NEW_RECEIVER_CALL);
  }

  pub fn is_super_receiver_call(&self) -> bool {
    return self.0.contains(SkipExprType::SUPER_RECEIVER_CALL);
  }

  pub fn is_template(&self) -> bool {
    return self.0.contains(SkipExprType::TEMPLATE);
  }

  pub fn is_spread(&self) -> bool {
    return self.0.contains(SkipExprType::SPREAD);
  }

  pub fn is_initializer(&self) -> bool {
    return self.0.contains(SkipExprType::INITIALIZER);
  }
}
impl_expr!(
  SkipExpr,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[SkipExpr {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

bitflags! {
  pub struct SkipStmtType: u8 {
    const STMT = 0x1;
    const CLASS_FIELD = 0x2;
    const VARS = 0x4;
    const VAR = 0x8;
    const VAR_WITHOUT_INITIALIZER = 0x10;
    const LABEL = 0x20;
    const LABELLED_FUNCTION = 0x40;
  }
}

pub struct SkipStmt {
  flag: SkipStmtType,
}
impl SkipStmt {
  pub fn new(region: &mut Region, flag: SkipStmtType) -> Node<Self> {
    return Node::<Self>::new(region, SkipStmt { flag });
  }

  pub fn is_class_field(&self) -> bool {
    return self.flag == SkipStmtType::CLASS_FIELD;
  }

  pub fn is_vars(&self) -> bool {
    return self.flag == SkipStmtType::VARS;
  }

  pub fn is_var(&self) -> bool {
    return self.flag.contains(SkipStmtType::VAR);
  }

  pub fn is_var_without_init(&self) -> bool {
    return self.flag.contains(SkipStmtType::VAR_WITHOUT_INITIALIZER);
  }

  pub fn is_label(&self) -> bool {
    return self.flag == SkipStmtType::LABEL;
  }

  pub fn is_labelled_fn(&self) -> bool {
    return self.flag == SkipStmtType::LABELLED_FUNCTION;
  }
}
impl_stmt!(
  SkipStmt,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[SkipStmt {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

bitflags! {
  pub struct SkipAnyType: u8 {
    const CLASS = 1;
    const FUNCTION = 2;
  }
}

pub struct SkipAny {
  flag: SkipAnyType,
}
impl SkipAny {
  pub fn new(region: &mut Region, flag: SkipAnyType) -> Node<Self> {
    return Node::<Self>::new(region, SkipAny { flag });
  }

  pub fn is_function(&self) -> bool {
    return self.flag == SkipAnyType::FUNCTION;
  }

  pub fn is_class(&self) -> bool {
    return self.flag == SkipAnyType::CLASS;
  }
}
impl_both!(
  SkipAny,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    let str = format!("{}[SkipAny {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &RuntimeSourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

#[cfg(test)]
mod ast_test {
  use super::*;

  #[test]
  fn expressions_test() {
    let mut region = Region::new();
    let mut expressions = Expressions::new(&mut region, Vec::new());
    let elision = Elision::new(&mut region);
    let elision2 = Elision::new(&mut region);
    expressions.push(elision.into());
    expressions.push(elision2.into());
    match compare_node(
      "Expressions",
      &expressions.to_string_tree(),
      indoc! {"
        [Expressions [0, 0]]
          [Elision [0, 0]]
          [Elision [0, 0]]
      "},
    ) {
      Err(em) => {
        println!("{}", em);
      }
      _ => {}
    }
  }
}
