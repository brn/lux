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

  fn to_string(&self, indent: &mut String, result: &mut String, source_positon: &SourcePosition);

  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_positon: &SourcePosition);
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

pub trait AstStringify {
  fn to_string(&self) -> String;

  fn to_string_tree(&self) -> String;
}

trait AstStringifyInternal {
  fn to_string_internal(&self, indent: &mut String, result: &mut String);

  fn to_string_tree_internal(&self, indent: &mut String, result: &mut String);
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
}

macro_rules! _ast_enum {
  ($name:ident { $($item:ident(Node<$type:ty>),)* }) => {
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
    StructuralLiteral(Node<StructuralLiteral>),
    UnaryExpression(Node<UnaryExpression>),
    ImportSpecifier(Node<ImportSpecifier>),
    ImportBinding(Node<ImportBinding>),
    NamedImportList(Node<NamedImportList>),
    NewExpression(Node<NewExpression>),
    TemplateLiteral(Node<TemplateLiteral>),
    ClassExpression(Node<ClassExpression>),
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
    ForStatement(Node<ForStatement>),
    WhileStatement(Node<WhileStatement>),
    DoWhileStatement(Node<DoWhileStatement>),
    IfStatement(Node<IfStatement>),
    SwitchStatement(Node<SwitchStatement>),
    SwitchCases(Node<SwitchCases>),
    BreakStatement(Node<BreakStatement>),
    ContinueStatement(Node<ContinueStatement>),
    BlockStatement(Node<BlockStatement>),
    LabelledStatement(Node<LabelledStatement>),
    WithStatement(Node<WithStatement>),
    VariableDeclaration(Node<VariableDeclaration>),
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
impl AstStringifyInternal for Ast {
  fn to_string_internal(&self, indent: &mut String, result: &mut String) {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_internal(indent, result),
      &Ast::Stmt(ref stmt) => stmt.to_string_internal(indent, result),
    };
  }

  fn to_string_tree_internal(&self, indent: &mut String, result: &mut String) {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_tree_internal(indent, result),
      &Ast::Stmt(ref stmt) => stmt.to_string_tree_internal(indent, result),
    };
  }
}

impl AstStringify for Ast {
  fn to_string(&self) -> String {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string(),
      &Ast::Stmt(ref stmt) => stmt.to_string(),
    };
  }

  fn to_string_tree(&self) -> String {
    return match self {
      &Ast::Expr(ref expr) => expr.to_string_tree(),
      &Ast::Stmt(ref stmt) => stmt.to_string_tree(),
    };
  }
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
    self
      .kind
      .to_string_tree(&mut indent, &mut result, self.source_position());
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

fn to_string_list<T: AstStringifyInternal>(list: &Vec<T>, indent: &mut String, result: &mut String) {
  let mut ni = format!("  {}", indent);
  for n in list {
    n.to_string_tree_internal(&mut ni, result);
  }
}

pub struct Expressions {
  items: Vec<Expr>,
}
impl_expr!(
  Expressions,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[Expressions {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    to_string_list(&self.items, indent, result);
  }
);

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

  pub fn push(&mut self, expr: Expr) {
    self.items.push(expr);
  }

  pub fn iter(&self) -> std::slice::Iter<Expr> {
    return self.items.iter();
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
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[Statements {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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
  pub fn new(region: &mut Region) -> Node<Statements> {
    return Node::<Statements>::new(
      region,
      Statements {
        items: Vec::<Stmt>::new(),
      },
    );
  }

  pub fn push(&mut self, expr: Stmt) {
    self.items.push(expr);
  }

  pub fn iter(&self) -> std::slice::Iter<Stmt> {
    return self.items.iter();
  }
}

#[derive(Property)]
pub struct Statement {
  #[property(get(type = "ref"))]
  expr: Expr,
}
impl_stmt!(
  Statement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[Statement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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

  #[property(get(type = "ref"), set(type = "own"))]
  lhs: Expr,

  #[property(get(type = "ref"), set(type = "own"))]
  rhs: Expr,
}
impl_expr!(
  BinaryExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[BinaryExpression operand = {} {}]\n",
      indent,
      self.op,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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

  #[property(get(type = "copy"), set(type = "own"))]
  op: Token,

  #[property(get(type = "ref"), set(type = "own"))]
  target: Expr,
}
impl_expr!(
  UnaryExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[UnaryExpression operand = {} position = {:?} {}]\n",
      indent,
      self.op,
      self.position,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.target.to_string_tree_internal(&mut ni, result);
  }
);

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
impl_expr!(
  ConditionalExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ConditionalExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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
  #[property(get(type = "ref"), set(type = "own"))]
  callee: Expr,
}
impl_expr!(
  NewExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ConditionalExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.callee.to_string_tree_internal(&mut ni, result);
  }
);

impl NewExpression {
  #[inline]
  pub fn new(region: &mut Region, callee: Expr) -> Node<NewExpression> {
    return Node::<NewExpression>::new(region, NewExpression { callee });
  }
}

#[derive(Debug)]
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
impl_expr!(
  CallExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "[CallExpression receiver = {:?} {}]\n",
      self.receiver,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.callee.to_string_tree_internal(&mut ni, result);
    if let Some(ref node) = self.parameters {
      node.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl CallExpression {
  #[inline]
  pub fn new(
    region: &mut Region,
    callee: Expr,
    receiver: CallReceiverType,
    parameters: Option<Node<Expressions>>,
  ) -> Node<CallExpression> {
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

pub enum PropertyAccessType {
  Dot = 1,
  Element,
}

pub struct PropertyAccessExpression {
  flags: Bitset<u8>,
  receiver: Option<Expr>,
  property: Option<Expr>,
}
impl_expr!(
  PropertyAccessExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[PropertyAccessExpression property_access = {} {}]\n",
      indent,
      if self.is_dot_access() {
        "dot"
      } else if self.is_element_access() {
        "element"
      } else if self.is_meta_property() {
        "meta"
      } else {
        "super"
      },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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
    callee: Expr,
    receiver: Option<Expr>,
    property: Option<Expr>,
  ) -> Node<PropertyAccessExpression> {
    return Node::<PropertyAccessExpression>::new(
      region,
      PropertyAccessExpression {
        flags: Bitset::<u8>::new(),
        receiver,
        property,
      },
    );
  }

  #[inline(always)]
  pub fn is_dot_access(&self) -> bool {
    return self.flags.get(PropertyAccessType::Dot as usize);
  }

  #[inline(always)]
  pub fn is_element_access(&self) -> bool {
    return self.flags.get(PropertyAccessType::Element as usize);
  }

  #[inline(always)]
  pub fn is_meta_property(&self) -> bool {
    return self.flags.get(CallReceiverType::New as usize);
  }

  #[inline(always)]
  pub fn is_super_property(&self) -> bool {
    return self.flags.get(CallReceiverType::Super as usize);
  }
}

#[derive(PartialEq)]
pub enum FunctionType {
  Scoped,
  NonScoped,
  Generator,
}

#[derive(Property)]
pub struct FunctionExpression {
  name: Option<Node<Literal>>,
  function_type: FunctionType,
  formal_parameters: Node<Expressions>,

  #[property(get(type = "copy"))]
  source_start_index: u32,

  #[property(get(type = "copy"))]
  source_end_index: u32,
}
impl_expr!(
  FunctionExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[FunctionExpression type = {} start = {} end = {} {}]\n",
      indent,
      if self.is_arrow_function() {
        "ArrowFunction"
      } else if self.is_generator_function() {
        "Generator"
      } else {
        "Function"
      },
      self.source_start_index,
      self.source_end_index,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref name) = self.name {
      name.to_string_tree_internal(&mut ni, result);
    }
    self.formal_parameters.to_string_tree_internal(&mut ni, result);
  }
);

impl FunctionExpression {
  pub fn new(
    region: &mut Region,
    name: Option<Node<Literal>>,
    function_type: FunctionType,
    formal_parameters: Node<Expressions>,
    source_start_index: u32,
    source_end_index: u32,
  ) -> Node<FunctionExpression> {
    return Node::new(
      region,
      FunctionExpression {
        name,
        function_type,
        formal_parameters,
        source_start_index,
        source_end_index,
      },
    );
  }

  #[inline(always)]
  pub fn is_arrow_function(&self) -> bool {
    return self.function_type == FunctionType::NonScoped;
  }

  #[inline(always)]
  pub fn is_generator_function(&self) -> bool {
    return self.function_type == FunctionType::Generator;
  }
}

#[derive(Property)]
pub struct ObjectPropertyExpression {
  #[property(get(type = "ref"))]
  key: Expr,

  #[property(get(type = "ref"))]
  value: Expr,
  #[property(skip)]
  init: Option<Expr>,
}
impl_expr!(
  ObjectPropertyExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ObjectPropertyExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.key.to_string_tree_internal(&mut ni, result);
    self.value.to_string_tree_internal(&mut ni, result);
    if let Some(ref init) = self.init {
      init.to_string_tree_internal(&mut ni, result);
    }
  }
);

impl ObjectPropertyExpression {
  pub fn new(region: &mut Region, key: Expr, value: Expr, init: Option<Expr>) -> Node<ObjectPropertyExpression> {
    return Node::new(region, ObjectPropertyExpression { key, value, init });
  }

  #[inline(always)]
  pub fn init(&self) -> Option<&Expr> {
    return self.init.as_ref();
  }
}

pub struct Elision;
impl_expr!(
  Elision,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[Elision {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl Elision {
  pub fn new(region: &mut Region) -> Node<Elision> {
    return Node::new(region, Elision {});
  }
}

pub enum StructuralLiteralType {
  Array = 0x1,
  Object = 0x2,
}

pub enum StructuralLiteralTrait {
  HasAccessor = 0x4,
  HasGenerator = 0x8,
  HasSpread = 0x10,
}

pub struct StructuralLiteral {
  flag: Bitset<u8>,
  properties: Node<Expressions>,
}
impl_expr!(
  StructuralLiteral,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[StructualLiteral type = {}{} {}]\n",
      indent,
      if self.is_array_literal() {
        "ArrayLiteral"
      } else {
        "ObjectLiteral"
      },
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
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.properties.to_string_tree_internal(&mut ni, result);
  }
);

impl StructuralLiteral {
  #[inline]
  pub fn new(region: &mut Region, properties: Node<Expressions>) -> Node<StructuralLiteral> {
    return Node::<StructuralLiteral>::new(
      region,
      StructuralLiteral {
        flag: Bitset::<u8>::new(),
        properties,
      },
    );
  }

  #[inline(always)]
  pub fn is_array_literal(&self) -> bool {
    return self.flag.get(StructuralLiteralType::Array as usize);
  }

  #[inline(always)]
  pub fn is_object_literal(&self) -> bool {
    return self.flag.get(StructuralLiteralType::Object as usize);
  }

  #[inline(always)]
  pub fn set_accessor(&mut self) {
    return self.flag.set(StructuralLiteralTrait::HasAccessor as usize);
  }

  #[inline(always)]
  pub fn has_accessor(&self) -> bool {
    return self.flag.get(StructuralLiteralTrait::HasAccessor as usize);
  }

  #[inline(always)]
  pub fn set_generator(&mut self) {
    return self.flag.set(StructuralLiteralTrait::HasGenerator as usize);
  }

  #[inline(always)]
  pub fn has_generator(&self) -> bool {
    return self.flag.get(StructuralLiteralTrait::HasGenerator as usize);
  }

  #[inline(always)]
  pub fn set_spread(&mut self) {
    return self.flag.set(StructuralLiteralTrait::HasSpread as usize);
  }

  #[inline(always)]
  pub fn has_spread(&self) -> bool {
    return self.flag.get(StructuralLiteralTrait::HasSpread as usize);
  }
}

#[derive(Property)]
pub struct Literal {
  #[property(get(type = "copy"))]
  literal_type: Token,

  #[property(get(type = "copy"))]
  value: FixedU16CodePointArray,
}
impl_expr!(
  Literal,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[Literal type = {:?} value = {} {}]\n",
      indent,
      self.literal_type,
      self.value.to_utf8(),
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

impl Literal {
  pub fn new(region: &mut Region, literal_type: Token, value: FixedU16CodePointArray) -> Node<Literal> {
    return Node::new(region, Literal { literal_type, value });
  }
}

pub struct TemplateLiteral {
  parts: Node<Expressions>,
}
impl_expr!(
  TemplateLiteral,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[TemplateLiteral {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.parts.to_string_tree_internal(&mut ni, result);
  }
);
impl TemplateLiteral {
  pub fn new(region: &mut Region, parts: Node<Expressions>) -> Node<TemplateLiteral> {
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
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[ImportSpecifier is_namespace = {} {}]\n",
      indent,
      self.is_namespace,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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
  pub fn new(
    region: &mut Region,
    is_namespace: bool,
    name: Option<Expr>,
    as_expr: Option<Expr>,
  ) -> Node<ImportSpecifier> {
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
  list: Node<Expressions>,
}
impl_expr!(
  NamedImportList,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[NamedImportList {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.list.to_string_tree_internal(&mut ni, result);
  }
);
impl NamedImportList {
  pub fn new(region: &mut Region, list: Node<Expressions>) -> Node<NamedImportList> {
    return Node::new(region, NamedImportList { list });
  }
}

pub struct ImportBinding {
  default_binding: Option<Expr>,
  namespace_import: Option<Expr>,
  named_import_list: Option<Expr>,
}
impl_expr!(
  ImportBinding,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ImportBinding {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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

  #[inline(always)]
  pub fn default_binding(&self) -> Option<&Expr> {
    return self.default_binding.as_ref();
  }

  #[inline(always)]
  pub fn namespace_import(&self) -> Option<&Expr> {
    return self.namespace_import.as_ref();
  }

  #[inline(always)]
  pub fn named_import_list(&self) -> Option<&Expr> {
    return self.named_import_list.as_ref();
  }
}

#[derive(Property)]
pub struct ImportDeclaration {
  #[property(skip)]
  import_binding: Option<Expr>,

  #[property(get(type = "ref"))]
  module_specifier: Expr,
}
impl_stmt!(
  ImportDeclaration,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ImportDeclaration {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[ExportDeclaration type = {} {}]\n",
      indent,
      if self.is_namespace_export() {
        "namespace"
      } else {
        "default"
      },
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
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

pub struct ClassExpression {
  name: Node<Literal>,
  heritage: Option<Expr>,
  static_fields: Node<Expressions>,
  instance_fields: Node<Expressions>,
}
impl_expr!(
  ClassExpression,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ClassExpression {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    if let Some(ref heritage) = self.heritage {
      heritage.to_string_tree_internal(&mut ni, result);
    }
    self.static_fields.to_string_tree_internal(&mut ni, result);
    self.instance_fields.to_string_tree_internal(&mut ni, result);
  }
);

pub struct ForStatement {
  declarations: Node<Statements>,
  comparison: Expr,
  computation: Expr,
}
impl_stmt!(
  ForStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ForStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.declarations.to_string_tree_internal(&mut ni, result);
    self.comparison.to_string_tree_internal(&mut ni, result);
    self.computation.to_string_tree_internal(&mut ni, result);
  }
);

pub struct WhileStatement {
  comparison: Expr,
  body: Node<Statements>,
}
impl_stmt!(
  WhileStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ForStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.comparison.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

pub struct DoWhileStatement {
  comparison: Expr,
  body: Node<Statements>,
}
impl_stmt!(
  DoWhileStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ForStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.comparison.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

pub struct IfStatement {
  condition: Expr,
  then_body: Node<Statements>,
  else_body: Node<Statement>,
}
impl_stmt!(
  IfStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[IfStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.then_body.to_string_tree_internal(&mut ni, result);
    self.else_body.to_string_tree_internal(&mut ni, result);
  }
);

pub struct SwitchStatement {
  condition: Expr,
  cases: Vec<Node<SwitchCases>>,
}
impl_stmt!(
  SwitchStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[SwitchStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    to_string_list(&self.cases, indent, result);
  }
);

pub struct SwitchCases {
  condition: Expr,
  body: Node<Statements>,
}
impl_stmt!(
  SwitchCases,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[SwitchCases {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.condition.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

pub struct BreakStatement;
impl_stmt!(
  BreakStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[BreakStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

pub struct ContinueStatement;
impl_stmt!(
  ContinueStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[ContinueStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
  }
);

pub struct BlockStatement {
  body: Node<Statements>,
}
impl_stmt!(
  BlockStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[BlockStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

pub struct LabelledStatement {
  stmt: Node<Statement>,
}
impl_stmt!(
  LabelledStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[BlockStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.stmt.to_string_tree_internal(&mut ni, result);
  }
);

pub struct WithStatement {
  expr: Expr,
  body: Node<Statement>,
}
impl_stmt!(
  WithStatement,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!("{}[BlockStatement {}]\n", indent, source_position.to_string());
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.expr.to_string_tree_internal(&mut ni, result);
    self.body.to_string_tree_internal(&mut ni, result);
  }
);

#[derive(PartialEq, Debug)]
pub enum VariableDeclarationType {
  Let,
  Const,
  Var,
}
pub struct VariableDeclaration {
  decl_type: VariableDeclarationType,
  binding: Expr,
  initializer: Option<Expr>,
}
impl_stmt!(
  VariableDeclaration,
  fn to_string(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    let str = format!(
      "{}[VariableDeclaration type = {:?} {}]\n",
      indent,
      self.decl_type,
      source_position.to_string()
    );
    result.push_str(&str);
  },
  fn to_string_tree(&self, indent: &mut String, result: &mut String, source_position: &SourcePosition) {
    self.to_string(indent, result, source_position);
    let mut ni = format!("  {}", indent);
    self.binding.to_string_tree_internal(&mut ni, result);
    if let Some(ref init) = self.initializer {
      init.to_string_tree_internal(&mut ni, result);
    }
  }
);

#[cfg(test)]
mod ast_test {
  use super::*;

  #[test]
  fn expressions_test() {
    let mut region = Region::new();
    let mut expressions = Expressions::new(&mut region);
    let elision = Elision::new(&mut region);
    let elision2 = Elision::new(&mut region);
    expressions.push(elision.into());
    expressions.push(elision2.into());
    compare_node(
      "Expressions",
      &expressions.to_string_tree(),
      indoc! {"
        [Expressions [0, 0, 0, 0]]
          [Elision [0, 0, 0, 0]]
          [Elision [0, 0, 0, 0]]
      "},
    );
  }
}
