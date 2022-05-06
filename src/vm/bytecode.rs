use super::bytecode_type::*;
use crate::context::ObjectRecordsInitializedContext;
use crate::structs::{HeapLayout, HeapObject, InternalArray, Repr};
use crate::utility::*;
use enum_index::{EnumIndex, IndexEnum};
use property::Property;

#[derive(Property)]
pub struct BytecodeNode {
  #[property(get(type = "copy"), set(disable))]
  bc: BytecodeRepr,

  #[property(get(type = "copy"), set(disable))]
  prev: Option<Exotic<BytecodeNode>>,

  #[property(get(type = "copy"), set(disable))]
  next: Option<Exotic<BytecodeNode>>,
}

impl BytecodeNode {
  pub fn new(mut region: WeakRegion, bc: BytecodeRepr) -> Exotic<Self> {
    region.alloc(BytecodeNode {
      bc,
      prev: None,
      next: None,
    })
  }

  pub fn new_next(&mut self, mut region: WeakRegion, bc: BytecodeRepr) -> Exotic<Self> {
    let next = region.alloc(BytecodeNode {
      bc,
      prev: Some(Exotic::from_self(self)),
      next: None,
    });
    self.next = Some(next);
    return next;
  }
}

#[repr(C)]
#[derive(Copy, Clone, Default)]
pub struct BytecodeVectorLayout {
  bytecode_array: InternalArray<Bytecode>,
}
#[repr(C)]
pub struct BytecodeVector(HeapLayout<BytecodeVectorLayout>);
impl_object!(BytecodeVector, HeapLayout<BytecodeVectorLayout>);

impl BytecodeVector {
  pub const SIZE: u32 = std::mem::size_of::<BytecodeVectorLayout>() as u32;
  pub fn new(context: impl ObjectRecordsInitializedContext, bytecode_list: BytecodeList) -> Self {
    let mut bv = BytecodeVector(HeapLayout::<BytecodeVectorLayout>::new(
      context,
      context.object_records().bytecode_vector_record(),
    ));
    bv.bytecode_array = InternalArray::new(context, bytecode_list.size());
    for bytecode_node in bytecode_list.into_iter() {
      bytecode_node.bc().encode(bv.bytecode_array);
    }
    return bv;
  }
}

impl std::fmt::Debug for BytecodeVector {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
    let mut v = Vec::new();
    for bc in self.bytecode_array.into_iter() {
      v.push(bc);
    }
    write!(f, "{:?}", v)
  }
}

#[derive(Property)]
pub struct BytecodeList {
  region: Region,

  #[property(get(type = "copy"), set(disable))]
  root: Option<Exotic<BytecodeNode>>,

  #[property(get(type = "copy"), set(disable))]
  current: Option<Exotic<BytecodeNode>>,

  #[property(get(type = "copy"), set(disable))]
  size: usize,
}

impl BytecodeList {
  pub fn new() -> Self {
    BytecodeList {
      region: Region::new(),
      root: None,
      current: None,
      size: 0,
    }
  }

  pub fn add_bytecode(&mut self, bc: BytecodeRepr) {
    self.size += (bc.size() + std::mem::size_of::<Bytecode>());
    if let Some(mut current) = self.current {
      self.current = Some(current.new_next(self.region.clone_weak(), bc));
    } else {
      self.root = Some(BytecodeNode::new(self.region.clone_weak(), bc));
      self.current = self.root.clone();
    }
  }
}

#[derive(Clone)]
pub struct BytecodeListIter {
  current_node: Option<Exotic<BytecodeNode>>,
}

impl Iterator for BytecodeListIter {
  type Item = Exotic<BytecodeNode>;
  fn next(&mut self) -> Option<Self::Item> {
    if let Some(next) = self.current_node {
      self.current_node = next.next();
      return Some(next);
    }
    return None;
  }
}

impl IntoIterator for BytecodeList {
  type Item = Exotic<BytecodeNode>;
  type IntoIter = BytecodeListIter;

  fn into_iter(self) -> Self::IntoIter {
    return BytecodeListIter { current_node: self.root };
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::context::LuxContext;

  #[test]
  fn bytecode_list_test() {
    let mut list = BytecodeList::new();
    list.add_bytecode(I32Add::new(0));
    list.add_bytecode(I32Sub::new(10));
    list.add_bytecode(I32Mul::new(12));
    list.add_bytecode(I32Div::new(14));
    let expectations = [I32Add::new(0), I32Sub::new(10), I32Mul::new(12), I32Div::new(14)];
    for (i, b) in list.into_iter().enumerate() {
      assert_eq!(b.bc(), expectations[i]);
    }
  }

  #[test]
  fn bytecode_vector_test() {
    let mut list = BytecodeList::new();
    list.add_bytecode(I32Add::new(0));
    list.add_bytecode(I32Sub::new(10));
    list.add_bytecode(I32Mul::new(12));
    list.add_bytecode(I32Div::new(14));
    let context = LuxContext::new_until_internal_object_records();
    let mut vector = BytecodeVector::new(context, list);
    assert_eq!(format!("{:?}", vector), "[0, 0, 0, 1, 10, 0, 2, 12, 0, 3, 14, 0]");
  }
}
