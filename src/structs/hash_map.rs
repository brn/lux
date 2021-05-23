use super::cell::*;
use super::internal_array::*;
use super::repr::Repr;
use super::shape::Shape;
use crate::context::{AllocationOnlyContext, Context};
use crate::utility::Len;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::mem::size_of;

pub trait ContextHash {
  fn context_hash<R: Hasher>(&self, context: &mut impl Context, state: &mut R) -> u64;
}

pub trait ContextHashMut {
  fn context_hash_mut<R: Hasher>(&mut self, context: &mut impl Context, state: &mut R) -> u64;
}

pub trait HashMapKey: Copy + PartialEq + ContextHash + ContextHashMut {}
impl<T: Copy + PartialEq + ContextHash + ContextHashMut> HashMapKey for T {}

#[repr(C)]
#[derive(Copy, Clone)]
struct HashMapEntryLayout<K: HashMapKey, V: Copy> {
  key: K,
  value: V,
  hash: u64,
  position: usize,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
struct HashMapEntry<K: HashMapKey, V: Copy>(HeapLayout<HashMapEntryLayout<K, V>>);
impl_object!(HashMapEntry<K: HashMapKey, V: Copy>, HeapLayout<HashMapEntryLayout<K, V>>);

impl<K: HashMapKey, V: Copy> HashMapEntry<K, V> {
  const SIZE: usize = Cell::SIZE + size_of::<HashMapEntryLayout<K, V>>();
  pub fn new(
    context: &mut impl AllocationOnlyContext,
    key: K,
    value: V,
    hash: u64,
    position: usize,
  ) -> HashMapEntry<K, V> {
    let mut layout =
      HeapLayout::<HashMapEntryLayout<K, V>>::new(context, HashMapEntry::<K, V>::SIZE, Shape::hash_map_entry());
    layout.key = key;
    layout.value = value;
    layout.hash = hash;
    layout.position = position;
    return HashMapEntry(layout);
  }

  pub fn is_null(&self) -> bool {
    return self.0.is_null();
  }
}

#[repr(C)]
#[derive(Copy, Clone)]
pub struct HashMapLayout<K: HashMapKey, V: Copy> {
  storage: BareHeapLayout<InternalArray<HashMapEntry<K, V>>>,
}

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct HashMap<K: HashMapKey, V: Copy>(HeapLayout<HashMapLayout<K, V>>);
impl_object!(HashMap<K: HashMapKey, V: Copy>, HeapLayout<HashMapLayout<K, V>>);

#[inline(always)]
fn udiff(a: usize, b: usize) -> usize {
  if a > b {
    a - b
  } else {
    b - a
  }
}

impl<K: HashMapKey, V: Copy> HashMap<K, V> {
  const SIZE: usize = Cell::SIZE + size_of::<HashMap<K, V>>();
  const DEFAULT_CAPACITY: usize = 24;

  #[inline]
  pub fn new(context: &mut impl AllocationOnlyContext) -> HashMap<K, V> {
    let mut layout = HeapLayout::<HashMapLayout<K, V>>::new(context, HashMap::<K, V>::SIZE, Shape::hash_map());
    layout.storage.set(InternalArray::<HashMapEntry<K, V>>::new(
      context,
      HashMap::<K, V>::DEFAULT_CAPACITY,
    ));
    layout
      .storage
      .handle()
      .fill(HashMapEntry::<K, V>(HeapLayout::<HashMapEntryLayout<K, V>>::null()));
    return HashMap(layout);
  }

  #[inline]
  pub fn insert(&mut self, context: &mut impl Context, key: K, value: V) {
    let hash = self.hash_mut(context, key);
    if self.load() > 0.8 {
      self.grow(context);
    }
    self.insert_to(context, key, value, hash);
  }

  #[inline]
  pub fn find(&self, context: &mut impl Context, key: K) -> Option<V> {
    let storage = self.storage.handle();
    match self.find_index(context, key) {
      Some(index) => {
        return Some(storage[index].value);
      }
      _ => return None,
    }
  }

  #[inline]
  pub fn delete(&self, context: &mut impl Context, key: K) -> bool {
    let mut storage = self.storage.handle();
    match self.find_index(context, key) {
      Some(index) => {
        storage.write(
          index,
          HashMapEntry::<K, V>(HeapLayout::<HashMapEntryLayout<K, V>>::null()),
        );
        storage.set_length(storage.length() - 1);
        return true;
      }
      _ => return false,
    }
  }

  #[inline(always)]
  pub fn len(&self) -> usize {
    return self.storage.handle().length();
  }

  #[inline(always)]
  fn hash(&self, context: &mut impl Context, key: K) -> u64 {
    let mut h = DefaultHasher::new();
    return key.context_hash(context, &mut h);
  }

  #[inline(always)]
  fn hash_mut(&mut self, context: &mut impl Context, mut key: K) -> u64 {
    let mut h = DefaultHasher::new();
    return key.context_hash_mut(context, &mut h);
  }

  #[inline]
  fn load(&self) -> f64 {
    let storage = self.storage.handle();
    return storage.len() as f64 / storage.capacity() as f64;
  }

  #[inline]
  pub fn find_index(&self, context: &mut impl Context, key: K) -> Option<usize> {
    let hash = self.hash(context, key);
    let storage = self.storage.handle();
    let cap = storage.capacity();
    let position = (hash % (cap as u64)) as usize;
    if !storage[position].is_null() && storage[position].key == key {
      return Some(position);
    } else {
      for i in position..cap {
        if !storage[i].is_null() && storage[i].key == key {
          return Some(i);
        }
      }
      for i in (0..position).rev() {
        if !storage[i].is_null() && storage[i].key == key {
          return Some(i);
        }
      }
    }
    return None;
  }

  #[inline]
  fn insert_to(&mut self, context: &mut impl AllocationOnlyContext, key: K, value: V, hash: u64) {
    let mut storage = self.storage.handle();
    let cap = storage.capacity();
    let position = (hash % (cap as u64)) as usize;
    let mut cur_entry = HashMapEntry::<K, V>::new(context, key, value, hash, position);
    storage.set_length(storage.len() + 1);
    if storage[position].is_null() {
      storage[position] = cur_entry;
    } else {
      let mut cur_position = self.get_next_position(cap, position);
      'outer: loop {
        for i in cur_position..cap {
          let entry = storage[i];
          if entry.is_null() {
            storage[i] = cur_entry;
            return;
          }
          if udiff(i, entry.position) < udiff(cur_entry.position, i) {
            storage[i] = cur_entry;
            cur_entry = entry;
            cur_position = self.get_next_position(cap, i);
            continue 'outer;
          }
        }
        for i in (0..cur_position).rev() {
          let entry = storage[i];
          if entry.is_null() {
            storage[i] = cur_entry;
            return;
          }
          if udiff(i, entry.position) < udiff(cur_entry.position, i) {
            storage[i] = cur_entry;
            cur_entry = entry;
            cur_position = self.get_next_position(cap, i);
            break;
          }
        }
      }
    }
  }

  #[inline(always)]
  fn get_next_position(&self, cap: usize, index: usize) -> usize {
    return if index < cap - 1 { index + 1 } else { index };
  }

  #[inline]
  fn grow(&mut self, context: &mut impl AllocationOnlyContext) {
    let old_storage = self.storage.handle();
    let mut new_storage = InternalArray::<HashMapEntry<K, V>>::new(context, old_storage.capacity() * 2);
    new_storage.fill(HashMapEntry::<K, V>(HeapLayout::<HashMapEntryLayout<K, V>>::null()));
    self.storage.set(new_storage);
    for i in 0..old_storage.capacity() {
      let entry = old_storage[i];
      if !entry.0.is_null() {
        self.insert_to(context, entry.key, entry.value, entry.hash);
      }
    }
  }
}

pub struct HashMapIterator<K: HashMapKey, V: Copy> {
  map: HashMap<K, V>,
  index: usize,
}

impl<K: HashMapKey, V: Copy> Iterator for HashMapIterator<K, V> {
  type Item = (K, V);
  fn next(&mut self) -> Option<Self::Item> {
    let storage = self.map.storage.handle();
    if storage.length() == 0 {
      return None;
    }
    if self.index >= storage.capacity() {
      return None;
    }
    while storage[self.index].is_null() {
      self.index += 1;
      if self.index >= storage.capacity() {
        return None;
      }
    }
    let result = storage[self.index];
    self.index += 1;
    return Some((result.key, result.value));
  }
}

impl<K: HashMapKey, V: Copy> IntoIterator for HashMap<K, V> {
  type Item = (K, V);
  type IntoIter = HashMapIterator<K, V>;

  fn into_iter(self) -> Self::IntoIter {
    return HashMapIterator::<K, V> { map: self, index: 0 };
  }
}

#[cfg(test)]
mod hash_map_test {
  use super::super::string::from_utf8;
  use super::*;
  use crate::context::isolate;
  use crate::structs::FixedU16CodePointArray;

  #[test]
  fn hash_map_insert_test() {
    let mut mc = isolate::context();
    let mut h = HashMap::<FixedU16CodePointArray, u32>::new(&mut mc);
    for i in 0..50 {
      let str = format!("test value{} !!", i);
      let u16_str = from_utf8(&mut mc, &str);
      h.insert(&mut mc, u16_str, i);
    }
    for i in 0..50 {
      let str = format!("test value{} !!", i);
      let u16_str = from_utf8(&mut mc, &str);
      assert_eq!(h.find(&mut mc, u16_str).unwrap(), i);
    }
  }

  #[test]
  fn hash_map_delete_test() {
    let mut mc = isolate::context();
    let mut h = HashMap::<FixedU16CodePointArray, u32>::new(&mut mc);
    for i in 0..50 {
      let str = format!("test value{} !!", i);
      let u16_str = from_utf8(&mut mc, &str);
      h.insert(&mut mc, u16_str, i);
    }
    for i in 0..50 {
      let str = format!("test value{} !!", i);
      let u16_str = from_utf8(&mut mc, &str);
      assert_eq!(h.delete(&mut mc, u16_str), true);
      assert_eq!(h.find(&mut mc, u16_str).is_none(), true);
    }
  }
}
