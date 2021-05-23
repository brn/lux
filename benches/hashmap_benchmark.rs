#[macro_use]
extern crate criterion;

use criterion::Criterion;
use lux::context::{Context, GlobalObjects};
use lux::def::*;
use lux::structs::{FixedU16CodePointArray, FromUnchecked, HashMap, Repr};

mod bench {
  use super::*;
  use std::alloc::{alloc, Layout};

  pub struct MockedContext {
    globals: GlobalObjects,
  }

  impl MockedContext {
    pub fn new() -> MockedContext {
      let mut mc = MockedContext {
        globals: GlobalObjects::from_unchecked(Repr::invalid()),
      };
      mc.globals = GlobalObjects::new(&mut mc);
      return mc;
    }
  }

  impl Context for MockedContext {
    fn allocate(&mut self, size: usize) -> Addr {
      return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
    }

    fn allocate_persist(&mut self, size: usize) -> Addr {
      return unsafe { alloc(Layout::from_size_align(size, ALIGNMENT).unwrap()) };
    }

    fn globals(&self) -> GlobalObjects {
      return self.globals;
    }
  }
}

fn hash_map_insert_find() {
  use bench::*;
  let mut mc = MockedContext::new();
  let mut h = HashMap::<FixedU16CodePointArray, u32>::new(&mut mc);
  for i in 0..50 {
    let str = format!("test value{} !!", i);
    let u16_str = FixedU16CodePointArray::from_utf8(&mut mc, &str);
    h.insert(&mut mc, u16_str, i);
  }
  for i in 0..50 {
    let str = format!("test value{} !!", i);
    let u16_str = FixedU16CodePointArray::from_utf8(&mut mc, &str);
    assert_eq!(h.find(u16_str).unwrap(), i);
  }
}

fn default_hash_map_insert_find() {
  use bench::*;
  use std::collections::HashMap;
  let mut mc = MockedContext::new();
  let mut h = HashMap::<FixedU16CodePointArray, u32>::new();
  for i in 0..50 {
    let str = format!("test value{} !!", i);
    let u16_str = FixedU16CodePointArray::from_utf8(&mut mc, &str);
    h.insert(u16_str, i);
  }
  for i in 0..50 {
    let str = format!("test value{} !!", i);
    let u16_str = FixedU16CodePointArray::from_utf8(&mut mc, &str);
    assert_eq!(*h.get(&u16_str).unwrap(), i);
  }
}

fn bench(c: &mut Criterion) {
  c.bench_function("HashMap insert and find", |b| b.iter(|| hash_map_insert_find()));
  c.bench_function("Default HashMap insert and find", |b| {
    b.iter(|| default_hash_map_insert_find())
  });
}

criterion_group!(benches, bench);
criterion_main!(benches);
