use crate::def;
use std::ptr;

mod vha {
  pub use super::super::virtual_heap_allocator::*;
}

const MAX_ALLOCATION_RETRY: u32 = 10;

pub unsafe fn allocate_aligned<'a>(block_size: usize, alignment: usize) -> Option<*mut libc::c_void> {
  if alignment <= def::ALIGNMENT {
    let ret = vha::allocate(
      ptr::null_mut(),
      block_size,
      vha::Prot::READ | vha::Prot::WRITE,
      vha::Flags::ANONYMOUS | vha::Flags::PRIVATE,
      vha::AllocationType::RESERVE,
    );

    if ret.is_ok() {
      return ret.ok();
    }
    return None;
  }

  let mut retry: u32 = 0;

  let allocated_size = block_size + (alignment - def::ALIGNMENT);
  loop {
    retry += 1;

    if retry > MAX_ALLOCATION_RETRY {
      return None;
    }

    match vha::allocate(
      ptr::null_mut(),
      allocated_size,
      vha::Prot::READ | vha::Prot::WRITE,
      vha::Flags::ANONYMOUS | vha::Flags::PRIVATE,
      vha::AllocationType::RESERVE,
    ) {
      Ok(allocated_address) => {
        let next_alignment = alignment - 1;
        let aligned_address = ((allocated_address.offset(next_alignment as isize) as usize) & !next_alignment) as *mut libc::c_void;
        let roundup = aligned_address.offset_from(allocated_address) as usize;
        let unused = alignment - def::ALIGNMENT - roundup;

        if unmap_excess_address(roundup, allocated_address, aligned_address, unused, block_size, allocated_size) {
          return Some(aligned_address);
        }
        return None;
      }
      Err(_) => {}
    };
  }
}

#[cfg(unix)]
unsafe fn unmap_excess_address(
  roundup: usize,
  allocated_address: *mut libc::c_void,
  aligned_address: *mut libc::c_void,
  unused: usize,
  block_size: usize,
  allocated_size: usize,
) -> bool {
  use nix::sys::mman;
  if roundup > 0 {
    vha::deallocate(allocated_address, roundup);
  }
  if unused > 0 {
    vha::deallocate(allocated_address.offset(allocated_size as isize).offset(-(unused as isize)), unused);
  }
  let mut flags = mman::ProtFlags::empty();
  flags.insert(mman::ProtFlags::PROT_READ);
  flags.insert(mman::ProtFlags::PROT_WRITE);
  let _ = mman::mprotect(aligned_address, block_size, flags);
  return true;
}

#[cfg(windows)]
unsafe fn unmap_excess_address(
  roundup: usize,
  allocated_address: *mut libc::c_void,
  aligned_address: *mut libc::c_void,
  unused: usize,
  block_size: usize,
  allocated_size: usize,
) -> bool {
  VirtualHeapAllocator::unmap(allocated_address, roundup, vha::types::DECOMMIT);
  VirtualHeapAllocator::unmap(
    allocated_address.offset((allocated_size - unused) as isize),
    unused,
    vha::types::DECOMMIT,
  );
  match VirtualHeapAllocator::map(
    aligned_address,
    block_size,
    vha::prot::READ | vha::prot::WRITE,
    vha::flags::ANONYMOUS | vha::flags::PRIVATE,
  ) {
    Ok(ptr) => {
      if ptr != aligned_address {
        VirtualHeapAllocator::unmap(allocated_address, block_size + roundup + unused);
        return false;
      }
      return true;
    }
    Err(_) => {
      return false;
    }
  }
}

#[cfg(test)]
mod test {
  struct Test {
    f1: u64,
    f2: u64,
    f3: u64,
    f4: u64,
    f5: u64,
    f6: u64,
  }
  impl Test {
    pub unsafe fn new<'a>(ptr: *mut libc::c_void) -> &'a Test {
      let test = ptr as *mut Test;
      // let layout = std::alloc::Layout::new::<Test>();
      // println!("{}", layout.size());
      (*test).f1 = 1;
      (*test).f2 = 2;
      (*test).f3 = 3;
      (*test).f4 = 4;
      (*test).f5 = 5;
      (*test).f6 = 6;
      return &(*test);
    }
  }

  #[test]
  fn aligned_heap_allocator_test() {
    use super::*;
    mod vha {
      pub use super::super::super::virtual_heap_allocator::*;
    }
    unsafe {
      let mut v = vec![];
      for _ in 0..100 {
        match allocate_aligned(mb!(1), mb!(1)) {
          Some(ptr) => {
            assert_eq!(ptr as isize & 0xFFFF, 0);
            let mut next_ptr = ptr;
            let layout = std::alloc::Layout::new::<Test>();
            for _ in 0..100 {
              let test = Test::new(next_ptr);
              assert_eq!(test.f1, 1);
              assert_eq!(test.f2, 2);
              assert_eq!(test.f3, 3);
              assert_eq!(test.f4, 4);
              assert_eq!(test.f5, 5);
              assert_eq!(test.f6, 6);
              next_ptr = next_ptr.offset(layout.size() as isize);
            }
            v.push(ptr);
          }
          None => {
            panic!("{}", "Memory Allocation Failed");
          }
        }
      }
      for ptr in v {
        vha::deallocate(ptr, mb!(1));
      }
    }
  }
}
