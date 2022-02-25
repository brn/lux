use libc;
use libc::c_void;
use nix::errno;
use nix::sys::mman;
use nix::sys::mman::MapFlags;
use nix::sys::mman::ProtFlags;
use std;
use std::result::Result;

#[cfg(windows)]
use libc::consts::os::extra::{PAGE_EXECUTE, PAGE_EXECUTE_READWRITE, PAGE_READONLY, PAGE_READWRITE};
#[cfg(windows)]
use libc::types::os::arch::extra;

bitflags! {
  pub struct Prot: u8 {
    const NONE = 0x1;
    const READ = 0x2;
    const WRITE = 0x4;
    const EXEC = 0x8;
  }
}
bitflags! {
  pub struct Flags: u8 {
    const NONE = 0x1;
    const ANONYMOUS = 0x2;
    const SHARED = 0x4;
    const PRIVATE = 0x8;
    const FIXED = 0x10;
    const JIT = 0x20;
  }
}
bitflags! {
  pub struct AllocationType: u8 {
    const COMMIT = 0x1;
    const DECOMMIT = 0x2;
    const RESERVE = 0x4;
    const RELEASE = 0x8;
  }
}

#[derive(Debug)]
pub struct AllocationError {
  message: &'static str,
}

impl<'a> std::fmt::Display for AllocationError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self.message)
  }
}

impl AllocationError {
  pub fn new(message: &'static str) -> AllocationError {
    AllocationError { message: message }
  }

  pub fn message(&self) -> &str {
    &self.message
  }
}

#[cfg(unix)]
#[inline]
pub fn allocate(addr: *mut c_void, size: usize, prot: Prot, flags: Flags, _: AllocationType) -> Result<*mut c_void, AllocationError> {
  unsafe {
    return alloc_posix(addr, size, prot, flags);
  }
}

#[cfg(windows)]
#[inline]
pub fn allocate(addr: *mut c_void, size: usize, prot: Prot, flags: Flags, t: AllocationType) -> Result<*mut c_void, AllocationError> {
  unsafe {
    return VirtualHeapAllocator::map_win32(addr, size, prot, flags, t);
  }
}

#[cfg(unix)]
#[inline]
pub fn deallocate(addr: *mut c_void, size: usize) {
  unsafe {
    dealloc_posix(addr, size);
  }
}

#[cfg(windows)]
#[inline]
pub fn deallocate(addr: *mut c_void, size: usize) {
  unsafe {
    dealloc_win32(addr, size);
  }
}

#[cfg(unix)]
unsafe fn alloc_posix(addr: *mut c_void, size: usize, prot: Prot, flags: Flags) -> Result<*mut c_void, AllocationError> {
  let mut prot_flags = ProtFlags::empty();
  let mut mmap_flags = MapFlags::empty();

  if prot != Prot::NONE {
    if prot.contains(Prot::READ) {
      prot_flags |= ProtFlags::PROT_READ;
    }

    if prot.contains(Prot::WRITE) {
      prot_flags |= ProtFlags::PROT_WRITE;
    }

    if prot.contains(Prot::EXEC) {
      prot_flags |= ProtFlags::PROT_EXEC;
    }
  } else {
    prot_flags |= ProtFlags::PROT_NONE;
  }

  if flags != Flags::NONE {
    if flags.contains(Flags::ANONYMOUS) {
      mmap_flags |= MapFlags::MAP_ANON;
    }

    if flags.contains(Flags::SHARED) {
      mmap_flags |= MapFlags::MAP_SHARED;
    }

    if flags.contains(Flags::PRIVATE) {
      mmap_flags |= MapFlags::MAP_PRIVATE;
    }

    if addr.is_null() && flags.contains(Flags::FIXED) {
      mmap_flags |= MapFlags::MAP_FIXED;
    }

    #[cfg(any(target_os = "ios", target_os = "macos"))]
    if flags.contains(Flags::JIT) {
      mmap_flags |= MapFlags::MAP_JIT;
    }
  }

  match mman::mmap(addr, size, prot_flags, mmap_flags, -1, 0) {
    Ok(h) => Result::Ok(h),
    Err(_) => Result::Err(AllocationError::new(get_last_error_posix())),
  }
}

#[cfg(windows)]
unsafe fn alloc_win32(addr: *mut c_void, size: usize, prot: i32, flags: i32, t: i32) -> Result<*mut c_void, AllocationError> {
  let fl_protect: extra::DWORD = 0;
  let fl_allocation_type: extra::DWORD = 0;

  if (prot != prot::NONE) {
    if ((prot & 0x7) == 0x7 || (prot & 0x6) == 0x6) {
      fl_protect = PAGE_EXECUTE_READWRITE;
    } else if ((prot & 0x5) == 0x5) {
      fl_protect = PAGE_EXECUTE_READ;
    } else if ((prot & prot::WRITE) == prot::WRITE || (prot & 0x3) == 0x3) {
      fl_protect = PAGE_READWRITE;
    } else if ((prot & prot::READ) == prot::READ) {
      fl_protect = PAGE_READONLY;
    } else if ((prot & prot::EXEC) == prot::EXEC) {
      fl_protect = PAGE_EXECUTE;
    }
  } else {
    fl_protect = PAGE_READONLY;
  }

  if (t == types::COMMIT || flags != flags::NONE) {
    fl_allocation_type = MEM_COMMIT;
  } else if (t == types::RESERVE) {
    fl_allocation_type = MEM_RESERVE;
  }

  let ret: &mut c_void = libc::VirtualAlloc(addr, size, fl_allocation_type, fl_protect);

  if (ret == std::ptr::null()) {
    return Result::Err(VirtualHeapAllocator::get_last_error_win());
  }
  Result::Ok(ret)
}

#[cfg(unix)]
unsafe fn dealloc_posix(addr: *mut c_void, size: usize) {
  match mman::munmap(addr, size) {
    Ok(_) => {}
    Err(err) => {
      println!("Cause: {}", err);
      panic!("Deallocate maped memory failed");
    }
  }
}

#[cfg(windows)]
unsafe fn dealloc_posix(addr: *mut c_void, size: usize) {
  libc::VirtualFree(addr, size);
}

#[cfg(unix)]
fn get_last_error_posix() -> &'static str {
  errno::from_i32(errno::errno()).desc()
}

#[cfg(windows)]
fn get_last_error_win() -> String {
  let msg_buffer: libc::LPVOID;
  FormatMessage(
    libc::FORMAT_MESSAGE_ALLOCATE_BUFFER | libc::FORMAT_MESSAGE_FROM_SYSTEM | libc::FORMAT_MESSAGE_IGNORE_INSERTS,
    std::ptr::null(),
    libc::GetLastError(),
    libc::MAKELANGID(libc::LANG_NEUTRAL, libc::SUBLANG_DEFAULT),
    &msg_buffer as libc::LPTSTR,
    0,
    std::ptr::null(),
  );
  let ret = String(msg_buffer as &str);
  libc::LocalFree(msg_buffer);
  ret
}

#[cfg(test)]
mod test {
  #[test]
  #[cfg(any(target_os = "ios", target_os = "macos"))]
  fn mmap_test() {
    use super::*;
    use std;
    match allocate(
      std::ptr::null_mut(),
      1024,
      Prot::READ | Prot::WRITE | Prot::EXEC,
      Flags::ANONYMOUS | Flags::PRIVATE | Flags::JIT,
      AllocationType::COMMIT,
    ) {
      Ok(_) => {}
      Err(e) => {
        panic!("{}", e)
      }
    }
  }

  #[cfg(all(not(target_os = "ios"), not(target_os = "macos")))]
  fn mmap_test() {
    use super::*;
    use std;
    match allocate(
      std::ptr::null_mut(),
      1024,
      Prot::READ | Prot::WRITE | Prot::EXEC,
      Flags::ANONYMOUS | Flags::PRIVATE,
      AllocationType::COMMIT,
    ) {
      Ok(_) => {}
      Err(e) => {
        panic!("{}", e)
      }
    }
  }
}
