use crate::def;

#[repr(C)]
struct Header {
  ///
  /// Use as object size information and used as forwarded pointer and mark bit.
  ///
  /// |--data---|--size tag--|--type tag--|--mark bit--|
  /// |--56bit--|----1bit----|----6bit----|----1bit----|
  ///
  size: u64,
}

impl Header {
  pub fn set_size(&mut self, size: usize) {
    self.size = (size << 2) | 2;
  }

  pub fn size(&self) -> usize {
    if self.is_size_used_as_size() {
      return self.size >> 2;
    }
    return 0;
  }

  pub fn set_forwarded_pointer(&mut self, addr: *mut def::Byte) {
    self.size = addr as usize;
  }

  pub fn forwarded_pointer(&self) {
    if !self.is_size_used_as_size() {
      return (self.size >> 1) as *mut def::Byte;
    }
    return 0xdeadbeef as *mut def::Byte;
  }

  pub fn is_size_used_as_size(&self) -> bool {
    return (self.size & 2) == 2;
  }

  pub fn mark(&mut self) {
    self.size |= 1;
  }

  pub fn unmark(&mut self) {
    self.size &= !1;
  }
}

#[repr(C)]
struct Managed {
  header: Header,
}
