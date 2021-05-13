pub fn cast_ptr<'a, T>(ptr: *const T) -> &'a T {
  return unsafe { &*ptr };
}

pub fn cast_ptr_mut<'a, T>(ptr: *mut T) -> &'a mut T {
  return unsafe { &mut *ptr };
}
