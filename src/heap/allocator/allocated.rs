use super::managed::Managed;
use crate::context::Context;

pub trait Allocated<'a, T> {
  fn new(context: &'a mut Context) -> Managed<'a, T> {
    return context.allocate::<T>();
  }
}
