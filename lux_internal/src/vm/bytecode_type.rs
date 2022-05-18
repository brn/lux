use crate::structs::{InternalArray, Repr};

pub type Bytecode = u8;

pub type Register = u16;

macro_rules! _strip {
    (+ $($rest: tt)*) => {
        $($rest)*
    };
    (&& $($rest: tt)*) => {
        $($rest)*
    }
}

pub trait SizedBytecode {
  fn size(&self) -> usize;
  fn bc(&self) -> u8;
}

macro_rules! impl_bytecode {
  ($($item:ident ( $($param:tt : $operands:ty),*$(,)? )),+$(,)?) => {
    impl_bytecode!(0u8, $($item ($($param: $operands,)*),)+);
    #[derive(Copy, Clone)]
    pub enum BytecodeRepr {
      $($item($item),)*
    }

    impl std::fmt::Debug for BytecodeRepr {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", match self {
          $(
            &BytecodeRepr::$item(body) => format!("{:?}", body),
          )*
        })
      }
    }

    impl BytecodeRepr {
      pub fn size(&self) -> usize {
        return match self {
          $(
            &BytecodeRepr::$item(body) => body.size(),
          )*
        };
      }

      pub fn opcode(&self) -> u8 {
        return match self {
          $(
            &BytecodeRepr::$item(body) => body.bc(),
          )*
        };
      }

      pub fn encode(&self, vector: InternalArray<Bytecode>) {
        return match self {
          $(
            &BytecodeRepr::$item(body) => body.encode(vector),
          )*
        };
      }
    }

    impl std::cmp::PartialEq for BytecodeRepr {
      fn eq(&self, other: &BytecodeRepr) -> bool {
        return match self {
          $(
            &BytecodeRepr::$item(body) => {
              match other {
                &BytecodeRepr::$item(other_body) => body == other_body,
                _ => false
              }
            },
          )*
        };
      }
    }
  };
  ($index:expr$(,)?) => {};
  ($index:expr, $item:ident ( $($param:tt : $operands:ty),*$(,)? ), $($rest_item:ident ( $($rest_param:tt : $rest_operands:ty),*$(,)? )),*$(,)?) => {
    impl_bytecode!(@impl $index, $item($($param: $operands),*));
    impl_bytecode!($index + 1u8, $($rest_item ($($rest_param: $rest_operands,)*),)*);
  };
  (@impl $index:expr, $item:ident ( $($param:tt : $operands:ty),*$(,)? )) => {
    #[repr(packed)]
    #[derive(Clone, Copy)]
    pub struct $item {
      $(
        $param: $operands,
      )*
    }

    impl std::fmt::Debug for $item {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} {{opcode: {}, operands: {:?}}}", stringify!($item), $index, ($(self.$param),*))
      }
    }

    impl $item {
      pub fn new($($param : $operands,)*) -> BytecodeRepr {
        return BytecodeRepr::$item($item {
          $($param,)*
        });
      }

      pub fn encode(&self, mut vector: InternalArray<Bytecode>) {
        let addr = unsafe {std::mem::transmute::<&$item, *const Bytecode>(self)};
        let count = self.size() / std::mem::size_of::<Bytecode>();
        vector.push(self.bc());
        for offset in 0..count {
          vector.push(unsafe {
            std::ptr::read_unaligned(addr.offset(offset as isize))
          });
        }
      }
    }

    impl std::cmp::PartialEq for $item {
      fn eq(&self, other: &$item) -> bool {
        return _strip!($(&& self.$param == other.$param)*);
      }
    }

    impl SizedBytecode for $item {
      #[inline]
      fn size(&self) -> usize {
        return _strip!($(+ std::mem::size_of::<$operands>())+);
      }

      #[inline]
      fn bc(&self) -> u8 {
        return $index;
      }
    }
  }
}

impl_bytecode! {
    /* Acc = Acc + Register */
    I32Add(r: Register),

    /* Acc = Acc - Register  */
    I32Sub(r: Register),

    /* Acc = Acc * Register */
    I32Mul(r: Register),

    /* Acc = Acc / Register */
    I32Div(r: Register),

    /* Acc = Acc - Register */
    F64Add(r: Register),

    /* Acc = Acc * Register */
    F64Sub(r: Register),

    /* Acc = Acc / Register */
    F64Mul(r: Register),

    /* Acc = Acc = Reg */
    F64Div(r: Register),

    /* Reg = Acc */
    StoreRA(r: Register),

    /* Reg = Repr */
    StoreR(r: Register, repr: Repr),

    /* Acc = Reg */
    LoadA(r: Register)
}

#[cfg(test)]
mod test {
  use super::*;
  use crate::context::LuxContext;

  #[test]
  fn bytecode_eq_test() {
    let a = I32Add::new(0);
    let b = I32Add::new(1);
    assert_ne!(a, b);
    let c = I32Add::new(10);
    let d = I32Add::new(10);
    assert_eq!(c, d);
  }

  #[test]
  fn bytecode_opcode_test() {
    let a = I32Add::new(0);
    let b = I32Sub::new(1);
    let c = I32Mul::new(1);
    let d = I32Div::new(1);
    assert_ne!(a.opcode(), b.opcode());
    assert_ne!(a.opcode(), c.opcode());
    assert_ne!(a.opcode(), d.opcode());
    assert_ne!(b.opcode(), c.opcode());
    assert_ne!(b.opcode(), d.opcode());
    assert_ne!(c.opcode(), d.opcode());
  }
}
