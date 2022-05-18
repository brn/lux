use std::hash::{Hash, Hasher};

macro_rules! gc_enum {
  ($name:ident { $($item:ident($id_start:expr, $id_continue:expr),)* }) => {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum $name {
      $(
        $item,
      )*
    }

    impl Default for $name {
      fn default() -> $name {
        return $name::Lu;
      }
    }
    impl PartialOrd for $name {
      fn partial_cmp(&self, other: &$name) -> Option<std::cmp::Ordering> {
        return Some(self.cmp(other));
      }
    }
    impl Ord for $name {
      fn cmp(&self, other: &$name) -> std::cmp::Ordering {
        return match self {
          $(
            &$name::$item => ($name::$item as usize).cmp(&(*other as usize)),
          )*
        }
      }
    }
    impl Hash for $name {
      fn hash<H: Hasher>(&self, state: &mut H) {
        return match self {
          $(
            &$name::$item => state.write_u8($name::$item as u8),
          )*
        }
      }
    }
    impl $name {
      pub fn value(&self) -> &'static str {
        return match self {
          $(
            &$name::$item => stringify!($item),
          )*
        }
      }
      pub fn is_id_start(&self) -> bool {
        return match self {
          $(
            &$name::$item => $id_start,
          )*
        }
      }

      pub fn is_id_continue(&self) -> bool {
        return match self {
          $(
            &$name::$item => $id_continue,
          )*
        }
      }
    }

    fn gc_from_str(s: &str) -> Result<GeneralCategory, String> {
      return match s {
        $(
          stringify!($item) => Ok($name::$item),
        )*
          _ => Err(format!("{} is not a valid GeneralCategory", s))
      };
    }
  };
}

gc_enum! {
  GeneralCategory {
    Lu(true, true),
    Ll(true, true),
    Lt(true, true),
    Lm(true, true),
    Lo(true, true),
    Mn(false, true),
    Mc(false, true),
    Me(false, false),
    Nd(false, true),
    Nl(true, false),
    No(false, false),
    Zs(false, false),
    Zl(false, false),
    Zp(false, false),
    Cc(false, false),
    Cf(false, false),
    Cs(false, false),
    Co(false, false),
    Pc(false, true),
    Pd(false, false),
    Ps(false, false),
    Pe(false, false),
    Pi(false, false),
    Pf(false, false),
    Po(false, false),
    Sm(false, false),
    Sc(false, false),
    Sk(false, false),
    So(false, false),
    Cn(false, false),
  }
}
// 29

macro_rules! bidi_enum {
  ($name:ident { $($item:ident,)* }) => {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum $name {
      $(
        $item,
      )*
    }

    impl Default for $name {
      fn default() -> $name {
        return $name::L;
      }
    }
    impl PartialOrd for $name {
      fn partial_cmp(&self, other: &$name) -> Option<std::cmp::Ordering> {
        return Some(self.cmp(other));
      }
    }
    impl Ord for $name {
      fn cmp(&self, other: &$name) -> std::cmp::Ordering {
        return match self {
          $(
            &$name::$item => ($name::$item as usize).cmp(&(*other as usize)),
          )*
        }
      }
    }
    impl Hash for $name {
      fn hash<H: Hasher>(&self, state: &mut H) {
        return match self {
          $(
            &$name::$item => state.write_u8($name::$item as u8),
          )*
        }
      }
    }
    impl  $name {
      pub fn value(&self) -> &'static str {
        return match self {
          $(
            &$name::$item => paste! {stringify!([<$item:upper>])},
          )*
        }
      }
    }

    fn bidi_from_str(s: &str) -> Result<$name, String> {
      paste! {
        return match s {
          $(
            stringify!([<$item:upper>]) => Ok($name::$item),
          )*
            _ => Err(format!("{} is not a valid BidiCategory", s))
        };
      }
    }
  };
}

bidi_enum! {
  BidiCategory {
    L,
    Lre,
    Lro,
    R,
    Al,
    Rle,
    Rlo,
    Pdf,
    En,
    Es,
    Et,
    An,
    Cs,
    Nsm,
    Bn,
    B,
    S,
    Ws,
    On,
    Lri,
    Rli,
    Fsi,
    Pdi,
  }
}

macro_rules! decmp_type_enum {
  ($name:ident { $($item:ident($repr:tt),)* }) => {
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum $name {
      $(
        $item,
      )*
    }

    fn dt_from_str(s: &str) -> Result<$name, String> {
        return match s {
          $(
            stringify!($repr) => Ok($name::$item),
          )*
            _ => Err(format!("{} is not a valid DecompositionType", s))
        };
      }
  };
}
decmp_type_enum! {
  DecompositionType {
    Font(font),
    NoBreak(noBreak),
    Initial(initial),
    Medial(medial),
    Final(final),
    Isolated(isolated),
    Circle(circle),
    Super(super),
    Sub(sub),
    Vertical(vertical),
    Wide(wide),
    Narrow(narrow),
    Small(small),
    Square(square),
    Fraction(fraction),
    Compat(compat),
  }
}
// 15

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UcdIdProperty {
  None = 0,
  IdStart,
  IdContinue,
}
impl Default for UcdIdProperty {
  fn default() -> UcdIdProperty {
    return UcdIdProperty::None;
  }
}

pub struct UcdProps {}
impl UcdProps {
  pub const NONE: u16 = 0;
  pub const ID_START: u16 = 1;
  pub const ID_CONTINUE: u16 = 2;
}
pub type RawUcd = (u32, GeneralCategory, u32, Option<DecompositionType>, [u32; 18], u32, u32, u32, u16);
