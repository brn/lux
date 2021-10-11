macro_rules! token_enum {
  ($name:ident { $(($item:ident, $pseudo:tt, $allowed_in_property_name:expr, $contextual:tt, $is_puncture:tt, $repr:expr),)* }) => {
    #[derive(PartialEq, Eq, Copy, Clone, Debug)]
    pub enum $name {
      $(
        $item,
      )*
    }

    impl std::fmt::Display for $name {
      fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        return write!(f, "{:?}", self);
      }
    }

    impl $name {
      pub fn is_pseudo_token(&self) -> bool {
        match self {
          $(
            &$name::$item => $pseudo,
          )*
        }
      }

      pub fn is_allowed_in_property_name(&self) -> bool {
        match self {
          $(
            &$name::$item => $allowed_in_property_name,
          )*
        }
      }

      pub const fn is_puncture(&self) -> bool {
        match self {
          $(
            &$name::$item => $is_puncture,
          )*
        }
      }

      pub const fn is_contextual_keyword(&self) -> bool {
        match self {
          $(
            &$name::$item => $contextual,
          )*
        }
      }

      pub const fn symbol(&self) -> &'static str {
        match self {
          $(
            &$name::$item => $repr,
          )*
        }
      }

      pub const fn str(&self) -> &'static str {
        return match self {
          $(
            &$name::$item => stringify!($item),
          )*
        }
      }

      pub fn values() -> std::slice::Iter<'static, $name> {
        static VALUES: &'static [$name] = &[$($name::$item),*];
        VALUES.into_iter()
      }

      pub fn is_assignment_operator(&self) -> bool {
        use $name::*;
        return match self {
          OpLogicalAndAssign | OpLogicalOrAssign | OpMulAssign | OpDivAssign | OpPlusAssign | OpMinusAssign | OpShlAssign | OpShrAssign | OpUShrAssign | OpAndAssign | OpOrAssign | OpXorAssign | OpPowAssign |OpAssign | OpModAssign => true,
          _ => false
        }
      }

      pub fn one_of(&self, tokens: &[Token]) -> bool {
        match self {
          $(
            &$name::$item => {
              for p in tokens {
                if *p == $name::$item {
                  return true;
                }
              }
              return false;
            },
          )*
        }
      }
    }
  }
}

token_enum! {
  Token {
    (Invalid,              true,  false, false, false, ""),
    (ArrowFunctionGlyph,   false, false, false, true,  "=>"),
    (Await,                false, true,  false, false, "await"),
    (BackQuote,            false, false, false, true,  "`"),
    (Break,                false, true,  false, false, "break"),
    (Case,                 false, true,  false, false, "case"),
    (Catch,                false, true,  false, false, "catch"),
    (Class,                false, true,  false, false, "class"),
    (Colon,                false, false, false, true,  ":"),
    (Comma,                false, false, false, true,  ","),
    (Const,                false, true,  false, false, "const"),
    (Continue,             false, true,  false, false, "continue"),
    (Comment,              true,  false, false, false, "$Comment"),
    (Debugger,             false, true,  false, false, "debugger"),
    (Default,              false, true,  false, false, "default"),
    (Delete,               false, true,  false, false, "delete"),
    (Do,                   false, true,  false, false, "do"),
    (Dot,                  false, false, false, true,  "."),
    (Else,                 false, true,  false, false, "else"),
    (Enum,                 false, true,  false, false, "enum"),
    (Export,               false, true,  false, false, "export"),
    (Extends,              false, true,  false, false, "extends"),
    (False,                false, true,  false, false, "false"),
    (Finally,              false, true,  false, false, "finally"),
    (For,                  false, true,  false, false, "for"),
    (Function,             false, true,  false, false, "function"),
    (If,                   false, true,  false, false, "if"),
    (Import,               false, true,  false, false, "import"),
    (In,                   false, true,  false, false, "in"),
    (Instanceof,           false, true,  false, false, "instanceof"),
    (LeftBrace,            false, false, false, true,  "{"),
    (LeftBracket,          false, false, false, true,  "["),
    (LeftParen,            false, false, false, true,  "("),
    (New,                  false, true,  false, false, "new"),
    (Null,                 false, true,  false, false, "null"),
    (OpAnd,                false, false, false, true,  "&"),
    (OpAndAssign,          false, false, false, true,  "&="),
    (OpAssign,             false, false, false, true,  "="),
    (OpDecrement,          false, false, false, true,  "--"),
    (OpDiv,                false, false, false, false, "/"),
    (OpDivAssign,          false, false, false, false, "/="),
    (OpEq,                 false, false, false, true,  "=="),
    (OpGreaterThan,        false, false, false, true,  ">"),
    (OpGreaterThanOrEq,    false, false, false, true,  ">="),
    (OpIncrement,          false, false, false, true,  "++"),
    (OpLessThan,           false, false, false, true,  "<"),
    (OpLessThanOrEq,       false, false, false, true,  "<="),
    (OpLogicalAnd,         false, false, false, true,  "&&"),
    (OpLogicalAndAssign,   false, false, false, true,  "&&="),
    (OpLogicalOr,          false, false, false, true,  "||"),
    (OpLogicalOrAssign,    false, false, false, true,  "||="),
    (OpMinus,              false, false, false, true,  "-"),
    (OpMinusAssign,        false, false, false, true,  "-="),
    (OpMod,                false, false, false, true,  "%"),
    (OpModAssign,          false, false, false, true,  "%="),
    (OpMul,                false, false, false, true,  "*"),
    (OpMulAssign,          false, false, false, true,  "*="),
    (OpNot,                false, false, false, true,  "!"),
    (OpNotEq,              false, false, false, true,  "!="),
    (OpNullCoalescing,     false, false, false, true,  "??"),
    (OpOptionalChaining,   false, false, false, true,  "?."),
    (OpOr,                 false, false, false, true,  "|"),
    (OpOrAssign,           false, false, false, true,  "|="),
    (OpPlus,               false, false, false, true,  "+"),
    (OpPlusAssign,         false, false, false, true,  "+="),
    (OpPow,                false, false, false, true,  "**"),
    (OpPowAssign,          false, false, false, true,  "**="),
    (OpShl,                false, false, false, true,  "<<"),
    (OpShlAssign,          false, false, false, true,  "<<="),
    (OpShr,                false, false, false, true,  ">>"),
    (OpShrAssign,          false, false, false, true,  ">>="),
    (OpStrictEq,           false, false, false, true,  "==="),
    (OpStrictNotEq,        false, false, false, true,  "!=="),
    (OpTilde,              false, false, false, true,  "~"),
    (OpUShr,               false, false, false, true,  ">>>"),
    (OpUShrAssign,         false, false, false, true,  ">>>="),
    (OpXor,                false, false, false, true,  "^"),
    (OpXorAssign,          false, false, false, true,  "^="),
    (Question,             false, false, false, true,  "?"),
    (PrivateIdentifier,    true,  false, true,  false, "#identifier"),
    (RegExp,               true,  false, false, false, "$RegExp"),
    (Return,               false, true,  false, false, "return"),
    (RightBrace,           false, false, false, false, "}"),
    (RightBracket,         false, false, false, true,  "]"),
    (RightParen,           false, false, false, true,  ")"),
    (Spread,               false, false, false, true,  "..."),
    (Super,                false, true,  false, false, "super"),
    (Switch,               false, true,  false, false, "switch"),
    (Terminate,            false, false, false, false, ";"),
    (This,                 false, true,  false, false, "this"),
    (Throw,                false, true,  false, false, "throw"),
    (True,                 false, true,  false, false, "true"),
    (Try,                  false, true,  false, false, "try"),
    (Typeof,               false, true,  false, false, "typeof"),
    (Var,                  false, true,  false, false, "var"),
    (Void,                 false, true,  false, false, "void"),
    (While,                false, true,  false, false, "while"),
    (With,                 false, true,  false, false, "with"),
    (Yield,                false, true,  false, false, "yield"),
    (YieldAggregator,      true,  true,  false, false, "yield*"),
    (Identifier,           true,  true, false,  false, "$Identifier"),
    (RegExpFlag,           true,  true, false,  false, "$RegExpFlag"),
    (NumericLiteral,       true,  true, false,  false, "$NumericLiteral"),
    (ImplicitOctalLiteral, true,  false, false, false, "$ImplicitOctalLiteral"),
    (StringLiteral,        true,  false, false, false, "$StringLiteral"),
    (Template,             true,  false, false, false, "$Template"),
    (TemplateParts,        true,  false, false, false, "$TemplateParts"),
    (TemplateSubstitution, true,  false, false, false, "$TemplateSubstitution"),
    (End,                  true,  false, false, false, "$End"),
    (Eval,                 false, true,  true,  false, "eval"),
    (Arguments,            false, true,  true,  false, "arguments"),
    (Async,                false, true,  true,  false, "async"),
    (Let,                  false, true,  true,  false, "let"),
    (Target,               false, true,  true,  false, "target"),
    (Get,                  false, true,  true,  false, "get"),
    (Set,                  false, true,  true,  false, "set"),
    (Meta,                 false, true,  true,  false, "meta"),
    (From,                 false, true,  true,  false, "from"),
    (As,                   false, true,  true,  false, "as"),
    (Implements,           false, true,  true,  false, "implements"),
    (Interface,            false, true,  true,  false, "interface"),
    (Package,              false, true,  true,  false, "package"),
    (Private,              false, true,  true,  false, "private"),
    (Protected,            false, true,  true,  false, "protected"),
    (Prototype,            false, true,  true,  false, "prototype"),
    (Proto,                false, true,  true,  false, "__proto__"),
    (Public,               false, true,  true,  false, "public"),
    (Static,               false, true,  true,  false, "static"),
    (StringLiteralES,      true,  true,  true,  false, "$StringLiteralES"),
    (Of,                   false, true,  true,  false, "of"),
  }
}
