macro_rules! token_enum {
  ($name:ident { $(($item:ident, $pseudo:tt, $allowed_in_property_name:expr, $contextual:tt, $repr:expr),)* }) => {
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
          OpMulAssign | OpDivAssign | OpPlusAssign | OpMinusAssign | OpShlAssign | OpShrAssign | OpUShrAssign | OpAndAssign | OpOrAssign | OpXorAssign | OpPowAssign |OpAssign | OpModAssign => true,
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
    (Invalid,              true,  false, false, ""),
    (ArrowFunctionGlyph,   false, false, false, "=>"),
    (Await,                false, true,  false, "await"),
    (BackQuote,            false, false, false, "`"),
    (Break,                false, true,  false, "break"),
    (Case,                 false, true,  false, "case"),
    (Catch,                false, true,  false, "catch"),
    (Class,                false, true,  false, "class"),
    (Colon,                false, false, false, ":"),
    (Comma,                false, false, false, ","),
    (Const,                false, true,  false, "const"),
    (Continue,             false, true,  false, "continue"),
    (Debugger,             false, true,  false, "debugger"),
    (Default,              false, true,  false, "default"),
    (Delete,               false, true,  false, "delete"),
    (Do,                   false, true,  false, "do"),
    (Dot,                  false, false, false, "."),
    (Else,                 false, true,  false, "else"),
    (Enum,                 false, true,  false, "enum"),
    (Export,               false, true,  false, "export"),
    (Extends,              false, true,  false, "extends"),
    (False,                false, true,  false, "false"),
    (Finally,              false, true,  false, "finally"),
    (For,                  false, true,  false, "for"),
    (Function,             false, true,  false, "function"),
    (If,                   false, true,  false, "if"),
    (Import,               false, true,  false, "import"),
    (In,                   false, true,  false, "in"),
    (Instanceof,           false, true,  false, "instanceof"),
    (LeftBrace,            false, false, false, "{"),
    (LeftBracket,          false, false, false, "["),
    (LeftParen,            false, false, false, "("),
    (New,                  false, true,  false, "new"),
    (Null,                 false, true,  false, "null"),
    (OpAnd,                false, false, false, "&"),
    (OpAndAssign,          false, false, false, "&="),
    (OpAssign,             false, false, false, "="),
    (OpDecrement,          false, false, false, "--"),
    (OpDiv,                false, false, false, "/"),
    (OpDivAssign,          false, false, false, "/="),
    (OpEq,                 false, false, false, "=="),
    (OpGreaterThan,        false, false, false, ">"),
    (OpGreaterThanOrEq,    false, false, false, ">="),
    (OpIncrement,          false, false, false, "++"),
    (OpLessThan,           false, false, false, "<"),
    (OpLessThanOrEq,       false, false, false, "<="),
    (OpLogicalAnd,         false, false, false, "&&"),
    (OpLogicalOr,          false, false, false, "||"),
    (OpMinus,              false, false, false, "-"),
    (OpMinusAssign,        false, false, false, "-="),
    (OpMod,                false, false, false, "%"),
    (OpModAssign,          false, false, false, "%="),
    (OpMul,                false, false, false, "*"),
    (OpMulAssign,          false, false, false, "*="),
    (OpNot,                false, false, false, "!"),
    (OpNotEq,              false, false, false, "!="),
    (OpNullCoalescing,     false, false, false, "??"),
    (OpOptionalChaining,   false, false, false, "?."),
    (OpOr,                 false, false, false, "|"),
    (OpOrAssign,           false, false, false, "|="),
    (OpPlus,               false, false, false, "+"),
    (OpPlusAssign,         false, false, false, "+="),
    (OpPow,                false, false, false, "**"),
    (OpPowAssign,          false, false, false, "**="),
    (OpShl,                false, false, false, "<<"),
    (OpShlAssign,          false, false, false, "<<="),
    (OpShr,                false, false, false, ">>"),
    (OpShrAssign,          false, false, false, ">>="),
    (OpStrictEq,           false, false, false, "==="),
    (OpStrictNotEq,        false, false, false, "!=="),
    (OpTilde,              false, false, false, "~"),
    (OpUShr,               false, false, false, ">>>"),
    (OpUShrAssign,         false, false, false, ">>>="),
    (OpXor,                false, false, false, "^"),
    (OpXorAssign,          false, false, false, "^="),
    (Question,             false, false, false, "?"),
    (RegExp,               true,  false, false, "$RegExp"),
    (Return,               false, true,  false, "return"),
    (RightBrace,           false, false, false, "}"),
    (RightBracket,         false, false, false, "]"),
    (RightParen,           false, false, false, ")"),
    (Spread,               false, false, false, "..."),
    (Super,                false, true,  false, "super"),
    (Switch,               false, true,  false, "switch"),
    (Terminate,            false, false, false, ";"),
    (This,                 false, true,  false, "this"),
    (Throw,                false, true,  false, "throw"),
    (True,                 false, true,  false, "true"),
    (Try,                  false, true,  false, "try"),
    (Typeof,               false, true,  false, "typeof"),
    (Var,                  false, true,  false, "var"),
    (Void,                 false, true,  false, "void"),
    (While,                false, true,  false, "while"),
    (With,                 false, true,  false, "with"),
    (Yield,                false, true,  false, "yield"),
    (YieldAggregator,      true,  true,  false, "yield*"),
    (Identifier,           true,  false, false, "$Identifier"),
    (NumericLiteral,       true,  false, false, "$NumericLiteral"),
    (ImplicitOctalLiteral, true,  false, false, "$ImplicitOctalLiteral"),
    (StringLiteral,        true,  false, false, "$StringLiteral"),
    (Template,             true,  false, false, "$Template"),
    (TemplateParts,        true,  false, false, "$TemplateParts"),
    (TemplateSubstitution, true,  false, false, "$TemplateSubstitution"),
    (End,                  true,  false, false, "$End"),
    (Eval,                 false, true,  true,  "eval"),
    (Arguments,            false, true,  true,  "arguments"),
    (Async,                false, true,  true,  "async"),
    (Let,                  false, true,  true,  "let"),
    (Target,               false, true,  true,  "target"),
    (Get,                  false, true,  true,  "get"),
    (Set,                  false, true,  true,  "set"),
    (From,                 false, true,  true,  "from"),
    (As,                   false, true,  true,  "as"),
    (Implements,           false, true,  true,  "implements"),
    (Interface,            false, true,  true,  "interface"),
    (Package,              false, true,  true,  "package"),
    (Private,              false, true,  true,  "private"),
    (Protected,            false, true,  true,  "protected"),
    (Public,               false, true,  true,  "public"),
    (Static,               false, true,  true,  "static"),
  }
}
