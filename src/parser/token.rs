macro_rules! token_enum {
  ($name:ident { $(($item:ident, $pseudo:tt, $contextual:tt, $repr:expr),)* }) => {
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
    (Invalid,              true,  false, ""),
    (ArrowFunctionGlyph,   false, false, "=>"),
    (Await,                false, false, "await"),
    (BackQuote,            false, false, "`"),
    (Break,                false, false, "break"),
    (Case,                 false, false, "case"),
    (Catch,                false, false, "catch"),
    (Class,                false, false, "class"),
    (Colon,                false, false, ":"),
    (Comma,                false, false, ","),
    (Const,                false, false, "const"),
    (Continue,             false, false, "continue"),
    (Debugger,             false, false, "debugger"),
    (Default,              false, false, "default"),
    (Delete,               false, false, "delete"),
    (Do,                   false, false, "do"),
    (Dot,                  false, false, "."),
    (Else,                 false, false, "else"),
    (Enum,                 false, false, "enum"),
    (Export,               false, false, "export"),
    (Extends,              false, false, "extends"),
    (False,                false, false, "false"),
    (Finally,              false, false, "finally"),
    (For,                  false, false, "for"),
    (Function,             false, false, "function"),
    (If,                   false, false, "if"),
    (Import,               false, false, "import"),
    (In,                   false, false, "in"),
    (Instanceof,           false, false, "instanceof"),
    (LeftBrace,            false, false, "{"),
    (LeftBracket,          false, false, "["),
    (LeftParen,            false, false, "("),
    (New,                  false, false, "new"),
    (Null,                 false, false, "null"),
    (OpAnd,                false, false, "&"),
    (OpAndAssign,          false, false, "&="),
    (OpAssign,             false, false, "="),
    (OpDecrement,          false, false, "--"),
    (OpDiv,                false, false, "/"),
    (OpDivAssign,          false, false, "/="),
    (OpEq,                 false, false, "=="),
    (OpGreaterThan,        false, false, ">"),
    (OpGreaterThanOrEq,    false, false, ">="),
    (OpIncrement,          false, false, "++"),
    (OpLessThan,           false, false, "<"),
    (OpLessThanOrEq,       false, false, "<="),
    (OpLogicalAnd,         false, false, "&&"),
    (OpLogicalOr,          false, false, "||"),
    (OpMinus,              false, false, "-"),
    (OpMinusAssign,        false, false, "-="),
    (OpMod,                false, false, "%"),
    (OpModAssign,          false, false, "%="),
    (OpMul,                false, false, "*"),
    (OpMulAssign,          false, false, "*="),
    (OpNot,                false, false, "!"),
    (OpNotEq,              false, false, "!="),
    (OpNullCoalescing,     false, false, "??"),
    (OpOptionalChaining,   false, false, "?."),
    (OpOr,                 false, false, "|"),
    (OpOrAssign,           false, false, "|="),
    (OpPlus,               false, false, "+"),
    (OpPlusAssign,         false, false, "+="),
    (OpPow,                false, false, "**"),
    (OpPowAssign,          false, false, "**="),
    (OpShl,                false, false, "<<"),
    (OpShlAssign,          false, false, "<<="),
    (OpShr,                false, false, ">>"),
    (OpShrAssign,          false, false, ">>="),
    (OpStrictEq,           false, false, "==="),
    (OpStrictNotEq,        false, false, "!=="),
    (OpTilde,              false, false, "~"),
    (OpUShr,               false, false, ">>>"),
    (OpUShrAssign,         false, false, ">>>="),
    (OpXor,                false, false, "^"),
    (OpXorAssign,          false, false, "^="),
    (Question,             false, false, "?"),
    (RegExp,               true,  false, "$RegExp"),
    (Return,               false, false, "return"),
    (RightBrace,           false, false, "}"),
    (RightBracket,         false, false, "]"),
    (RightParen,           false, false, ")"),
    (Spread,               false, false, "..."),
    (Super,                false, false, "super"),
    (Switch,               false, false, "switch"),
    (Terminate,            false, false, ";"),
    (This,                 false, false, "this"),
    (Throw,                false, false, "throw"),
    (True,                 false, false, "true"),
    (Try,                  false, false, "try"),
    (Typeof,               false, false, "typeof"),
    (Var,                  false, false, "var"),
    (Void,                 false, false, "void"),
    (While,                false, false, "while"),
    (With,                 false, false, "with"),
    (Yield,                false, false, "yield"),
    (YieldAggregator,      true,  false, "yield*"),
    (Identifier,           true,  false, "$Identifier"),
    (NumericLiteral,       true,  false, "$NumericLiteral"),
    (ImplicitOctalLiteral, true,  false, "$ImplicitOctalLiteral"),
    (StringLiteral,        true,  false, "$StringLiteral"),
    (Template,             true,  false, "$Template"),
    (TemplateParts,        true,  false, "$TemplateParts"),
    (TemplateSubstitution, true,  false, "$TemplateSubstitution"),
    (End,                  true,  false, "$End"),
    (Eval,                 false, true,  "eval"),
    (Arguments,            false, true,  "arguments"),
    (Async,                false, true,  "async"),
    (Let,                  false, true,  "let"),
    (Target,               false, true,  "target"),
    (Get,                  false, true,  "get"),
    (Set,                  false, true,  "set"),
    (From,                 false, true,  "from"),
    (As,                   false, true,  "as"),
    (Implements,           false, true,  "implements"),
    (Interface,            false, true,  "interface"),
    (Package,              false, true,  "package"),
    (Private,              false, true,  "private"),
    (Protected,            false, true,  "protected"),
    (Public,               false, true,  "public"),
    (Static,               false, true,  "static"),
  }
}
