macro_rules! token_enum {
  ($name:ident { $(($item:ident, $pseudo:tt, $repr:expr),)* }) => {
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

      pub fn symbol(&self) -> &str {
        match self {
          $(
            &$name::$item => $repr,
          )*
        }
      }

      pub fn values() -> std::slice::Iter<'static, $name> {
        static VALUES: &'static [$name] = &[$($name::$item),*];
        VALUES.into_iter()
      }
    }
  }
}

token_enum! {
  Token {
    (Invalid,              true,  ""),
    (ArrowFunctionGlyph,   false, "=>"),
    (Await,                false, "await"),
    (BackQuote,            false, "`"),
    (Break,                false, "break"),
    (Case,                 false, "case"),
    (Catch,                false, "catch"),
    (Class,                false, "class"),
    (Colon,                false, ":"),
    (Comma,                false, ","),
    (Const,                false, "const"),
    (Continue,             false, "continue"),
    (Debugger,             false, "debugger"),
    (Default,              false, "default"),
    (Delete,               false, "delete"),
    (Do,                   false, "do"),
    (Dot,                  false, "."),
    (Else,                 false, "else"),
    (Enum,                 false, "enum"),
    (Export,               false, "export"),
    (Extends,              false, "extends"),
    (False,                false, "false"),
    (Finally,              false, "finally"),
    (For,                  false, "for"),
    (Function,             false, "function"),
    (If,                   false, "if"),
    (Import,               false, "import"),
    (In,                   false, "in"),
    (Instanceof,           false, "instanceof"),
    (LeftBrace,            false, "{"),
    (LeftBracket,          false, "["),
    (LeftParen,            false, "("),
    (New,                  false, "new"),
    (Null,                 false, "null"),
    (OpAnd,                false, "&"),
    (OpAndAssign,          false, "&="),
    (OpAssign,             false, "="),
    (OpDecrement,          false, "--"),
    (OpDiv,                false, "/"),
    (OpDivAssign,          false, "/="),
    (OpEq,                 false, "=="),
    (OpGreaterThan,        false, ">"),
    (OpGreaterThanOrEq,    false, ">="),
    (OpIncrement,          false, "++"),
    (OpLessThan,           false, "<"),
    (OpLessThanOrEq,       false, "<="),
    (OpLogicalAnd,         false, "&&"),
    (OpLogicalOr,          false, "||"),
    (OpMinus,              false, "-"),
    (OpMinusAssign,        false, "-="),
    (OpMod,                false, "%"),
    (OpModAssign,          false, "%="),
    (OpMul,                false, "*"),
    (OpMulAssign,          false, "*="),
    (OpNot,                false, "!"),
    (OpNotEq,              false, "!="),
    (OpNullCoalescing,     false, "??"),
    (OpOptionalChaining,   false, "?."),
    (OpOr,                 false, "|"),
    (OpOrAssign,           false, "|="),
    (OpPlus,               false, "+"),
    (OpPlusAssign,         false, "+="),
    (OpPow,                false, "**"),
    (OpPowAssign,          false, "**="),
    (OpShl,                false, "<<"),
    (OpShlAssign,          false, "<<="),
    (OpShr,                false, ">>"),
    (OpShrAssign,          false, ">>="),
    (OpStrictEq,           false, "==="),
    (OpStrictNotEq,        false, "!=="),
    (OpTilde,              false, "~"),
    (OpUShr,               false, ">>>"),
    (OpUShrAssign,         false, ">>>="),
    (OpXor,                false, "^"),
    (OpXorAssign,          false, "^="),
    (Question,             false, "?"),
    (RegExp,               true,  "$RegExp"),
    (Return,               false, "return"),
    (RightBrace,           false, "}"),
    (RightBracket,         false, "]"),
    (RightParen,           false, ")"),
    (Spread,               false, "..."),
    (Super,                false, "super"),
    (Switch,               false, "switch"),
    (Terminate,            false, ";"),
    (This,                 false, "this"),
    (Throw,                false, "throw"),
    (True,                 false, "true"),
    (Try,                  false, "try"),
    (Typeof,               false, "typeof"),
    (Var,                  false, "var"),
    (Void,                 false, "void"),
    (While,                false, "while"),
    (With,                 false, "with"),
    (Yield,                false, "yield"),
    (Identifier,           true,  "$Identifier"),
    (NumericLiteral,       true,  "$NumericLiteral"),
    (ImplicitOctalLiteral, true,  "$ImplicitOctalLiteral"),
    (StringLiteral,        true,  "$StringLiteral"),
    (Template,             true,  "$Template"),
    (TemplateParts,        true,  "$TemplateParts"),
    (TemplateSubstitution, true,  "$TemplateSubstitution"),
    (End,                  true,  "$End"),
  }
}
