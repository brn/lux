# Identifier

`IdentifierStart :: \ UnicodeEscapeSequence`  
- It is a Syntax Error if the SV of UnicodeEscapeSequence is none of "$", or "_", or ! UTF16EncodeCodePoint(cp) for some Unicode code point cp matched by the UnicodeIDStart lexical grammar production.

`IdentifierPart :: \ UnicodeEscapeSequence`  
- It is a Syntax Error if the SV of UnicodeEscapeSequence is none of "$", "_", ! UTF16EncodeCodePoint(<ZWNJ>), ! UTF16EncodeCodePoint(<ZWJ>), or ! UTF16EncodeCodePoint(cp) for some Unicode code point cp that would be matched by the UnicodeIDContinue lexical grammar production.

# BindingIdentifier

`BindingIdentifier : Identifier`
- It is a Syntax Error if the code matched by this production is contained in strict mode code and the StringValue of Identifier is "arguments" or "eval".

`IdentifierReference : yield`  
`BindingIdentifier : yield`  
`LabelIdentifier : yield`  
- It is a Syntax Error if the code matched by this production is contained in strict mode code.

# PropertyDefinition

`PropertyDefinition : MethodDefinition`  
- It is a Syntax Error if HasDirectSuper of MethodDefinition is true.

In addition to describing an actual object initializer the ObjectLiteral productions are also used as a cover grammar for ObjectAssignmentPattern and may be recognized as part of a CoverParenthesizedExpressionAndArrowParameterList. When ObjectLiteral appears in a context where ObjectAssignmentPattern is required the following Early Error rules are not applied. In addition, they are not applied when initially parsing a CoverParenthesizedExpressionAndArrowParameterList or CoverCallExpressionAndAsyncArrowHead.

`PropertyDefinition : CoverInitializedName`  
- Always throw a Syntax Error if code matches this production.

# PrimaryExpression
`PrimaryExpression : RegularExpressionLiteral`  
- It is a Syntax Error if IsValidRegularExpressionLiteral(RegularExpressionLiteral) is false.

`PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList`  
- It is a Syntax Error if CoverParenthesizedExpressionAndArrowParameterList is not covering a ParenthesizedExpression.
- All Early Error rules for ParenthesizedExpression and its derived productions also apply to CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.

# OptionalChain
```
OptionalChain :
?. TemplateLiteral
OptionalChain TemplateLiteral
```  
- It is a Syntax Error if any code matches this production.

### note
```
This production exists in order to prevent automatic semicolon insertion rules (12.9) from being applied to the following code:

a?.b
`c`
so that it would be interpreted as two valid statements. The purpose is to maintain consistency with similar code without optional chaining:

a.b
`c`
which is a valid statement and where automatic semicolon insertion does not apply.
```

```
ImportMeta :
import . meta
```  
- It is a Syntax Error if the syntactic goal symbol is not Module.

# UpdateExpression

```
UpdateExpression :
LeftHandSideExpression ++
LeftHandSideExpression --
```  
- It is an early Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.

```
UpdateExpression :
++ UnaryExpression
-- UnaryExpression
```  
- It is an early Syntax Error if AssignmentTargetType of UnaryExpression is not simple.

# UnaryExpression
```
UnaryExpression : delete UnaryExpression
```  
- It is a Syntax Error if the UnaryExpression is contained in strict mode code and the derived UnaryExpression is PrimaryExpression : IdentifierReference .
- It is a Syntax Error if the derived UnaryExpression is `PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList` and CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in place of UnaryExpression, would produce a Syntax Error according to these rules. This rule is recursively applied.

```
The last rule means that expressions such as delete (((foo))) produce early errors because of recursive application of the first rule.
```
# AssignmentExpression

`AssignmentExpression : LeftHandSideExpression = AssignmentExpression`  

### If LeftHandSideExpression is an ObjectLiteral or an ArrayLiteral, the following Early Error rules are applied:
- It is a Syntax Error if LeftHandSideExpression is not covering an AssignmentPattern.
- All Early Error rules for AssignmentPattern and its derived productions also apply to the AssignmentPattern that is covered by LeftHandSideExpression.

### If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, the following Early Error rule is applied:
- It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.

```
AssignmentExpression :
LeftHandSideExpression AssignmentOperator AssignmentExpression
LeftHandSideExpression &&= AssignmentExpression
LeftHandSideExpression ||= AssignmentExpression
LeftHandSideExpression ??= AssignmentExpression
```  
- It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.

# AssignmentProperty
`AssignmentProperty : IdentifierReference Initializeropt`  
- It is a Syntax Error if AssignmentTargetType of IdentifierReference is not simple.

`AssignmentRestProperty : ... DestructuringAssignmentTarget`
- It is a Syntax Error if DestructuringAssignmentTarget is an ArrayLiteral or an ObjectLiteral.

`DestructuringAssignmentTarget : LeftHandSideExpression`

### If LeftHandSideExpression is an ObjectLiteral or an ArrayLiteral, the following Early Error rules are applied:
- It is a Syntax Error if LeftHandSideExpression is not covering an AssignmentPattern.
- All Early Error rules for AssignmentPattern and its derived productions also apply to the AssignmentPattern that is covered by LeftHandSideExpression.

### If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, the following Early Error rule is applied:
- It is a Syntax Error if AssignmentTargetType of LeftHandSideExpression is not simple.

# TemplateLiteral

`TemplateLiteral[Yield, Await, Tagged] : NoSubstitutionTemplate`  
- It is a Syntax Error if the [Tagged] parameter was not set and NoSubstitutionTemplate Contains NotEscapeSequence.

`TemplateLiteral[Yield, Await, Tagged] : SubstitutionTemplate[?Yield, ?Await, ?Tagged]`  
- It is a Syntax Error if the number of elements in the result of TemplateStrings of TemplateLiteral with argument false is greater than 232 - 1.

`SubstitutionTemplate[Yield, Await, Tagged] : TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]`  
- It is a Syntax Error if the [Tagged] parameter was not set and TemplateHead Contains NotEscapeSequence.

`TemplateSpans[Yield, Await, Tagged] : TemplateTail`  
- It is a Syntax Error if the [Tagged] parameter was not set and TemplateTail Contains NotEscapeSequence.

```
TemplateMiddleList[Yield, Await, Tagged] :
TemplateMiddle Expression[+In, ?Yield, ?Await]
TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
```  
- It is a Syntax Error if the [Tagged] parameter was not set and TemplateMiddle Contains NotEscapeSequence.


# RegularExpression

## RegularExpressionFlags :: RegularExpressionFlags IdentifierPart
- It is a Syntax Error if IdentifierPart contains a Unicode escape sequence.


# Block

`Block : { StatementList }`  
- It is a Syntax Error if the LexicallyDeclaredNames of StatementList contains any duplicate entries.
- It is a Syntax Error if any element of the LexicallyDeclaredNames of StatementList also occurs in the VarDeclaredNames of StatementList.

# LexicalDeclaration

`LexicalDeclaration : LetOrConst BindingList ;`  
- It is a Syntax Error if the BoundNames of BindingList contains "let".
- It is a Syntax Error if the BoundNames of BindingList contains any duplicate entries.

`LexicalBinding : BindingIdentifier Initializeropt`  
- It is a Syntax Error if Initializer is not present and IsConstantDeclaration of the LexicalDeclaration containing this LexicalBinding is true.

# IfStatement

```
IfStatement :
if ( Expression ) Statement else Statement
if ( Expression ) Statement
```  
- It is a Syntax Error if IsLabelledFunction(Statement) is true.

# DoWhileStatement

`DoWhileStatement : do Statement while ( Expression ) ;`  
- It is a Syntax Error if IsLabelledFunction(Statement) is true.

### note
```
It is only necessary to apply this rule if the extension specified in B.3.2 is implemented.
```

# WhileStatement

`WhileStatement : while ( Expression ) Statement`  
- It is a Syntax Error if IsLabelledFunction(Statement) is true.

# ForStatement
```
ForStatement :
for ( Expressionopt ; Expressionopt ; Expressionopt ) Statement
for ( var VariableDeclarationList ; Expressionopt ; Expressionopt ) Statement
for ( LexicalDeclaration Expressionopt ; Expressionopt ) Statement
```  

- It is a Syntax Error if IsLabelledFunction(Statement) is true.

### note

```
It is only necessary to apply this rule if the extension specified in B.3.2 is implemented.
```

# ForInOfStatement

```
ForInOfStatement :
for ( LeftHandSideExpression in Expression ) Statement
for ( var ForBinding in Expression ) Statement
for ( ForDeclaration in Expression ) Statement
for ( LeftHandSideExpression of AssignmentExpression ) Statement
for ( var ForBinding of AssignmentExpression ) Statement
for ( ForDeclaration of AssignmentExpression ) Statement
for await ( LeftHandSideExpression of AssignmentExpression ) Statement
for await ( var ForBinding of AssignmentExpression ) Statement
for await ( ForDeclaration of AssignmentExpression ) Statement
```  
- It is a Syntax Error if IsLabelledFunction(Statement) is true.

### note

```
It is only necessary to apply this rule if the extension specified in B.3.2 is implemented.
```

# ContinueStatement

```
ContinueStatement :
continue ;
continue LabelIdentifier ;
```

- It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing function boundaries), within an IterationStatement.

# BreakStatement

`BreakStatement : break ;`  

- It is a Syntax Error if this BreakStatement is not nested, directly or indirectly (but not crossing function boundaries), within an IterationStatement or a SwitchStatement.

# WithStatement

`WithStatement : with ( Expression ) Statement`  
- It is a Syntax Error if the code that matches this production is contained in strict mode code.
- It is a Syntax Error if IsLabelledFunction(Statement) is true.

### note
```
It is only necessary to apply the second rule if the extension specified in B.3.2 is implemented.
```

# SwitchStatement

`SwitchStatement : switch ( Expression ) CaseBlock`

- It is a Syntax Error if the LexicallyDeclaredNames of CaseBlock contains any duplicate entries.
- It is a Syntax Error if any element of the LexicallyDeclaredNames of CaseBlock also occurs in the VarDeclaredNames of CaseBlock.


# LabelledItem

`LabelledItem : FunctionDeclaration`

- It is a Syntax Error if any source text matches this rule.

### note

```
An alternative definition for this rule is provided in B.3.2.
```

# Catch

`Catch : catch ( CatchParameter ) Block`

- It is a Syntax Error if BoundNames of CatchParameter contains any duplicate elements.
- It is a Syntax Error if any element of the BoundNames of CatchParameter also occurs in the LexicallyDeclaredNames of Block.
- It is a Syntax Error if any element of the BoundNames of CatchParameter also occurs in the VarDeclaredNames of Block.

### note

```
An alternative static semantics for this production is given in B.3.5.
```

# UniqueFormalParameters

`UniqueFormalParameters : FormalParameters`

- It is a Syntax Error if BoundNames of FormalParameters contains any duplicate elements.

`FormalParameters : FormalParameterList`

- It is a Syntax Error if IsSimpleParameterList of FormalParameterList is false and BoundNames of FormalParameterList contains any duplicate elements.

### note

Multiple occurrences of the same BindingIdentifier in a FormalParameterList is only allowed for functions which have simple parameter lists and which are not defined in [strict mode code](https://262.ecma-international.org/12.0/#sec-strict-mode-code).

# FunctionDeclaration

```
FunctionDeclaration :
function BindingIdentifier ( FormalParameters ) { FunctionBody }
function ( FormalParameters ) { FunctionBody }
FunctionExpression :
function BindingIdentifieropt ( FormalParameters ) { FunctionBody }
```  

- If the source code matching FormalParameters is strict mode code, the Early Error rules for UniqueFormalParameters : FormalParameters are applied.
- If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
- It is a Syntax Error if FunctionBodyContainsUseStrict of FunctionBody is true and IsSimpleParameterList of FormalParameters is false.
- It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of FunctionBody.
- It is a Syntax Error if FormalParameters Contains SuperProperty is true.
- It is a Syntax Error if FunctionBody Contains SuperProperty is true.
- It is a Syntax Error if FormalParameters Contains SuperCall is true.
- It is a Syntax Error if FunctionBody Contains SuperCall is true.

### note


The [LexicallyDeclaredNames](https://262.ecma-international.org/12.0/#sec-static-semantics-lexicallydeclarednames) of a FunctionBody does not include identifiers bound using var or function declarations.

# ArrowFunction

```
ArrowFunction : ArrowParameters => ConciseBody
```

- It is a Syntax Error if ArrowParameters Contains YieldExpression is true.
- It is a Syntax Error if ArrowParameters Contains AwaitExpression is true.
- It is a Syntax Error if ConciseBodyContainsUseStrict of ConciseBody is true and IsSimpleParameterList of ArrowParameters is false.
- It is a Syntax Error if any element of the BoundNames of ArrowParameters also occurs in the LexicallyDeclaredNames of ConciseBody.

`ArrowParameters : CoverParenthesizedExpressionAndArrowParameterList`

- It is a Syntax Error if CoverParenthesizedExpressionAndArrowParameterList is not covering an ArrowFormalParameters.
- All early error rules for ArrowFormalParameters and its derived productions also apply to CoveredFormalsList of CoverParenthesizedExpressionAndArrowParameterList.

# MethodDefinition

`MethodDefinition : PropertyName ( UniqueFormalParameters ) { FunctionBody }`

- It is a Syntax Error if [FunctionBodyContainsUseStrict](https://262.ecma-international.org/12.0/#sec-static-semantics-functionbodycontainsusestrict) of FunctionBody is true and [IsSimpleParameterList](https://262.ecma-international.org/12.0/#sec-static-semantics-issimpleparameterlist) of UniqueFormalParameters is false.
- It is a Syntax Error if any element of the [BoundNames](https://262.ecma-international.org/12.0/#sec-static-semantics-boundnames) of UniqueFormalParameters also occurs in the [LexicallyDeclaredNames](https://262.ecma-international.org/12.0/#sec-static-semantics-lexicallydeclarednames) of FunctionBody.

`MethodDefinition : set PropertyName ( PropertySetParameterList ) { FunctionBody }`

- It is a Syntax Error if [BoundNames](https://262.ecma-international.org/12.0/#sec-static-semantics-boundnames) of PropertySetParameterList contains any duplicate elements.
- It is a Syntax Error if [FunctionBodyContainsUseStrict](https://262.ecma-international.org/12.0/#sec-static-semantics-functionbodycontainsusestrict) of FunctionBody is true and [IsSimpleParameterList](https://262.ecma-international.org/12.0/#sec-static-semantics-issimpleparameterlist) of PropertySetParameterList is false.
- It is a Syntax Error if any element of the [BoundNames](https://262.ecma-international.org/12.0/#sec-static-semantics-boundnames) of PropertySetParameterList also occurs in the [LexicallyDeclaredNames](https://262.ecma-international.org/12.0/#sec-static-semantics-lexicallydeclarednames) of FunctionBody.


# GeneratorMethod

`GeneratorMethod : * PropertyName ( UniqueFormalParameters ) { GeneratorBody }`

- It is a Syntax Error if HasDirectSuper of GeneratorMethod is true.
- It is a Syntax Error if UniqueFormalParameters Contains YieldExpression is true.
- It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList of UniqueFormalParameters is false.
- It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the LexicallyDeclaredNames of GeneratorBody.

```
GeneratorDeclaration :
function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
function * ( FormalParameters ) { GeneratorBody }
GeneratorExpression :
function * BindingIdentifieropt ( FormalParameters ) { GeneratorBody }
```

- If the source code matching FormalParameters is strict mode code, the Early Error rules for UniqueFormalParameters : FormalParameters are applied.
- If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
- It is a Syntax Error if FunctionBodyContainsUseStrict of GeneratorBody is true and IsSimpleParameterList of FormalParameters is false.
- It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of GeneratorBody.
- It is a Syntax Error if FormalParameters Contains YieldExpression is true.
- It is a Syntax Error if FormalParameters Contains SuperProperty is true.
- It is a Syntax Error if GeneratorBody Contains SuperProperty is true.
- It is a Syntax Error if FormalParameters Contains SuperCall is true.
- It is a Syntax Error if GeneratorBody Contains SuperCall is true.

# AsyncGeneratorMethod

```
AsyncGeneratorMethod : async * PropertyName ( UniqueFormalParameters ) { AsyncGeneratorBody }
```

- It is a Syntax Error if HasDirectSuper of AsyncGeneratorMethod is true.
- It is a Syntax Error if UniqueFormalParameters Contains YieldExpression is true.
- It is a Syntax Error if UniqueFormalParameters Contains AwaitExpression is true.
- It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncGeneratorBody is true and IsSimpleParameterList of UniqueFormalParameters is false.
- It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the LexicallyDeclaredNames of AsyncGeneratorBody.

```
AsyncGeneratorDeclaration :
async function * BindingIdentifier ( FormalParameters ) { AsyncGeneratorBody }
async function * ( FormalParameters ) { AsyncGeneratorBody }
```

```
AsyncGeneratorExpression :
async function * BindingIdentifieropt ( FormalParameters ) { AsyncGeneratorBody }
```

- If the source code matching FormalParameters is strict mode code, the Early Error rules for UniqueFormalParameters : FormalParameters are applied.
- If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
- It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncGeneratorBody is true and IsSimpleParameterList of FormalParameters is false.
- It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of AsyncGeneratorBody.
- It is a Syntax Error if FormalParameters Contains YieldExpression is true.
- It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
- It is a Syntax Error if FormalParameters Contains SuperProperty is true.
- It is a Syntax Error if AsyncGeneratorBody Contains SuperProperty is true.
- It is a Syntax Error if FormalParameters Contains SuperCall is true.
- It is a Syntax Error if AsyncGeneratorBody Contains SuperCall is true.

# ClassTail

`ClassTail : ClassHeritageopt { ClassBody }`

### It is a Syntax Error if ClassHeritage is not present and the following algorithm evaluates to true:

1. Let constructor be ConstructorMethod of ClassBody.
2. If constructor is empty, return false.
3. Return HasDirectSuper of constructor.

`ClassBody : ClassElementList`

- It is a Syntax Error if PrototypePropertyNameList of ClassElementList contains more than one occurrence of "constructor".

`ClassElement : MethodDefinition`

- It is a Syntax Error if PropName of MethodDefinition is not "constructor" and HasDirectSuper of MethodDefinition is true.
- It is a Syntax Error if PropName of MethodDefinition is "constructor" and SpecialMethod of MethodDefinition is true.

`ClassElement : static MethodDefinition`

- It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
- It is a Syntax Error if PropName of MethodDefinition is "prototype".

# AsyncMethod

```
AsyncMethod : async PropertyName ( UniqueFormalParameters ) { AsyncFunctionBody }
```

- It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncFunctionBody is true and IsSimpleParameterList of UniqueFormalParameters is false.
- It is a Syntax Error if HasDirectSuper of AsyncMethod is true.
- It is a Syntax Error if UniqueFormalParameters Contains AwaitExpression is true.
- It is a Syntax Error if any element of the BoundNames of UniqueFormalParameters also occurs in the LexicallyDeclaredNames of AsyncFunctionBody.

```
AsyncFunctionDeclaration :
async function BindingIdentifier ( FormalParameters ) { AsyncFunctionBody }
async function ( FormalParameters ) { AsyncFunctionBody }
AsyncFunctionExpression :
async function BindingIdentifieropt ( FormalParameters ) { AsyncFunctionBody }
```

- It is a Syntax Error if FunctionBodyContainsUseStrict of AsyncFunctionBody is true and IsSimpleParameterList of FormalParameters is false.
- It is a Syntax Error if FormalParameters Contains AwaitExpression is true.
- If the source code matching FormalParameters is strict mode code, the Early Error rules for UniqueFormalParameters : FormalParameters are applied.
- If BindingIdentifier is present and the source code matching BindingIdentifier is strict mode code, it is a Syntax Error if the StringValue of BindingIdentifier is "eval" or "arguments".
- It is a Syntax Error if any element of the BoundNames of FormalParameters also occurs in the LexicallyDeclaredNames of AsyncFunctionBody.
- It is a Syntax Error if FormalParameters Contains SuperProperty is true.
- It is a Syntax Error if AsyncFunctionBody Contains SuperProperty is true.
- It is a Syntax Error if FormalParameters Contains SuperCall is true.
- It is a Syntax Error if AsyncFunctionBody Contains SuperCall is true.

# AsyncArrowFunction

`AsyncArrowFunction : async AsyncArrowBindingIdentifier => AsyncConciseBody`

- It is a Syntax Error if any element of the BoundNames of AsyncArrowBindingIdentifier also occurs in the LexicallyDeclaredNames of AsyncConciseBody.

`AsyncArrowFunction : CoverCallExpressionAndAsyncArrowHead => AsyncConciseBody`

- It is a Syntax Error if CoverCallExpressionAndAsyncArrowHead Contains YieldExpression is true.
- It is a Syntax Error if CoverCallExpressionAndAsyncArrowHead Contains AwaitExpression is true.
- It is a Syntax Error if CoverCallExpressionAndAsyncArrowHead is not covering an AsyncArrowHead.
- It is a Syntax Error if any element of the BoundNames of CoverCallExpressionAndAsyncArrowHead also occurs in the LexicallyDeclaredNames of AsyncConciseBody.
- It is a Syntax Error if AsyncConciseBodyContainsUseStrict of AsyncConciseBody is true and IsSimpleParameterList of CoverCallExpressionAndAsyncArrowHead is false.
- All Early Error rules for AsyncArrowHead and its derived productions apply to CoveredAsyncArrowHead of CoverCallExpressionAndAsyncArrowHead.

# Script

`Script : ScriptBody`

- It is a Syntax Error if the LexicallyDeclaredNames of ScriptBody contains any duplicate entries.
- It is a Syntax Error if any element of the LexicallyDeclaredNames of ScriptBody also occurs in the VarDeclaredNames of ScriptBody.

`ScriptBody : StatementList`

- It is a Syntax Error if StatementList Contains super unless the source code containing super is eval code that is being processed by a direct eval. Additional early error rules for super within direct eval are defined in 19.2.1.1.
- It is a Syntax Error if StatementList Contains NewTarget unless the source code containing NewTarget is eval code that is being processed by a direct eval. Additional early error rules for NewTarget in direct eval are defined in 19.2.1.1.
- It is a Syntax Error if ContainsDuplicateLabels of StatementList with argument « » is true.
- It is a Syntax Error if ContainsUndefinedBreakTarget of StatementList with argument « » is true.
- It is a Syntax Error if ContainsUndefinedContinueTarget of StatementList with arguments « » and « » is true.

# Module

`ModuleBody : ModuleItemList`

- It is a Syntax Error if the LexicallyDeclaredNames of ModuleItemList contains any duplicate entries.
- It is a Syntax Error if any element of the LexicallyDeclaredNames of ModuleItemList also occurs in the VarDeclaredNames of ModuleItemList.
- It is a Syntax Error if the ExportedNames of ModuleItemList contains any duplicate entries.
- It is a Syntax Error if any element of the ExportedBindings of ModuleItemList does not also occur in either the VarDeclaredNames of ModuleItemList, or the LexicallyDeclaredNames of ModuleItemList.
- It is a Syntax Error if ModuleItemList Contains super.
- It is a Syntax Error if ModuleItemList Contains NewTarget.
- It is a Syntax Error if ContainsDuplicateLabels of ModuleItemList with argument « » is true.
- It is a Syntax Error if ContainsUndefinedBreakTarget of ModuleItemList with argument « » is true.
- It is a Syntax Error if ContainsUndefinedContinueTarget of ModuleItemList with arguments « » and « » is true.

### note

The duplicate ExportedNames rule implies that multiple export default ExportDeclaration items within a ModuleBody is a Syntax Error. Additional error conditions relating to conflicting or duplicate declarations are checked during module linking prior to evaluation of a Module. If any such errors are detected the Module is not evaluated.

# ModuleItem

```
ModuleItem : ImportDeclaration
```

- It is a Syntax Error if the BoundNames of ImportDeclaration contains any duplicate entries.

# ExportDeclaration

```
ExportDeclaration : export NamedExports ;
```

- For each IdentifierName n in ReferencedBindings of NamedExports: It is a Syntax Error if StringValue of n is a ReservedWord or if the StringValue of n is one of: "implements", "interface", "let", "package", "private", "protected", "public", or "static".


### note

The above rule means that each ReferencedBindings of NamedExports is treated as an IdentifierReference.


# RegExpParsing

`Pattern :: Disjunction`

- It is a Syntax Error if NcapturingParens ≥ 232 - 1.
- It is a Syntax Error if Pattern contains multiple GroupSpecifiers whose enclosed RegExpIdentifierNames have the same CapturingGroupName.

`QuantifierPrefix :: { DecimalDigits , DecimalDigits }`

- It is a Syntax Error if the MV of the first DecimalDigits is larger than the MV of the second DecimalDigits.

`AtomEscape :: k GroupName`

- It is a Syntax Error if the enclosing Pattern does not contain a GroupSpecifier with an enclosed RegExpIdentifierName whose CapturingGroupName equals the CapturingGroupName of the RegExpIdentifierName of this production's GroupName.

`AtomEscape :: DecimalEscape`

- It is a Syntax Error if the CapturingGroupNumber of DecimalEscape is larger than NcapturingParens (22.2.2.1).

`NonemptyClassRanges :: ClassAtom - ClassAtom ClassRanges`

- It is a Syntax Error if IsCharacterClass of the first ClassAtom is true or IsCharacterClass of the second ClassAtom is true.
- It is a Syntax Error if IsCharacterClass of the first ClassAtom is false and IsCharacterClass of the second ClassAtom is false and the CharacterValue of the first ClassAtom is larger than the CharacterValue of the second ClassAtom.

`NonemptyClassRangesNoDash :: ClassAtomNoDash - ClassAtom ClassRanges`

- It is a Syntax Error if IsCharacterClass of ClassAtomNoDash is true or IsCharacterClass of ClassAtom is true.
- It is a Syntax Error if IsCharacterClass of ClassAtomNoDash is false and IsCharacterClass of ClassAtom is false and the CharacterValue of ClassAtomNoDash is larger than the CharacterValue of ClassAtom.

`RegExpIdentifierStart[U] :: \ RegExpUnicodeEscapeSequence[+U]`

- It is a Syntax Error if the CharacterValue of RegExpUnicodeEscapeSequence is not the code point value of "$", "_", or some code point matched by the UnicodeIDStart lexical grammar production.

`RegExpIdentifierStart[U] :: UnicodeLeadSurrogate UnicodeTrailSurrogate`

- It is a Syntax Error if the result of performing UTF16SurrogatePairToCodePoint on the two code points matched by UnicodeLeadSurrogate and UnicodeTrailSurrogate respectively is not matched by the UnicodeIDStart lexical grammar production.

`RegExpIdentifierPart[U] :: \ RegExpUnicodeEscapeSequence[+U]`

- It is a Syntax Error if the CharacterValue of RegExpUnicodeEscapeSequence is not the code point value of "$", "_", <ZWNJ>, <ZWJ>, or some code point matched by the UnicodeIDContinue lexical grammar production.

`RegExpIdentifierPart[U] :: UnicodeLeadSurrogate UnicodeTrailSurrogate`

- It is a Syntax Error if the result of performing UTF16SurrogatePairToCodePoint on the two code points matched by UnicodeLeadSurrogate and UnicodeTrailSurrogate respectively is not matched by the UnicodeIDContinue lexical grammar production.

`UnicodePropertyValueExpression :: UnicodePropertyName = UnicodePropertyValue`

- It is a Syntax Error if the List of Unicode code points that is SourceText of UnicodePropertyName is not identical to a List of Unicode code points that is a Unicode property name or property alias listed in the “Property name and aliases” column of Table 56.
- It is a Syntax Error if the List of Unicode code points that is SourceText of UnicodePropertyValue is not identical to a List of Unicode code points that is a value or value alias for the Unicode property or property alias given by SourceText of UnicodePropertyName listed in the “Property value and aliases” column of the corresponding tables Table 58 or Table 59.
UnicodePropertyValueExpression :: LoneUnicodePropertyNameOrValue
- It is a Syntax Error if the List of Unicode code points that is SourceText of LoneUnicodePropertyNameOrValue is not identical to a List of Unicode code points that is a Unicode general category or general category alias listed in the “Property value and aliases” column of Table 58, nor a binary property or binary property alias listed in the “Property name and aliases” column of Table 57.

`ExtendedAtom :: InvalidBracedQuantifier`

- It is a Syntax Error if any source text matches this rule.
- Additionally, the rules for the following productions are modified with the addition of the highlighted text:

`NonemptyClassRanges :: ClassAtom - ClassAtom ClassRanges`

- It is a Syntax Error if IsCharacterClass of the first ClassAtom is true or IsCharacterClass of the second ClassAtom is true and this production has a [U] parameter.
- It is a Syntax Error if IsCharacterClass of the first ClassAtom is false and IsCharacterClass of the second ClassAtom is false and the CharacterValue of the first ClassAtom is larger than the CharacterValue of the second ClassAtom.

`NonemptyClassRangesNoDash :: ClassAtomNoDash - ClassAtom ClassRanges`

- It is a Syntax Error if IsCharacterClass of ClassAtomNoDash is true or IsCharacterClass of ClassAtom is true and this production has a [U] parameter.
- It is a Syntax Error if IsCharacterClass of ClassAtomNoDash is false and IsCharacterClass of ClassAtom is false and the CharacterValue of ClassAtomNoDash is larger than the CharacterValue of ClassAtom.
