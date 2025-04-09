{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | This module implements parsing of C23 expressions and operators.
--
-- An expression in C can be either:
--
--   * An operator applied to operands
--   * A primary expression (identifier, literal, string literal,
--     parenthesized expression, or generic selection)
--
-- Implementation note:
--
-- Rather than directly translating the C grammar (which would produce
-- a deep and unbalanced tree), this implementation constructs the AST
-- directly without building an intermediate CST (Concrete Syntax Tree).
module Language.NC.Internal.Parse.Op (
  -- * Parsers
  expr,
  expr_,
  assign,
) where

import Language.NC.Internal.Lex
import Language.NC.Internal.Parse.Type
import Language.NC.Internal.Prelude hiding (assign, shift)

{- PEG grammar referenced; converted from C23 CFG grammar.
# Expressions
expr <- assign ("," assign)*
assign <- unary ("=" / "+=" / "-=" / "*=" / "/=" / "%=" / "<<=" / ">>=" / "|=" / "^=" / "&=") assign / cond
cond <- logor ("?" expr ":" assign)?
logor <- logand ("||" logand)*
logand <- bitor ("&&" bitor)*
bitor <- bitxor ("|" bitxor)*
bitxor <- bitand ("^" bitand)*
bitand <- rel ("&" rel)*
rel <- shift (("<" / ">" / "<=" / ">=" / "==" / "!=") shift)*
shift <- add (("<<" / ">>") add)*
add <- mul (("+" / "-") mul)*
mul <- cast (("*" / "/" / "%") cast)*
cast <- unary / "(" type ")" cast
unary <- postfix / ("++" / "--") unary / ("&" / "*" / "+" / "-" / "~" / "!") cast / "sizeof" (unary / "(" type ")") / "alignof" "(" type ")"
-}

-- | Parse an expression and additionally return the span.
expr :: Parser (WithSpan Expr)
expr = runandgetspan expr_

-- | Parse an expression without returning the span.
expr_ :: Parser Expr
expr_ = chainl ExprComma assign (comma >> assign)

-- | Parse an assignment expression.
assign :: Parser Expr
assign = assignop <|> cond
 where
  assignop = do
    a <- unary
    operator <- binasgnop
    b <- assign
    pure $ operator a b

cond = do
  a <- logor
  option a do
    questionmark
    b <- expr_
    colon
    c <- assign
    pure $ ExprConditional a b c

logor = do
  a <- logand
  go a
 where
  go a = option a do
    dbbar
    b <- logand
    go (ExprOr a b)

logand = do
  a <- bitor
  go a
 where
  go a = option a do
    dbamp
    b <- bitor
    go (ExprAnd a b)

bitor = do
  a <- bitxor
  go a
 where
  go a = option a do
    bar
    b <- bitxor
    go (ExprBitOr a b)

bitxor = do
  a <- bitand
  go a
 where
  go a = option a do
    caret
    b <- bitand
    go (ExprBitXor a b)

bitand = do
  a <- rel
  go a
 where
  go a = option a do
    ampersand
    b <- rel
    go (ExprBitAnd a b)

rel = do
  a <- shift
  go a
 where
  go a = option a do
    r <- relop
    b <- shift
    go (r a b)
  relop =
    $( switch_ws0
         [|
           case _ of
             "<" -> pure ExprLT
             ">" -> pure ExprGT
             "<=" -> pure ExprLE
             ">=" -> pure ExprGE
             "==" -> pure ExprEQ
             "!=" -> pure ExprNE
           |]
     )

shift = do
  a <- add
  go a
 where
  go a = option a do
    s <- shiftop
    b <- add
    go (s a b)
  shiftop =
    $( switch_ws0
         [|
           case _ of
             "<<" -> pure ExprShiftL
             ">>" -> pure ExprShiftR
           |]
     )

add = do
  a <- mul
  go a
 where
  go a = option a do
    s <- addop
    b <- mul
    go (s a b)
  addop =
    $( switch_ws0
         [|
           case _ of
             "+" -> pure ExprPlus
             "-" -> pure ExprMinus
           |]
     )

mul = do
  a <- cast_
  go a
 where
  go a = option a do
    s <- mulop
    b <- cast_
    go (s a b)
  mulop =
    $( switch_ws0
         [|
           case _ of
             "*" -> pure ExprTimes
             "/" -> pure ExprDiv
             "%" -> pure ExprMod
           |]
     )

cast_ =
  -- can't really use branch_inpar because it needs to be followed up by cast_.
  branch
    $(char '(')
    (skipBack 1 >> ws0 >> ExprCast <$> inpar typename <*> cast_)
    unary

unary =
  $( switch_ws0
       [|
         case _ of
           "++" -> ExprPreInc <$> unary
           "--" -> ExprPreDec <$> unary
           "&" -> ExprAddrOf <$> cast_
           "*" -> ExprDeref <$> cast_
           "+" -> ExprUnaryPlus <$> cast_
           "-" -> ExprUnaryMinus <$> cast_
           "~" -> ExprBitNot <$> cast_
           "!" -> ExprNot <$> cast_
           _ ->
             $( switch_ws1
                  [|
                    case _ of
                      "sizeof" -> sizeof
                      "alignof" -> alignof
                      "_Alignof" -> alignof
                      _ -> postfix
                    |]
              )
         |]
   )
 where
  sizeof = ExprSizeOf <$> branch_inpar (Left <$> typename) (Right <$> unary)
  alignof = ExprAlignOf . Left <$> inpar typename

compound = do
  -- For now, just handle the error since we need to refactor
  -- the Expr and Type handling in the AST to properly implement
  -- compound literals
  inpar typename >> incur anyChar >> failed

postfix = primary >>= go
 where
  go a = do
    let getid = identifier
        mksym i = do
          s <- newsymbol
          symgivename s i $> s
        onsqb = do
          b <- expr_
          rsqb $> ExprArray a b
        onpar = do
          b <- assign `sepBy` comma
          rpar $> ExprCall a b
        sfxop =
          $( switch_ws0
               [|
                 case _ of
                   "[" -> onsqb
                   "<:" -> onsqb
                   "(" -> onpar
                   "." -> ExprMember a <$> (getid >>= mksym)
                   "->" -> ExprMember a <$> (getid >>= mksym) -- FIXME
                   "--" -> pure $ ExprPostDec a
                   "++" -> pure $ ExprPostInc a
                   _ -> compound
                 |]
           )
    result <- option a sfxop
    if result == a
      then pure result
      else go result -- Recursively parse more postfix operators

primary = branch _Generic' generic $ branch_inpar paren (ident <|> litexpr)
 where
  generic =
    inpar do
      a <- assign <* cut comma (ExprParseError (MissingSeparator "comma"))
      b <- genassoc `sepBy1` comma
      pure $ ExprGeneric a b
   where
    genassoc = do
      let tynam = branch default' (pure Nothing) $ Just <$> typename
      tt <- cut tynam (ExprParseError MalformedGenericExpression)
      cut colon (ExprParseError MalformedGenericExpression)
      GenAssoc tt <$> assign
  paren = Expr <$> runandgetspan (PrimParen <$> cut_expr_)
   where
    cut_expr_ = cut expr_ (ExprParseError ExpectedExpression)
  ident = withSpan cut_identifier \i s -> pure $ Expr $ WithSpan s $ PrimId i
   where
    cut_identifier = cut identifier (ExprParseError ExpectedIdentifier)
  litexpr = Expr <$> runandgetspan (PrimLit <$> cut_literal)
   where
    cut_literal = cut literal (ExprParseError ExpectedLiteral)

binasgnop :: Parser (Expr -> Expr -> Expr)
binasgnop =
  $( switch_ws0
       [|
         case _ of
           "=" -> pure ExprAssign
           "+=" -> pure ExprAssignPlus
           "-=" -> pure ExprAssignMinus
           "*=" -> pure ExprAssignTimes
           "/=" -> pure ExprAssignDiv
           "%=" -> pure ExprAssignMod
           "<<=" -> pure ExprAssignShiftL
           ">>=" -> pure ExprAssignShiftR
           "&=" -> pure ExprAssignBitAnd
           "^=" -> pure ExprAssignBitXor
           "|=" -> pure ExprAssignBitOr
         |]
   )
