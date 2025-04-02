{-# OPTIONS_GHC -Wno-missing-signatures #-}

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
module Language.NC.Internal.Lex.Op (
  -- * Types
  Expr (..),
  PrimExpr (..),
  GenAssoc (..),

  -- * Parsers
  expr,
  expr_,
) where

import Language.NC.Internal.Lex.Lex
import {-# SOURCE #-} Language.NC.Internal.Lex.Type
import Language.NC.Internal.Prelude hiding (assign, shift)

-- | Represents a primary expression, which is the most basic form
-- of expression in C.
data PrimExpr
  = -- | An identifier
    PrimId Str
  | -- | A literal value (excluding string literals)
    PrimLit Lit
  | -- | A string literal
    PrimStrLit Str
  | -- | An expression in parentheses. Don't count on the
    -- parentheses being included in the span (or not being
    -- included in the span).
    PrimParen Expr
  | -- | A generic selection expression
    PrimGeneric Expr GenAssoc
  deriving (Eq, Show, Ord)

-- | A @Generic\_@ selection expression.
data GenAssoc
  = -- | generic selection
    GenAssoc (Maybe Str) Expr
  deriving (Eq, Show, Ord)

-- | According to the C standard, an expression is an operator bound
-- to operands, or a primary expression.
data Expr
  = -- | primary expression
    Expr (WithSpan PrimExpr)
  | -- | postfix increment (++)
    ExprPostInc Expr
  | -- | postfix decrement (--)
    ExprPostDec Expr
  | -- | function call
    ExprCall Expr [Expr]
  | -- | array subscript
    ExprArray Expr Expr
  | -- | member access using dot (.)
    ExprMember Expr Symbol
  | -- | member access using arrow (->)
    ExprMemberPtr Expr Symbol
  | -- | compound literal, type and value
    ExprCompoundLiteral Expr Expr
  | -- | generic selection
    ExprGeneric Expr [GenAssoc]
  | -- | prefix increment (++)
    ExprPreInc Expr
  | -- | prefix decrement (--)
    ExprPreDec Expr
  | -- | unary plus (+)
    ExprUnaryPlus Expr
  | -- | unary minus (-)
    ExprUnaryMinus Expr
  | -- | logical not (!)
    ExprNot Expr
  | -- | bitwise not (~)
    ExprBitNot Expr
  | -- | cast, type and value
    ExprCast Type Expr
  | -- | dereference using (*)
    ExprDeref Expr
  | -- | address of using (&)
    ExprAddrOf Expr
  | -- | sizeof operator, either type or value
    ExprSizeOf (Either Type Expr)
  | -- | alignof operator, either type or value
    ExprAlignOf (Either Type Expr)
  | -- | multiplication
    ExprTimes Expr Expr
  | -- | division
    ExprDiv Expr Expr
  | -- | modulus
    ExprMod Expr Expr
  | -- | addition
    ExprPlus Expr Expr
  | -- | subtraction
    ExprMinus Expr Expr
  | -- | left shift
    ExprShiftL Expr Expr
  | -- | right shift
    ExprShiftR Expr Expr
  | -- | less than
    ExprLT Expr Expr
  | -- | greater than
    ExprGT Expr Expr
  | -- | less than or equal to
    ExprLE Expr Expr
  | -- | greater than or equal to
    ExprGE Expr Expr
  | -- | equal to
    ExprEQ Expr Expr
  | -- | not equal to
    ExprNE Expr Expr
  | -- | bitwise and
    ExprBitAnd Expr Expr
  | -- | bitwise xor
    ExprBitXor Expr Expr
  | -- | bitwise or
    ExprBitOr Expr Expr
  | -- | logical and
    ExprAnd Expr Expr
  | -- | logical or
    ExprOr Expr Expr
  | -- | conditional operator (?:)
    ExprConditional Expr Expr Expr
  | -- | assignment operator
    ExprAssign Expr Expr
  | -- | assignment operator (+=)
    ExprAssignPlus Expr Expr
  | -- | assignment operator (-=)
    ExprAssignMinus Expr Expr
  | -- | assignment operator (*=)
    ExprAssignTimes Expr Expr
  | -- | assignment operator (/=)
    ExprAssignDiv Expr Expr
  | -- | assignment operator (%=)
    ExprAssignMod Expr Expr
  | -- | assignment operator (<<=)
    ExprAssignShiftL Expr Expr
  | -- | assignment operator (>>=)
    ExprAssignShiftR Expr Expr
  | -- | assignment operator (&=)
    ExprAssignBitAnd Expr Expr
  | -- | assignment operator (^=)
    ExprAssignBitXor Expr Expr
  | -- | assignment operator (|=)
    ExprAssignBitOr Expr Expr
  | -- | comma operator
    ExprComma Expr Expr
  deriving (Eq, Show, Ord)

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
expr_ = chainl ExprComma assign (lx0 comma >> assign)

assign = assignop <|> cond
 where
  assignop = do
    a <- lx0 unary
    operator <- lx0 binasgnop
    b <- lx0 assign
    pure $ operator a b

cond = do
  a <- lx0 logor
  option a do
    lx0 questionmark
    b <- lx0 expr_
    lx0 colon
    c <- assign
    pure $ ExprConditional a b c

logor = do
  a <- lx0 logand
  go a
 where
  go a = option a do
    lx0 doublebar
    b <- lx0 logand
    go (ExprOr a b)

logand = do
  a <- lx0 bitor
  go a
 where
  go a = option a do
    lx0 doubleamp
    b <- lx0 bitor
    go (ExprAnd a b)

bitor = do
  a <- lx0 bitxor
  go a
 where
  go a = option a do
    lx0 bar
    b <- lx0 bitxor
    go (ExprBitOr a b)

bitxor = do
  a <- lx0 bitand
  go a
 where
  go a = option a do
    lx0 caret
    b <- lx0 bitand
    go (ExprBitXor a b)

bitand = do
  a <- lx0 rel
  go a
 where
  go a = option a do
    lx0 ampersand
    b <- lx0 rel
    go (ExprBitAnd a b)

rel = do
  a <- lx0 shift
  go a
 where
  go a = option a do
    r <- relop
    b <- lx0 shift
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
  a <- lx0 add
  go a
 where
  go a = option a do
    s <- shiftop
    b <- lx0 add
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
  a <- lx0 mul
  go a
 where
  go a = option a do
    s <- addop
    b <- lx0 mul
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
  a <- lx0 cast_
  go a
 where
  go a = option a do
    s <- mulop
    b <- lx0 cast_
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
  choice
    [ lx0 unary,
      do
        typ <- lx0 (inpar (lx1 parsetype))
        val <- cast_
        pure $ ExprCast typ val
    ]

unary = choice [postfix, pfxpm, pfxcast, sizeof, alignof]
 where
  pfxpm =
    $( switch_ws0
         [|
           case _ of "++" -> pure ExprPreInc; "--" -> pure ExprPreDec
           |]
     )
      <*> unary
  pfxcast =
    $( switch_ws0
         [|
           case _ of
             "&" -> pure ExprAddrOf
             "*" -> pure ExprDeref
             "+" -> pure ExprUnaryPlus
             "~" -> pure ExprBitNot
             "!" -> pure ExprNot
             "-" -> pure ExprUnaryMinus
           |]
     )
      <*> cast_
  sizeof = do
    lx1 sizeof'
    (ExprSizeOf . Left <$> inpar (lx1 parsetype))
      <|> (ExprSizeOf . Right <$> unary)
  alignof = do
    lx1 alignof'
    ExprAlignOf . Left <$> inpar (lx1 parsetype)

compound = do
  -- For now, just handle the error since we need to refactor
  -- the Expr and Type handling in the AST to properly implement
  -- compound literals
  lx0 (inpar (lx1 parsetype)) >> lx0 (incur anyChar) >> failed

postfix = go =<< primary
 where
  go a = do
    let sfxop =
          anyChar >>= \case
            '[' -> do
              b <- lx0 expr_
              lx0 rsqb
              pure $ ExprArray a b
            '(' -> do
              b <- assign `sepBy` lx0 comma
              lx0 rpar
              pure $ ExprCall a b
            '.' -> do
              -- placeholder because we don't have a symbol table yet.
              _ <- lx1 identifier
              s <- newUnique
              pure $ ExprMember a s
            '-' ->
              anyChar >>= \case
                '>' -> do
                  _ <- lx1 identifier
                  s <- newUnique
                  pure $ ExprMemberPtr a s
                '-' -> pure $ ExprPostDec a
                _ -> failed
            '+' ->
              anyChar >>= \case
                '+' -> pure $ ExprPostInc a
                _ -> failed
            _ -> failed
    result <- option a (sfxop <|> compound)
    if result == a
      then pure result
      else go result -- Recursively parse more postfix operators

primary = ident <|> litexpr <|> paren <|> generic

paren = Expr <$> runandgetspan (inpar (PrimParen <$> expr_))

litexpr = Expr <$> runandgetspan (PrimLit <$> literal)

ident = do
  withSpan (lx1 $ byteStringOf identifier) \i s ->
    pure $ Expr $ WithSpan s $ PrimId i

generic = do
  lx1 _Generic'
  inpar do
    a <- lx0 assign
    lx0 comma
    b <- genassoc `sepBy1` lx0 comma
    pure $ ExprGeneric a b
 where
  genassoc = do
    -- Temporarily handle only the default case since we need to
    -- decide how to convert Type to Str or adjust the GenAssoc structure
    lx1 default'
    lx0 colon
    b <- lx0 assign
    pure $ GenAssoc Nothing b

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
