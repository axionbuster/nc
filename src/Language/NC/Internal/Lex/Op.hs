{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.NC.Internal.Lex.Op where

import Control.Monad.Combinators.Expr
import Language.NC.Internal.Lex.Lex
import Language.NC.Internal.Prelude hiding (assign)

-- parsing C23 operators...
--  - an expression is an operator bound to operands, or a primary expression
--  - a primary expression is an identifier, a literal, a string literal,
--    a parenthesized expression, or a generic selection
--
-- ... design notes ...
--  - not much to say except that it'll refrain from a deep or unbalanced
--    tree such as would occur with a literal implementation of the
--    C grammar. the CST isn't being constructed; the AST is
--    being directly constructed.

-- | A primary expression.
data PrimExpr
  = -- | identifier
    PrimId Str
  | -- | literal, except string literal
    PrimLit Lit
  | -- | string literal
    PrimStrLit Str
  | -- | parenthesized expression
    PrimParen (WithSpan Expr)
  | -- | generic selection
    PrimGeneric Expr GenAssoc

-- | A @Generic\_@ selection expression.
data GenAssoc
  = -- | generic selection
    GenAssoc (Maybe Str) Expr

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
    ExprCast Expr Expr
  | -- | dereference using (*)
    ExprDeref Expr
  | -- | address of using (&)
    ExprAddrOf Expr
  | -- | sizeof operator, either type or value
    ExprSizeOf Expr
  | -- | alignof operator, either type or value
    ExprAlignOf Expr
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

-- | Parse an expression and additionally return the span.
expr = runandgetspan expr_

expr_ = assign `sepBy` lx0 comma

assign =
  cond <|> do
    a <- lx0 unary
    op <- lx0 binasgnop
    b <- lx0 assign
    op a b

cond = do
  a <- lx0 logor
  let qu = do
        lx0 questionmark
        b <- lx1 expr
        lx0 colon
        c <- cond
        pure $ ExprConditional a b c
  qu <|> pure a

logor =
  chainl ExprOr (lx0 logand) do
    lx0 doublebar
    lx0 logand

logand =
  chainl ExprAnd (lx0 bitor) do
    lx0 ampersand
    lx0 bitor

bitor =
  chainl ExprBitOr (lx0 bitxor) do
    lx0 bar
    lx0 bitxor

bitxor =
  chainl ExprBitXor (lx0 bitand) do
    lx0 caret
    lx0 bitand

bitand =
  chainl ExprBitAnd (lx0 rel) do
    lx0 ampersand
    lx0 rel

rel = do
  a <- lx0 shift
  r <- relop
  b <- shift
  pure $ r a b
  where
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
  s <- shiftop
  b <- add
  pure $ s a b
  where
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
  s <- addop
  b <- mul
  pure $ s a b
  where
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
  s <- mulop
  b <- cast_
  pure $ s a b
  where
    mulop =
      $( switch_ws0
           [|
             case _ of
               "*" -> pure ExprTimes
               "/" -> pure ExprDiv
               "%" -> pure ExprMod
             |]
       )

cast_ = lx0 unary <|> ExprCast <$> lx0 (inpar (lx0 typeexpr)) <*> cast_

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
      unary <|> fmap ExprSizeOf (inpar (lx0 typeexpr))
    alignof = do
      lx1 alignof'
      ExprAlignOf <$> inpar (lx0 typeexpr)

compound =
  -- now literal in initializer list form.
  err $ InternalError "compound literal not implemented yet"

args = assign `sepBy` comma

postfix = do
  a <- primary
  let sfxop =
        anyChar >>= \case
          '[' -> do
            b <- lx0 expr_
            lx0 ']' $> ExprArray a b
          '(' -> do
            b <- assign `sepBy` comma
            lx0 ')' $> ExprCall a b
          '.' -> do
            b <- lx0 ident
            pure $ ExprMember a b
          '-' ->
            anyChar >>= \case
              '>' -> do
                b <- lx0 ident
                pure $ ExprMemberPtr a b
              '-' -> pure $ ExprPostDec a
              _ -> failed
          _ -> failed
  sfxop <|> compound

primary = ident <|> literal <|> inpar (lx0 expr) <|> generic

ident = do
  WithSpan s i <- lx1 (runandgetspan $ sbsOf identifier)
  pure . Expr . WithSpan s . PrimId $ i

generic = do
  lx1 _Generic'
  inpar do
    a <- lx1 assign
    lx0 comma
    b <- genassoc `sepBy1` comma
    pure $ ExprGeneric a b
  where
    genassoc = do
      a <- lx1 (typeexpr <|> default')
      lx0 colon
      GenAssoc (Just a) <$> many (lx0 comma *> lx1 typeexpr)

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
