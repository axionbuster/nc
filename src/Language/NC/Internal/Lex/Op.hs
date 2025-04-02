{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.NC.Internal.Lex.Op where

import Language.NC.Internal.Lex.Lex
import {-# SOURCE #-} Language.NC.Internal.Lex.Type
import Language.NC.Internal.Prelude hiding (assign, shift)

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
  deriving (Eq, Show)

-- | A @Generic\_@ selection expression.
data GenAssoc
  = -- | generic selection
    GenAssoc (Maybe Str) Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

{- PEG grammar referenced; converted from C23 CFG grammar.

# Expressions
primary <- id / const / str / "(" expr ")" / "_Generic" "(" assign "," genassoc ("," genassoc)* ")"
genassoc <- (type ":" / "default" ":") assign
postfix <- primary (("[" expr "]" / "(" args? ")" / "." id / "->" id / "++" / "--") / compound)*
args <- assign ("," assign)*
compound <- "(" storage? type ")" init
storage <- storage_spec+
unary <- postfix / ("++" / "--") unary / ("&" / "*" / "+" / "-" / "~" / "!") cast / "sizeof" (unary / "(" type ")") / "alignof" "(" type ")"
cast <- unary / "(" type ")" cast
mul <- cast (("*" / "/" / "%") cast)*
add <- mul (("+" / "-") mul)*
shift <- add (("<<" / ">>") add)*
rel <- shift (("<" / ">" / "<=" / ">=" / "==" / "!=") shift)*
bitand <- rel ("&" rel)*
bitxor <- bitand ("^" bitand)*
bitor <- bitxor ("|" bitxor)*
logand <- bitor ("&&" bitor)*
logor <- logand ("||" logand)*
cond <- logor ("?" expr ":" cond)?
assign <- cond / unary ("=" / "+=" / "-=" / "*=" / "/=" / "%=" / "<<=" / ">>=" / "|=" / "^=" / "&=") assign
expr <- assign ("," assign)*

# Declarations
decl <- decl_spec init_decl_list? ";" / attr decl_spec init_decl_list ";" / static_assert / attr_decl
decl_spec <- (storage_spec / type_spec / func_spec) attr? decl_spec?
init_decl_list <- init_decl ("," init_decl)*
-}

-- | Parse an expression and additionally return the span.
expr = runandgetspan expr_

-- Use foldl to chain comma operators
expr_ = do
  a <- assign
  b <- many (lx0 comma >> assign)
  pure $ foldl ExprComma a b

assign =
  cond <|> do
    a <- lx0 unary
    operator <- lx0 binasgnop
    b <- lx0 assign
    pure $ operator a b

cond = do
  a <- lx0 logor
  let qu = do
        lx0 questionmark
        b <- lx0 expr_
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
  b <- lx0 shift
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
  b <- lx0 add
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
  b <- lx0 mul
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
  b <- lx0 cast_
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
    (ExprSizeOf . Left <$> inpar (lx1 parsetype)) <|> (ExprSizeOf . Right <$> unary)
  alignof = do
    lx1 alignof'
    ExprAlignOf . Left <$> inpar (lx1 parsetype)

compound = do
  -- For now, just handle the error since we need to refactor
  -- the Expr and Type handling in the AST to properly implement
  -- compound literals
  lx0 (inpar (lx1 parsetype)) >> lx0 (incur anyChar) >> failed

postfix = do
  a <- primary
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
          _ -> failed
  sfxop <|> compound

primary = ident <|> litexpr <|> paren <|> generic

paren = inpar $ do
  we <- runandgetspan expr_
  withSpan (pure ()) $ \_ spn ->
    pure $ Expr $ WithSpan spn $ PrimParen we

litexpr = do
  l <- literal
  withSpan (pure ()) $ \_ spn ->
    pure $ Expr $ WithSpan spn $ PrimLit l

ident = do
  -- Use byteStringOf to capture the text
  WithSpan s _ <- lx1 (runandgetspan identifier)
  i <- byteStringOf identifier
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
