-- |
-- Module: NC.Parser.Exor
-- Description: Parse expressions and statements.
module NC.Parser.Expr (
  expr,
  assignexpr,
  condexpr,
  constexpr,
) where

import NC.Internal.Prelude1 hiding (assign)

-- | Most general expression parser
expr :: P Expr
expr = undefined

-- | The rule @assignment-expression@ is used
-- on its own in a few different places.
assignexpr :: P Expr
assignexpr = undefined

-- | The rule @conditional-expression@ also doubles as @constant-expression@.
condexpr :: P Expr
condexpr = undefined

-- | This is a specialized version of 'condexpr' that simplifies parsing
-- by wrapping the parsed expression in a 'ConstIntExpr'.
constexpr :: P ConstIntExpr
constexpr = ConstIntExpr <$> condexpr <*> pure Nothing
