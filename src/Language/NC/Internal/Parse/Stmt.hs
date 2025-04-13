module Language.NC.Internal.Parse.Stmt (
  -- * Statement parsers (debug)
  statement,
) where

import Language.NC.Internal.Lex
import Language.NC.Internal.Parse.Op
import Language.NC.Internal.Parse.Type
import Language.NC.Internal.Prelude
import Unsafe.Coerce

-- | Parse many items in a sequence. Code is adapted from
-- the FlatParse source.
seq_many :: Parser a -> Parser (Seq a)
seq_many (ParserT p) = ParserT do
  fix \go fp !r eob s n st ->
    case p fp r eob s n st of
      OK# st' a s' n' ->
        case go fp r eob s' n' st' of
          OK# st'' as s'' n'' -> OK# st'' (a :<| as) s'' n''
          x -> x
      Fail# st' -> OK# st' mempty s n
      e -> unsafeCoerce# e

-- | Parse a statement
statement :: Parser Statement
statement =
  let cutcolon = cut colon (BasicError "expected :")
      cutsemicolon = cut semicolon (BasicError "expected ;")
      cutstmt = statement `cut` BasicError "expected statement"
      cutlpar = lpar `cut` BasicError "expected ("
      cutrpar = rpar `cut` BasicError "expected )"
      cutparexpr = do
        cutlpar
        e <- expr_ `cut` BasicError "expected expression"
        cutrpar
        pure e
   in do
        as <- attrspecs
        let label =
              $( switch_ws0
                   [|
                     case _ of
                       "case" ->
                         LabelCase as
                           <$> (CIEUnresolved <$> expr_)
                           <*> newsymbol
                           <* cutcolon
                       "default" ->
                         LabelDefault as <$> newsymbol <* cutcolon
                       _ -> do
                         labname <- identifier_def <* colon
                         undefined labname
                     |]
               )
            primaryblock =
              branch_incur
                ( StmtCompound
                    <$> seq_many do
                      choice
                        [ BIDecl <$> declaration,
                          BIStmt <$> statement,
                          BILabel <$> label
                        ]
                )
                $( switch_ws1
                     [|
                       case _ of
                         "if" -> do
                           cond <- cutparexpr
                           thenclause <- cutstmt
                           elseclause <- optional $ else' >> cutstmt
                           pure $ StmtIf cond thenclause elseclause
                         "switch" -> StmtSwitch <$> cutparexpr <*> cutstmt
                         "while" -> StmtWhile <$> cutparexpr <*> cutstmt
                         "do" ->
                           StmtDoWhile
                             <$> statement
                             <*> (while' >> cutparexpr)
                         "for" -> do
                           cutlpar
                           initcl <-
                             (ForDecl <$> declaration)
                               <|> (ForExpr <$> optional expr_ <* cutsemicolon)
                           testcl <- optional expr_ <* cutsemicolon
                           postcl <- optional expr_
                           cutrpar
                           bodycl <- cutstmt
                           pure $ StmtFor (initcl testcl postcl) bodycl
                       |]
                 )
            jumpstmt =
              $( switch_ws1
                   [|
                     case _ of
                       "goto" -> flip cut (BasicError "expected label") do
                         StmtJump . JumpGoto . JGUnresolved <$> identifier
                       "continue" -> cutsemicolon $> StmtJump JumpContinue
                       "break" -> cutsemicolon $> StmtJump JumpBreak
                       "return" -> StmtJump . JumpReturn <$> optional expr_
                     |]
               )
        withOption
          label
          (\l -> StmtLabeled l <$> statement)
          do (StmtExpr as <$> optional expr_) <|> primaryblock <|> jumpstmt
        <* cutsemicolon
