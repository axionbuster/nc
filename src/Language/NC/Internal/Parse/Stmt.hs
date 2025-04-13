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
statement = do
  as <- attrspecs
  let label_continue l = StmtLabeled l <$> (colon *> statement)
  let label = undefined
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
                     cond <- inpar expr_
                     thenclause <- statement
                     elseclause <- optional (else' >> statement)
                     pure $ StmtIf cond thenclause elseclause
                   "switch" -> StmtSwitch <$> inpar expr_ <*> statement
                   "while" -> StmtWhile <$> inpar expr_ <*> statement
                   "do" ->
                     StmtDoWhile
                       <$> statement
                       <*> (while' >> inpar expr_)
                   "for" -> do
                     initcl <-
                       option (Left Nothing)
                         $ (Right <$> declaration)
                         <|> (Left . Just <$> expr_ <* semicolon)
                     testcl <- optional expr_ <* semicolon
                     postcl <- optional expr_ <* semicolon
                     bodycl <- statement
                     let fh =
                           ForExpr Nothing testcl postcl
                             & cforh_init
                             .~ initcl
                     pure $ StmtFor fh bodycl
                 |]
           )
      jumpstmt =
        $( switch_ws1
             [|
               case _ of
                 "goto" -> flip cut (BasicError "expected label") do
                   StmtJump . JumpGoto . JGUnresolved <$> identifier
                 "continue" -> flip cut (BasicError "expected ;") do
                   semicolon $> StmtJump JumpContinue
                 "break" -> flip cut (BasicError "expected ;") do
                   semicolon $> StmtJump JumpBreak
                 "return" ->
                   flip cut (BasicError "expected expression") do
                     StmtJump . JumpReturn <$> optional expr_
               |]
         )
  $( switch_ws0
       [|
         case _ of
           "case" ->
             LabelCase as
               <$> (CIEUnresolved <$> expr_)
               <*> newsymbol
               >>= label_continue
           "default" ->
             LabelDefault as <$> newsymbol >>= label_continue
           _ ->
             -- this backtracking is due to the need to backtrack on
             -- the colon (:) to distinguish between labeled statements
             -- vs. (primary-block or jump-statement).
             choice
               [ do
                   -- FIXME: labels should belong to a separate namespace.
                   -- I need to fix the symbol table/hierarchy of scopes.
                   labelname <- identifier_def
                   labelsym <- newsymbol <* colon
                   symgivename labelsym labelname -- BAD!
                   label_continue $ LabelNamed as labelsym,
                 primaryblock,
                 jumpstmt
               ]
         |]
   )
