{-# LANGUAGE MultilineStrings #-}

module Language.NC.Internal.Parse.Stmt (
  -- * Statement parsers (debug)
  statement,
  statement0,
  _dbg_example0,
) where

import Data.Sequence qualified as S
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

-- | Construct a compound statement type but with simple culling if there's only
-- a single statement.
conscompound :: Seq BlockItem -> Statement
conscompound = \case
  (BIStmt s :<| S.Empty) -> s
  ss -> StmtCompound ss

-- | Internal type. Decide what to do when statement parsing fails.
-- Use backup or just fail?
type OnEmpty a = a -> Parser a -> Parser a

-- | Parse a statement. Fail on empty statement.
statement :: Parser Statement
statement = statement_ (const id)

-- | Parse a statement. Succeed with an empty expression statement
-- on parse failure.
statement0 :: Parser Statement
statement0 = statement_ option

-- | The zero statement
zerostmt :: Statement
zerostmt = StmtExpr [] Nothing

-- | Parse a statement
statement_ :: OnEmpty Statement -> Parser Statement
statement_ oespolicy =
  let self = statement_ oespolicy
      cutcolon = pcut colon (BasicError "expected :")
      cutsemicolon = pcut semicolon (BasicError "expected ;")
      cutstmt = statement `pcut` BasicError "expected statement"
      cutlpar = lpar `pcut` BasicError "expected ("
      cutparexpr = do
        cutlpar
        e <- expr_ `pcut` BasicError "expected expression"
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
                         l <- LabelNamed as <$> newsymbol
                         symassoclabel l labname $> l
                     |]
               )
            primaryblock =
              branch_incur
                ( conscompound
                    <$> seq_many do
                      choice
                        [ BIDecl <$> declaration,
                          BIStmt <$> self,
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
                           bodycl <- statement <|> (cutsemicolon $> zerostmt)
                           pure $ StmtFor (initcl testcl postcl) bodycl
                       |]
                 )
            jumpstmt =
              $( switch_ws1
                   [|
                     case _ of
                       "goto" -> flip pcut (BasicError "expected label") do
                         StmtJump . JumpGoto . JGUnresolved <$> identifier
                       "continue" -> pure $ StmtJump JumpContinue
                       "break" -> pure $ StmtJump JumpBreak
                       "return" -> StmtJump . JumpReturn <$> optional expr_
                     |]
               )
                <* cutsemicolon
        withOption
          label
          (\l -> StmtLabeled l <$> self)
          $ oespolicy
            (StmtExpr as Nothing) -- fallback; usage depends on oespolicy.
            (StmtExpr as . Just <$> expr_ <* cutsemicolon)
          <|> primaryblock
          <|> jumpstmt

_dbg_example0 :: String
_dbg_example0 =
  """
    {
    /* Complex nested control structures with mixed declarations */
    start:
    /* [[deprecated("Use new_api instead")]] int result = 0; */
    int result = 0;

    /* Nested compound statements with various control flows */
    if (argc > 1) {
      /* Declaration with multiple variables and initializers */
      register const volatile long *values[10], count = argc - 1, *ptr = NULL;
      /* Switch with fallthrough cases, nested blocks and declarations */
      switch (argv[1][0]) {
        case 'a': case 'A': {
          /* our parser does not support interpreting floating points yet */
          /* double temp = 0.0; */
          long temp = 0;
          for (int i = 0; i < count; ++i) {
            if (i % 2) continue;
            /* temp += i * 3.14; */
            temp += i * 314;
          }
          result = (int)temp;
          /* Intentional fallthrough */
        }
        case 'b': {
          do {
            /* Complex multiple declaration with function pointers */
            void (*handlers[5])(int, void*), (*process)(void) = NULL;
            /* Nested if-else with goto */
            if (count-- > 5) {
              while (count > 0 && !ptr) {
                ptr = count % 3 ? &count : NULL;
                if (!ptr) break;
              }
            } else if (count == 0) {
              goto end;
            } else {
              default_handler: 
              result = -1;
            }
          } while (ptr && *ptr > 0);
          break;
        }
        default:
          result = -2;
      }
    } else {
      [[unlikely]] for (struct { int x; char *y; } entry = {0, "test"}; 
           entry.x < 10; 
           entry.x++) {
        if (entry.x == 5) {
          entry.y = "halfway";
          continue;
        }
        result += entry.x;
      }
    }

    /* Statement expressions and complex comma expressions */
    /* (void)({int x = 5; x *= 2; result += x;}); */
    result = (printf("Processing: %d\\n", result), result > 0 ? result * 2 : 0);

    end:
    return result;
  }
  """
