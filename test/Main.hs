module Main (main) where

import Control.Monad
import Language.NC.CTypes qualified as CT
import Language.NC.ParseDec
import Language.NC.Prelude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unittests]

data PT = PT String (Either Error CT.PrimType)
  deriving (Eq, Show)

checkpt :: PT -> Parser (WithSpan CT.PrimType) -> IO ()
checkpt (PT s e) pa = do
  r <- test_runparser0 pa s
  case r of
    OK (WithSpan _ a) _ _ -> case e of
      Right k -> unless (a == k) $ assertFailure "checkpt OK/Right/NEQ"
      _ -> assertFailure "checkpt OK/Left (errored where success expected)"
    Err f -> case e of
      Left g -> unless (f == g) $ assertFailure "checkpt Err/Left/NEQ"
      _ -> assertFailure "checkpt Err/Right (succeeded where error expected)"
    Fail -> assertFailure "checkpt Fail (gave uninformative error)"

checkpt0 :: PT -> IO ()
checkpt0 pt = checkpt pt primtype

unittests :: TestTree
unittests =
  testGroup
    "Unit Tests"
    [testCase "" $ checkpt0 (PT "int" (Right CT.Int_))]
