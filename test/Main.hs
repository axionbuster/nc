module Main (main) where

import Control.Monad
import Language.NC.CTypes qualified as CT
import Language.NC.ParseDec
import Language.NC.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unittests]

data PT = PT String (Either Error CT.PrimType)
  deriving (Eq, Show)

ptlist1 :: [PT]
ptlist1 =
  [ PT "int" (Right CT.Int_),
    PT "char" (Right CT.Char_),
    PT "void" (Right CT.Void),
    PT "signed char" (Right (CT.Char (Just CT.Signed))),
    PT "unsigned char" (Right (CT.Char (Just CT.Unsigned))),
    PT "short" (Right CT.Short_),
    PT "signed short" (Right CT.Short_),
    PT "short int" (Right CT.Short_),
    PT "signed short int" (Right CT.Short_),
    PT "unsigned short" (Right CT.UShort_),
    PT "unsigned short int" (Right CT.UShort_),
    PT "int" (Right CT.Int_),
    PT "signed" (Right CT.Int_),
    PT "signed int" (Right CT.Int_),
    PT "unsigned" (Right CT.UInt_),
    PT "unsigned int" (Right CT.UInt_),
    PT "long" (Right CT.Long_),
    PT "signed long" (Right CT.Long_),
    PT "long int" (Right CT.Long_),
    PT "signed long int" (Right CT.Long_),
    PT "unsigned long" (Right CT.ULong_),
    PT "unsigned long int" (Right CT.ULong_),
    PT "long long" (Right CT.LongLong_),
    PT "signed long long" (Right CT.LongLong_),
    PT "long long int" (Right CT.LongLong_),
    PT "signed long long int" (Right CT.LongLong_),
    PT "unsigned long long" (Right CT.ULongLong_),
    PT "unsigned long long int" (Right CT.ULongLong_),
    PT "float" (Right CT.Float_),
    PT "double" (Right CT.Double_),
    PT "long double" (Right CT.LongDouble_),
    PT "_Bool" (Right CT.Bool),
    PT "float _Complex" (Right CT.ComplexFloat_),
    PT "double _Complex" (Right CT.ComplexDouble_),
    PT "long double _Complex" (Right CT.ComplexLongDouble_)
  ]

ptlist2 :: [PT]
ptlist2 =
  [ PT "signed long double" (Left (PrimTypeBadError BecauseSignInLongDouble)),
    PT "unsigned long double" (Left (PrimTypeBadError BecauseSignInLongDouble)),
    PT "signed float" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "unsigned float" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "signed double" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "unsigned double" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "signed _Bool" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "unsigned _Bool" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "signed void" (Left (PrimTypeBadError BecauseSignNotMeaningful)),
    PT "unsigned void" (Left (PrimTypeBadError BecauseSignNotMeaningful))
  ]

runpttests :: [PT] -> [TestTree]
runpttests = map (\p@(PT n _) -> testCase n (checkpt p))

checkpt :: PT -> IO ()
checkpt (PT s e) = do
  r <- test_runparser0 (primtype <* cut eof UnexpectedEOFError) s
  case r of
    OK (WithSpan _ a) _ _ -> case e of
      Right k ->
        unless (a == k) $
          assertFailure $
            printf
              "checkpt OK/Right/NEQ: got %s instead of %s"
              (show a)
              (show k)
      k ->
        assertFailure $
          printf
            "checkpt OK/Left: succeeded with %s where error of %s expected"
            (show a)
            (show k)
    Err f -> case e of
      Left g ->
        unless (f == g) $
          assertFailure $
            printf
              "checkpt Err/Left/NEQ: got %s instead of %s"
              (show f)
              (show g)
      g ->
        assertFailure $
          printf
            "checkpt Err/Right: errored with %s where success of %s expected"
            (show f)
            (show g)
    Fail -> assertFailure $ printf "checkpt Fail: gave uninformative error"

unittests :: TestTree
unittests =
  testGroup
    "Unit Tests"
    [ testGroup
        "Primitive, non-derived type parsing, must parse"
        (runpttests ptlist1),
      testGroup
        "Primitive, non-derived type parsing, must not parse"
        (runpttests ptlist2)
    ]
