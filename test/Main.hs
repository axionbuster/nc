{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Functor ((<&>))
import Data.List (nub)
import Data.String (IsString (..))
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

-- | Primitive type parsing case.
--
--  - 'Left': one of many errors that could be thrown; empty list for any error.
--  - 'Right': the type it must resolve to.
data PT = PT String (Either [Error] CT.PrimType)
  deriving (Eq, Show)

-- positive parsing
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

-- negative parsing
ptlist2 :: [PT]
ptlist2 =
  [ PT "signed long double" (Left errs0),
    PT "unsigned long double" (Left errs0),
    PT "signed float" (Left errs1),
    PT "unsigned float" (Left errs1),
    PT "signed double" (Left errs1),
    PT "unsigned double" (Left errs1),
    PT "signed _Bool" (Left errs1),
    PT "unsigned _Bool" (Left errs1),
    PT "signed void" (Left errs1),
    PT "unsigned void" (Left errs1)
  ]
  where
    -- possible range of errors for long double
    errs0 =
      [ PrimTypeBadError BecauseSignInLongDouble,
        PrimTypeBadError BecauseSignNotMeaningful
      ]
    -- possible range of errors for other types
    errs1 =
      [ PrimTypeBadError BecauseSignNotMeaningful,
        UnexpectedEOFError
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
      Left [] ->
        assertFailure $
          printf
            "checkpt OK/Right/[]: succeeded with %s where any error expected"
            (show a)
      k ->
        assertFailure $
          printf
            "checkpt OK/Left: succeeded with %s where error of one of %s exp."
            (show a)
            (show k)
    Err f -> case e of
      Left [] -> pure ()
      Left gs ->
        unless (f `elem` gs) $
          assertFailure $
            printf
              "checkpt Err/Left/NEQ: got %s instead of one of %s"
              (show f)
              (show gs)
      gs ->
        assertFailure $
          printf
            "checkpt Err/Right: errored with %s where success of %s expected"
            (show f)
            (show gs)
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
        (runpttests ptlist2),
      testGroup
        "Prmitive, exhaustive test over a list"
        [testCase "" exhcheck1]
    ]

-- used for some 'LMAO' testing below
data LMAO = LMAO {lmaointended :: String, lmaoexpressed :: String}

instance Semigroup LMAO where
  LMAO ~a ~b <> LMAO ~c ~d = LMAO (a <> c) (b <> d)

instance Monoid LMAO where
  mempty = LMAO mempty mempty

instance Show LMAO where
  show l = printf "\"%s\" (--> %s)" l.lmaoexpressed l.lmaointended

instance IsString LMAO where
  fromString s = LMAO s s

-- comparison on expressed portion only
instance Eq LMAO where
  ~a == ~b = a.lmaointended == b.lmaointended

-- make a sea of "types" -- well, only a tiny fraction will
-- designate primitive non-derived types.
mkseaoftypes :: [LMAO]
mkseaoftypes = do
  -- last computed: less than 337,500 choices. may have changed.
  let ty1 =
        [ "",
          "int",
          "char",
          "void",
          "short",
          "long",
          "long int",
          "long long",
          "double",
          "float",
          "_Bool",
          "_Complex"
        ]
  let sgn = ["", "signed", "unsigned"]
  let ws = LMAO "" <$> ["", " ", "/*\t*/\t/* int */", "/*int long*/", "//\n"]
  let mergewords a b c d e = a <> b <> c <> d <> e
  mergewords
    <$> ws
    <*> (sgn `mplus` ty1)
    <*> ws
    <*> (sgn `mplus` ty1)
    <*> ws

-- an exhaustive check to make sure there's no over-parsing.
exhcheck1 :: IO ()
exhcheck1 = nubbed >>= report
  where
    nubbed = nub <$> filterM parses mkseaoftypes
    report n
      | length n == length ptlist1 = pure ()
      | otherwise = do
          let samplen = 50
          assertFailure
            ( printf
                "get %d /= expect %d; samples (%d)\n%s"
                (length n)
                (length ptlist1)
                (min samplen (length n))
                (show $ Prelude.take samplen n)
            )
    parses s =
      test_runparser0 primtype s.lmaoexpressed <&> \case
        OK {} -> True
        _ -> False
