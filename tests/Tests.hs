{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Tests where

import           Prelude                hiding (exp)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           System.Directory
import           System.FilePath

import           Control.Applicative
import           Control.Monad
import           Data.List              (isSuffixOf)

import           Arbitrary
import qualified Control.Exception      as E
import           Language.Java.Java
import           Language.Java.Parser
import           Language.Java.Position
import           Language.Java.Pretty



----------------------------------------------------------
testJavaDirectory :: FilePath
testJavaDirectory = "java"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

parserSeg :: P (a Singleton) -> String -> Either ParseError (a Singleton)
parserSeg = parser

parseOne jFile = parserSeg compilationUnitNode <$> readFile jFile

toTestSimpleCase expected jFile = testCase (takeBaseName jFile) doTest
  where doTest = do r <- E.try (parseOne jFile)
                    case r of
                        Left (e :: E.SomeException) -> assertBool ("failure exception: " ++ show e) (not expected)
                        Right (Left perr)           -> assertBool ("failure parse error: " ++ show perr) (not expected)
                        Right (Right p)             -> assertBool ("success: " ++ show p) expected

getAllJavaPaths path = map (path </>) . filter isJavaFile <$> getDirectoryContents path

main = do
    exists <- doesDirectoryExist testJavaDirectory
    when (not exists) $ error "cannot find tests files java directory"

    allGoodJavas <- getAllJavaPaths (testJavaDirectory </> "good")
    allBadJavas <- getAllJavaPaths (testJavaDirectory </> "bad")

    defaultMain $ testGroup "java"
        [ testGroup "(parse.show.parse) == parse" (map (doRoundTripReal) allGoodJavas)
        , testGroup "parsing unit bad" (map (toTestSimpleCase False) allBadJavas)
        , testProperty "parsing.generating==id" (\(g :: CompilationUnit Singleton) ->
                                                        case parserSeg compilationUnit (show $ pretty g) of
                                                            Right g'  -> g == g'
                                                            Left perr -> error (show (pretty g) ++ show perr))
        , testGroup "generating.parsing==id"
          [ testRoundTripGenerated expParser' "ClassFieldAccess" "Object.super.x"
          , testRoundTripGenerated expParser' "QualInstanceCreation" "foo.new Bar()"
          , testRoundTripGenerated expParser' "MethodInvocation" "foo(1 + 2)"
          ]
        , testGroup "operator parsing"
          [ testParseSame expParser' "precedence 1"
              "1 +  2 * 3"
              "1 + (2 * 3)"
          , testParseSame expParser' "precedence 2"
              " 1 * 2  +  2 * 3"
              "(1 * 2) + (2 * 3)"
          , testParseSame expParser' "precedence 3"
              "1 || 2 && 3 | 4 ^ 5 == 6 > 7 >>> 8 - 9 * 10"
              "1 || (2 && (3 | (4 ^ (5 == (6 > (7 >>> (8 - (9 * 10))))))))"
          , testParseSame expParser' "associativity"
              "  1 - 2  - 3  - 4"
              "((1 - 2) - 3) - 4"
          ]
        ]
    where
        expParser' :: P (ExpNode Singleton)
        expParser' = expParser

testRoundTripGenerated p testName str = testCase testName $
  case parserSeg p str of
    Right syn -> assertEqual "" (prettyPrint syn) str
    Left perr -> error (str ++ show perr)

doRoundTripReal jFile = testCase (takeBaseName jFile) doTest
    where doTest = do   r <- E.try (testRoundTripReal <$> (readFile jFile))
                        case r of
                            Left (e :: E.SomeException) -> error ("failure exception: " ++ show e)
                            Right (x)                   -> x

-- | test euqality of ast because of whitespaces, optional parens etc.
testRoundTripReal str =
    case (,) <$> doRoundTrip <*> singleStep of
        Right (ast1, ast2) -> assertEqual "" ast1 ast2
        Left perr -> error (str ++ show perr)
        where
            doRoundTrip, singleStep :: Either ParseError (CompilationUnitNode Singleton)
            doRoundTrip = (prettyPrint) <$> (parserSeg compilationUnitNode str) >>= (parserSeg compilationUnitNode)
            singleStep = parserSeg compilationUnitNode str

testParseSame p testName s1 s2 = testCase testName $
  case (parserSeg p s1, parserSeg p s2) of
    (Right e1, Right e2) -> assertEqual "" e1 e2
    result -> error (show result)
