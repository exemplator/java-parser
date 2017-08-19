{-# LANGUAGE TupleSections #-}
module Main where

import           Criterion.Main

import           Language.Java.Parser

import           BenchClasses
import           Control.Monad        (mapM)
import           Data.List            (isSuffixOf)
import           System.Directory
import           System.FilePath
import           Text.Printf          (printf)

goodJavaDirectory :: FilePath
goodJavaDirectory = "java/good"

hugeJavaDirectory :: FilePath
hugeJavaDirectory = "java/huge"

badJavaDirectory :: FilePath
badJavaDirectory = "java/bad"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

main :: IO ()
main = (\a b c -> (a,b,c)) <$> goodFiles <*> hugeFile <*> badFile >>= (\(benchFiles, huge, bad) -> defaultMain
    [
        bgroup "parseGood" $
            map (\(file, content) ->
                (bench file . nf (either (error.show) id . parseCompilationUnit)) content) benchFiles,
        bgroup "parseHuge" $ pure $ benchFile "huge java file" huge,
        bgroup "parseBad" $ pure $ benchBad "BadRealLife" bad
    ])
    where
        goodFiles = benchFilesIO >>= (mapM (\f -> (f,) <$> readFile f))
        hugeFile = readFile (hugeJavaDirectory </> "RandoopTest0.java")
        badFile = readFile (badJavaDirectory </> "RealLifeJava.java")
        benchFilesIO = map (goodJavaDirectory </>) . filter isJavaFile <$> getDirectoryContents goodJavaDirectory
        benchFile name = (bench name . nf (either (error.show) id . parseCompilationUnit))
        benchBad name = (bench name . nf (parseCompilationUnit))
