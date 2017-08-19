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
main = benchFilesIO >>= (mapM (\f -> (f,) <$> readFile f)) >>= (\benchFiles -> defaultMain
    [
        bgroup "parseGood" $
            map (\(file, content) ->
                (bench file . nf (either (error.show) id . parseCompilationUnit)) content) benchFiles,
        -- bgroup "parseHuge" $ pure $ benchFile "huge java file" hugeJavaDirectory "RandoopTest0.java",
        bgroup "parseBad" $ pure $ benchBad "BadRealLife" badJavaDirectory "RealLifeJava.java"
    ])
    where
        benchFilesIO = map (goodJavaDirectory </>) . filter isJavaFile <$> getDirectoryContents goodJavaDirectory
        benchFile name dir file = (bench name . nf (either (error.show) id . parseCompilationUnit)) (dir </> file)
        benchBad name dir file = (bench name . nf (parseCompilationUnit)) (dir </> file)
