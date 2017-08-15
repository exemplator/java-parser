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

benchJavaDirectory :: FilePath
benchJavaDirectory = "java/good"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

main :: IO ()
main = benchFilesIO >>= (mapM (\f -> (f,) <$> readFile f)) >>= (\benchFiles -> defaultMain
    [ bgroup "parse" $
        map (\(file, content) -> (bench file . nf (either (error.show) id . parseCompilationUnit)) content) benchFiles
    ])
    where benchFilesIO = map (benchJavaDirectory </>) . filter isJavaFile <$> getDirectoryContents benchJavaDirectory
