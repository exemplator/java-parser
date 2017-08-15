module Bench where

import           Criterion.Main

import           Language.Java.Parser

import           Text.Printf          (printf)

benchJavaDirectory :: FilePath
benchJavaDirectory = "java/good"

isJavaFile :: FilePath -> Bool
isJavaFile f = ".java" `isSuffixOf` f

main :: IO ()
main = defaultMain
  [ bgroup "parse"
    map (\file -> bench file $ parseCompilationUnit file) benchFiles
  ]
  where benchFiles = map (benchJavaDirectory </>) . filter isJavaFile <$> getDirectoryContents benchJavaDirectory
