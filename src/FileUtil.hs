module FileUtil (getScalaFilesInDir,
                 fetchLines,
                 getDirectories,
                 getAllSubDirectories) where

import Data.Char (isSpace)
import Control.Monad (filterM)
import Data.List (dropWhileEnd, isSuffixOf, isPrefixOf)
import System.Directory
import System.FilePath.Posix

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- TODO: Make this typesafe. Eg. Dir and File types
getScalaFilesInDir :: FilePath -> IO [FilePath]
getScalaFilesInDir dir = do files <- listDirectory dir
                            let scalaFiles = filter scalaFile files
                                withPath   = (dir </>) <$> scalaFiles
                            return withPath

-- TODO: Extract filter predicate into a param
-- TOOD: Use a WriterT here to get the files observed
getDirectories :: FilePath -> IO [FilePath]
getDirectories root = do files <- listDirectory root
                         let absoluteFiles = fmap (root </>) $ filter (not . isHidden) files
                         filterM doesDirectoryExist absoluteFiles

-- TODO: Handle directory read errors - skip and continue
getAllSubDirectories :: FilePath -> IO [FilePath]
getAllSubDirectories root = do dirs <- getDirectories root
                               let subs    = fmap getAllSubDirectories dirs
                                   allDirs = (pure dirs) : subs
                               foldMap id allDirs


fetchLines :: FilePath -> IO [String]
fetchLines file = (fmap trim . lines) <$> readFile file

scalaFile :: FilePath -> Bool
scalaFile = isSuffixOf ".scala"

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "."