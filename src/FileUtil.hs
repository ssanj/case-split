module FileUtil (getScalaFilesInDir,
                 fetchLines,
                 findFilesThatMatch,
                 getDirectories,
                 getAllSubDirectories,
                 findAdtMatches) where

import Data.Char (isSpace)
import Data.Maybe (isJust)
import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Data.List (dropWhileEnd, find, isSuffixOf, isPrefixOf)
import System.Directory
import System.FilePath.Posix
import AdtParser

data AdtMatchedResults     = AdtMatchedResults [FilePath] deriving Show
data AdtConsideredResults = AdtConsideredResults [FilePath] deriving Show


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
                         _ <- mapM_ putStrLn absoluteFiles
                         filterM doesDirectoryExist absoluteFiles

-- TODO: Handle directory read errors - skip and continue
getAllSubDirectories :: FilePath -> IO [FilePath]
getAllSubDirectories root = do dirs <- getDirectories root
                               let subs    = fmap getAllSubDirectories dirs
                                   allDirs = (pure dirs) : subs
                               foldMap id allDirs

findAdtMatches :: FilePath -> IO (AdtMatchedResults, AdtConsideredResults)
findAdtMatches root = do allDirs <- getAllSubDirectories root
                         let scalaFiles = fmap getScalaFilesInDir allDirs -- [IO [FilePath]]
                         (matched, considered) <- foldMap (fmap (\fp -> (findFilesThatMatch fp, pure fp))) scalaFiles -- (IO [FilePath], IO [FilePath])
                         liftA2 (\m c -> (AdtMatchedResults m, AdtConsideredResults c)) matched considered

findFilesThatMatch :: [FilePath] -> IO [FilePath]
findFilesThatMatch files = filterM (\f -> isJust . find hasADT <$> (fetchLines f)) files

fetchLines :: FilePath -> IO [String]
fetchLines file = (fmap trim . lines) <$> readFile file

scalaFile :: FilePath -> Bool
scalaFile = isSuffixOf ".scala"

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "."