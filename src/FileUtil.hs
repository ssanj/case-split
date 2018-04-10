module FileUtil (getScalaFilesInDir,
                 fetchLines,
                 getDirectories,
                 getAllSubDirectories) where

import Data.Char (isSpace)
import Control.Monad (filterM)
import Data.Monoid (Monoid)
import Control.Exception (IOException, catch)
import Data.List (dropWhileEnd, isSuffixOf, isPrefixOf)
import System.Directory
import System.IO (hPutStrLn, stderr)
import System.FilePath.Posix

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- TODO: Make this typesafe. Eg. Dir and File types
getScalaFilesInDir :: FilePath -> IO [FilePath]
getScalaFilesInDir dir = do files <- orEmpty (listDirectory dir)
                            let scalaFiles = filter scalaFile files
                                withPath   = (dir </>) <$> scalaFiles
                            return withPath

-- TODO: Extract filter predicate into a param
-- TOOD: Use a WriterT here to get the files observed
getDirectories :: FilePath -> IO [FilePath]
getDirectories root = do files <- orEmpty (listDirectory root)
                         let absoluteFiles = (root </>) <$> filter (not . isHidden) files
                         filterM doesDirectoryExist absoluteFiles

-- TODO: Handle directory read errors - skip and continue
getAllSubDirectories :: FilePath -> IO [FilePath]
getAllSubDirectories root = do dirs <- orEmpty (getDirectories root)
                               let subs    = fmap getAllSubDirectories dirs
                                   allDirs = pure dirs : subs
                               foldMap id allDirs

orEmpty :: Monoid a => IO a -> IO a
orEmpty target = handleErrors target mempty

handleErrors :: IO a -> a -> IO a
handleErrors target fallback =
  catch target (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("Warning: got error: " ++ err)
                         return fallback
                  )

fetchLines :: FilePath -> IO [String]
fetchLines file = (fmap trim . lines) <$> orEmpty (readFile file)

scalaFile :: FilePath -> Bool
scalaFile = isSuffixOf ".scala"

isHidden :: FilePath -> Bool
isHidden = isPrefixOf "."