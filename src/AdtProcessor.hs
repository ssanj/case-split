module AdtProcessor (findAndProcessAdts) where

import Data.Either (rights)
import Data.List (find)
import Data.Maybe (isJust)
import Control.Applicative (liftA2)
import Control.Monad (filterM)
import System.Directory
import FileUtil
import AdtParser

data AdtMatchedResults     = AdtMatchedResults [FilePath] deriving Show
data AdtConsideredResults  = AdtConsideredResults [FilePath] deriving Show

processMatches :: AdtMatchedResults -> IO [(String, [AdtType])]
processMatches (AdtMatchedResults files) =
    let processedFileContent = fmap (\f -> fmap processAdt $ fetchLines f) files in
    foldMap id processedFileContent

processAdt :: [String] -> [(String, [AdtType])]
processAdt contents = let keys       = rights $ fmap (getAdt) contents
                          keyValues  = fmap (\k -> (k, getAdtType k contents)) keys
                          matches    = filter (\(_, v) -> not $ null v) keyValues
                      in matches

findAdtMatches :: FilePath -> IO (AdtMatchedResults, AdtConsideredResults)
findAdtMatches root = do allDirs <- getAllSubDirectories root
                         let scalaFiles = fmap getScalaFilesInDir allDirs -- [IO [FilePath]]
                         (matched, considered) <- foldMap (fmap (\fp -> (findFilesThatMatch fp, pure fp))) scalaFiles -- (IO [FilePath], IO [FilePath])
                         liftA2 (\m c -> (AdtMatchedResults m, AdtConsideredResults c)) matched considered

findFilesThatMatch :: [FilePath] -> IO [FilePath]
findFilesThatMatch files = filterM (\f -> isJust . find hasADT <$> (fetchLines f)) files

findAndProcessAdts :: FilePath -> IO [(String, [AdtType])]
findAndProcessAdts filepath = do (matched, _) <- findAdtMatches filepath
                                 processMatches matched