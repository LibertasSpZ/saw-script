{- |
Module      : SAWScript.JavaTools
Description : TODO RGS
License     : BSD3
Maintainer  : atomb
Stability   : provisional
-}

module SAWScript.JavaTools
  ( JavaTools(..)
  , findJavaTools
  ) where

import Data.Maybe
import System.Directory

-- | TODO RGS
data JavaTools = JavaTools
  { jdepsPath  :: FilePath
  , jimagePath :: FilePath
  , jlinkPath  :: FilePath
  } deriving Show

-- | TODO RGS
findJavaTools :: [FilePath] -> IO (Maybe JavaTools)
findJavaTools javaBinDirs = do
  mbJdepsPath  <- findJavaToolPath_maybe "jdeps"
  mbJimagePath <- findJavaToolPath_maybe "jimage"
  mbJlinkPath  <- findJavaToolPath_maybe "jlink"
  pure $ JavaTools <$> mbJdepsPath
                   <*> mbJimagePath
                   <*> mbJlinkPath
  where
    findJavaToolPath_maybe :: String -> IO (Maybe FilePath)
    findJavaToolPath_maybe exe = listToMaybe <$> findExecutablesInDirectories javaBinDirs exe
