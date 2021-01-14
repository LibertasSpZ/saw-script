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
  , findJavaMajorVersion
  ) where

import Control.Monad (when)
import Data.Char (isNumber)
import Data.List.Extra (firstJust, isPrefixOf, trim)
import Data.Maybe
import System.Directory
import System.Exit
import System.Process

-- | TODO RGS
data JavaTools = JavaTools
  { javaPath   :: FilePath
  , jdepsPath  :: FilePath
  , jimagePath :: FilePath
  , jlinkPath  :: FilePath
  } deriving Show

-- | TODO RGS
findJavaTools :: [FilePath] -> IO (Maybe JavaTools)
findJavaTools javaBinDirs = do
  let findJavaToolPath_maybe exe = listToMaybe <$> findExecutablesInDirectories javaBinDirs exe
  mbJavaPath   <- findJavaToolPath_maybe "java"
  mbJdepsPath  <- findJavaToolPath_maybe "jdeps"
  mbJimagePath <- findJavaToolPath_maybe "jimage"
  mbJlinkPath  <- findJavaToolPath_maybe "jlink"
  pure $ JavaTools <$> mbJavaPath
                   <*> mbJdepsPath
                   <*> mbJimagePath
                   <*> mbJlinkPath

-- | TODO RGS: Is this even necessary? If Java has jimage and jlink, it's probably 9 or later.
findJavaMajorVersion :: JavaTools -> IO Int
findJavaMajorVersion (JavaTools{javaPath=java}) = do
  (ec, stdout, stderr) <- readProcessWithExitCode java ["-XshowSettings:properties", "-version"] ""
  when (ec /= ExitSuccess) $
    fail $ unlines
      [ java ++ " returned non-zero exit code: " ++ show ec
      , "Standard output:"
      , stdout
      , "Standard error:"
      , stderr
      ]
  let settings     = lines stderr
      mbVersionStr = firstJust getVersionStr_maybe settings
  case mbVersionStr of
    Nothing         -> fail $ "Could not detect the version of Java at " ++ java
    Just versionStr -> pure $ read $ takeMajorVersionStr $ trimOldJavaVersion versionStr
  where
    -- TODO RGS: Docs
    getVersionStr_maybe :: String -> Maybe String
    getVersionStr_maybe setting =
      let setting' = trim setting in
      if "java.version =" `isPrefixOf` setting'
          then Just $ dropWhile (not . isNumber) setting'
          else Nothing

    -- TODO RGS: Docs
    takeMajorVersionStr :: String -> String
    takeMajorVersionStr = takeWhile (/= '.')

    -- Turn version numbers like 1.8.<...> into 8.<...>
    -- TODO RGS: Say why this is necessary
    trimOldJavaVersion :: String -> String
    trimOldJavaVersion verStr | "1." `isPrefixOf` verStr = drop 2 verStr
                              | otherwise                = verStr
