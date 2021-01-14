{- |
Module      : SAWScript.JavaCodebase
Description : TODO RGS
License     : BSD3
Maintainer  : atomb
Stability   : provisional
-}

-- TODO RGS: Note that this is a giant hack
module SAWScript.JavaCodebase
  ( JavaCodebase(..)
  , rtJarCodebase

    -- * TODO RGS: Note that these are cargo-culted
  , loadCodebase
  , lookupClass
  , supers
  , tryLookupClass
  ) where

import Control.Monad.Extra (ap, concatMapM, when)
import Data.List.Extra (isPrefixOf, trim)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.IORef
import Language.JVM.Common
import Language.JVM.JarReader
import Language.JVM.Parser
import SAWScript.JavaTools
import System.Directory.Extra (doesFileExist, listDirectory, listFilesInside)
import System.FilePath
import System.Exit
import System.IO.Temp
import System.Process
import qualified Verifier.Java.Codebase as JSS

-- TODO RGS: Docs
data JavaCodebase
  = JlinkCodebase JlinkCodebase
  | RTJarCodebase JSS.Codebase

-- TODO RGS: Docs
data JlinkCodebase = JC
  { jcJarReader  :: JarReader
  , jcClassPaths :: [FilePath]
  , jcState      :: IORef JlinkCodebaseState
  }

-- TODO RGS: Docs
data JlinkCodebaseState = JCS
  { jcsClassMap    :: Map ClassName Class
  , jcsSubclassMap :: Map ClassName [Class]
  }

-- TODO RGS: This is gon' change
loadCodebase :: [FilePath] -> [FilePath] -> [FilePath] -> IO JavaCodebase
loadCodebase jarFiles classPaths javaBinDirs = do
  mbJavaTools <- findJavaTools javaBinDirs
  case mbJavaTools of
    Nothing        -> rtJar
    Just javaTools -> do
      javaMajorVersion <- findJavaMajorVersion javaTools
      if javaMajorVersion >= 9
        then jlinked javaTools
        else rtJar
  where
    jlinked :: JavaTools -> IO JavaCodebase
    jlinked javaTools = do
      jars <- newJarReader jarFiles
      jimageExtractedDirs <- makeJlinkedRuntime javaTools
      jcs <- newIORef $ JCS{jcsClassMap = M.empty, jcsSubclassMap = M.empty}
      pure $ JlinkCodebase $ JC{ jcJarReader  = jars
                               , jcClassPaths = classPaths ++ jimageExtractedDirs
                               , jcState      = jcs }

    rtJar :: IO JavaCodebase
    rtJar = RTJarCodebase <$> JSS.loadCodebase jarFiles classPaths

    makeJlinkedRuntime :: JavaTools -> IO [FilePath]
    makeJlinkedRuntime JavaTools{ jdepsPath = jdeps, jlinkPath = jlink
                                , jimagePath = jimage } = do
      -- First, determine all of the modules needed with the jdeps tool.
      -- Unfortunately, jdeps requires all .class files to be specified
      -- individually, so we compute all of the .class files manually by
      -- traversing the classpaths.
      classFiles
        <- concatMapM (listFilesInside (\f -> pure $ takeExtension f == ".class")) classPaths
      -- TODO RGS: Factor out the scaffolding for readProcessWithExitCode
      (jdepsEC, jdepsStdout, jdepsStderr)
        <- readProcessWithExitCode jdeps ("--print-module-deps" : classFiles ++ jarFiles) ""
      when (jdepsEC /= ExitSuccess) $
        fail $ unlines
          [ jdeps ++ " returned non-zero exit code: " ++ show jdepsEC
          , "Standard output:"
          , jdepsStdout
          , "Standard error:"
          , jdepsStderr
          ]

      -- Next, use the modules computed earlier to assemble a minimal runtime
      -- environment with jlink. We put the resulting JRE in a temporary directory.
      systemTempDir <- getCanonicalTemporaryDirectory
      sawTempDir <- createTempDirectory systemTempDir "saw"
      let sawJRETempDir = sawTempDir </> "jre"
          jdepsModules  = trim jdepsStdout -- Remove any trailing newlines
      (jlinkEC, jlinkStdout, jlinkStderr)
        <- readProcessWithExitCode jlink [ "--no-header-files"
                                         , "--no-man-pages"
                                         , "--add-modules", jdepsModules
                                         , "--output", sawJRETempDir
                                         ] ""
      when (jlinkEC /= ExitSuccess) $
        fail $ unlines
          [ jlink ++ " returned non-zero exit code: " ++ show jlinkEC
          , "Standard output:"
          , jlinkStdout
          , "Standard error:"
          , jlinkStderr
          ]

      -- Finally, extract the contents of the minimal runtime to a temporary
      -- location. SAW will consult this when loading particular classes when
      -- they are first encountered.
      let jimageExtractedDir = sawTempDir </> "jre-extracted"
      (jimageEC, jimageStdout, jimageStderr)
        <- readProcessWithExitCode jimage [ "extract"
                                          , "--include", "regex:.*\\.class"
                                          , "--dir", jimageExtractedDir
                                          , sawJRETempDir </> "lib" </> "modules"
                                          ] ""
      when (jimageEC /= ExitSuccess) $
        fail $ unlines
          [ jimage ++ " returned non-zero exit code: " ++ show jimageEC
          , "Standard output:"
          , jimageStdout
          , "Standard error:"
          , jimageStderr
          ]

      listDirectory jimageExtractedDir

lookupClass :: JavaCodebase -> ClassName -> IO Class
lookupClass (JlinkCodebase cb) = lookupClassJlink cb
lookupClass (RTJarCodebase cb) = JSS.lookupClass cb

lookupClassJlink :: JlinkCodebase -> ClassName -> IO Class
lookupClassJlink cb clNm = do
  maybeCl <- tryLookupClassJlink cb clNm
  case maybeCl of
    Just cl -> return cl
    Nothing -> error $ errorMsg
  where
    dotNm = slashesToDots (unClassName clNm)
    isStandardLibClass = "java.lang" `isPrefixOf` dotNm
    errorMsg = unlines $
      if isStandardLibClass then
        [ "Cannot find class " ++ dotNm ++ " in codebase."
        , ""
        , "You probably forgot to specify the location of the"
        , "Java standard libraries JAR using the '-j' flag to saw or jss. The"
        , " standard libraries JAR is called 'classes.jar' on OS X systems and"
        , "'rt.jar' on Windows and Linux systems. Its location can be found by"
        , "running 'java -verbose 2>&1 | grep Opened', assuming you're using"
        , "a Sun Java."
        ]
      else
        [ "Cannot find class " ++ dotNm ++ " in codebase."
        , ""
        , "You can specify the location of classes you depend on using"
        , "the '-c' flag to specify non-jar classpaths and the '-j' flag"
        , "to specify the location of JAR files."
        ]

rtJarCodebase :: JavaCodebase -> JSS.Codebase
rtJarCodebase (RTJarCodebase cb) = cb
rtJarCodebase JlinkCodebase{}    = error "jvm-verifier does not support Java 9 or later"

supers :: JavaCodebase -> Class -> IO [Class]
supers (RTJarCodebase cb) cl = JSS.supers cb cl
supers (JlinkCodebase cb) cl =
  starClosureM (maybe (return []) (fmap (:[]) . lookupClassJlink cb) . superClass) cl
  where
    -- The monadic variant of starClosure
    starClosureM :: Monad m => (a -> m [a]) -> a -> m [a]
    starClosureM fn a =
      return ((a:) . concat) `ap` (mapM (starClosureM fn) =<< fn a)

tryLookupClass :: JavaCodebase -> ClassName -> IO (Maybe Class)
tryLookupClass (JlinkCodebase cb) = tryLookupClassJlink cb
tryLookupClass (RTJarCodebase cb) = JSS.tryLookupClass cb

tryLookupClassJlink :: JlinkCodebase -> ClassName -> IO (Maybe Class)
tryLookupClassJlink (JC{ jcJarReader = jarReader
                       , jcClassPaths = classPaths
                       , jcState = stateRef }) clNme = do
  state <- readIORef stateRef
  case M.lookup clNme (jcsClassMap state) of
    Just cl -> return (Just cl)
    Nothing -> do
      -- Here we bias our search to JARs before classpath directories,
      -- as mentioned above in 'loadCodebase'.
      let mcls = [loadClassFromJar clNme jarReader] ++
                 map (loadClassFromDir clNme) classPaths
      mcl <- foldl1 firstSuccess mcls
      case mcl of
        Just cl -> do
          writeIORef stateRef $! addClass cl state
          return $ Just cl
        Nothing -> return Nothing
  where
    -- Attempt to load a class by searching under directory @dir@, which
    -- is assumed to be a classpath component. If class @C1. ... .Cn@ is
    -- available under @dir@, then it must be located at
    -- @dir/C1/.../Cn.class@.
    -- http://docs.oracle.com/javase/8/docs/technotes/tools/findingclasses.html#userclass
    loadClassFromDir :: ClassName -> FilePath -> IO (Maybe Class)
    loadClassFromDir clNm dir = do
      exists <- doesFileExist file
      if exists
      then Just <$> loadClass file
      else return Nothing
      where
        file = dir </> classNameToClassFilePath clNm
        -- Turn a @com/example/Class@-style classname into a
        --  @"com" </> "example" </> "Class.class"@-style platform dependent
        --  relative class-file path.
        --
        -- TODO: move this to 'jvm-parser.git:Language.JVM.Common'?
        classNameToClassFilePath :: ClassName -> FilePath
        classNameToClassFilePath claNme =
          map (\c -> if c == '/' then pathSeparator else c) (unClassName claNme) <.> "class"

    -- Register a class with the given codebase
    addClass :: Class -> JlinkCodebaseState -> JlinkCodebaseState
    addClass cl (JCS cMap scMap) =
      JCS (M.insert (className cl) cl cMap)
          (foldr addToSuperclass scMap
             (maybeToList (superClass cl)++classInterfaces cl))
      where addToSuperclass super m =
              M.alter (\subclasses -> case subclasses of
                                        Just list -> Just (cl : list)
                                        Nothing -> Just [cl])
                      super
                      m

    -- Combine two @IO (Maybe a)@ computations lazily, choosing the
    -- first to succeed (i.e. return 'Just').
    firstSuccess :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
    -- This seems like it would be a common pattern, although I can't
    -- find it in the standard libraries.
    firstSuccess ima1 ima2 = do
      ma1 <- ima1
      case ma1 of
        Nothing -> ima2
        Just _ -> return ma1
