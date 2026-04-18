-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesFileExist,
    doesDirectoryExist,
    listDirectory,
  )
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  -- apply @processFile@ to each path in @fullPaths@ together with @recursive@
  let lists = mapM (processFile . (, recursive)) fullPaths
  -- flatten @IO [[TestCaseFile]]@ into @IO [TestCaseFile]@
  concat <$> lists


-- | Process a file path, returning zero or more 'TestCaseFile's.
--
-- If the path is a directory and @recursive@ is 'True', recursively
-- runs 'discoverTests' on that directory. If the path is a @.test@
-- file, builds a 'TestCaseFile' using 'findCompanionFiles'.
-- Otherwise returns an empty list.
processFile :: (FilePath, Bool) -> IO [TestCaseFile]
processFile (path, recursive) = do
  isDir <- doesDirectoryExist path
  let isTest = takeExtension path == ".test"
  if isDir && recursive
    then discoverTests True path
  else
    if isTest then do
      scf <- findCompanionFiles path
      return [scf]
    else return []


-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
