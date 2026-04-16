-- | Executing test cases by running external parser and interpreter processes.
--
-- Each test case is executed according to its 'TestCaseType':
--
-- * 'ParseOnly': run the parser with source on stdin, check exit code.
-- * 'ExecuteOnly': write XML to a temp file, run the interpreter, check
--   exit code, optionally diff stdout against @.out@.
-- * 'Combined': run the parser first (must exit 0), write its output to a
--   temp file, then run the interpreter as in 'ExecuteOnly'.
module SOLTest.Executor
  ( executeTest,
    runParser,
    runInterpreter,
    runDiff,
  )
where

import Control.Exception (IOException, try)
import Data.Maybe (fromMaybe)
import SOLTest.Types
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Process (proc, readCreateProcessWithExitCode)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Execute a single test case and return a @TestCaseReport@.
--
-- Returns @Left UnexecutedReason@ when execution cannot proceed (e.g.
-- the required executable is missing or not executable).
executeTest ::
  -- | Path to the parser executable (required for 'ParseOnly' and 'Combined').
  Maybe FilePath ->
  -- | Path to the interpreter executable (required for 'ExecuteOnly' and 'Combined').
  Maybe FilePath ->
  TestCaseDefinition ->
  IO (Either UnexecutedReason TestCaseReport)
executeTest mParser mInterp test =
  case tcdTestType test of
    ParseOnly ->
      withExecutable mParser $ \parserPath ->
        Right <$> executeParseOnly parserPath test
    ExecuteOnly ->
      withExecutable mInterp $ \interpPath ->
        Right <$> executeExecuteOnly interpPath test
    Combined ->
      withExecutable mParser $ \parserPath ->
        withExecutable mInterp $ \interpPath ->
          Right <$> executeCombined parserPath interpPath test

-- ---------------------------------------------------------------------------
-- Per-type execution
-- ---------------------------------------------------------------------------

-- | Execute a 'ParseOnly' test case.
executeParseOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeParseOnly parserPath test = do
  (exitCode, pOut, pErr) <- runParser parserPath (tcdSourceCode test)
  let code = exitCodeToInt exitCode
      result
        | code `elem` expectedCodes = Passed
        | otherwise = ParseFail
      expectedCodes = fromMaybe [] (tcdExpectedParserExitCodes test)
  return
    TestCaseReport
      { tcrResult = result,
        tcrParserExitCode = Just code,
        tcrInterpreterExitCode = Nothing,
        tcrParserStdout = Just pOut,
        tcrParserStderr = Just pErr,
        tcrInterpreterStdout = Nothing,
        tcrInterpreterStderr = Nothing,
        tcrDiffOutput = Nothing
      }

-- | Execute an 'ExecuteOnly' test case.
executeExecuteOnly :: FilePath -> TestCaseDefinition -> IO TestCaseReport
executeExecuteOnly interpPath test =
  withTempSource (tcdSourceCode test) $ \tmpPath -> do
    (exitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
    let code = exitCodeToInt exitCode
        expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
    (result, diffOut) <- checkInterpreterResult code expectedCodes iOut (tcdExpectedStdoutFile test)
    return
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = Nothing,
          tcrInterpreterExitCode = Just code,
          tcrParserStdout = Nothing,
          tcrParserStderr = Nothing,
          tcrInterpreterStdout = Just iOut,
          tcrInterpreterStderr = Just iErr,
          tcrDiffOutput = diffOut
        }

-- | Execute a 'Combined' test case.
--
-- FLP: Implement this function. You'll use @withTempSource@ here.
executeCombined :: FilePath -> FilePath -> TestCaseDefinition -> IO TestCaseReport
executeCombined parserPath interpPath test = do
  parsRes <- executeParseOnly parserPath test
  -- based on parser result, interpreter is either run or not
  case tcrParserExitCode parsRes of
    Just 0 ->
      withTempSource (fromMaybe "" (tcrParserStdout parsRes)) $ \tmpPath -> do -- TODO zkontrolovat, zda fromMaybe může být tady
        (exitCode, iOut, iErr) <- runInterpreter interpPath tmpPath (tcdStdinFile test)
        let code = exitCodeToInt exitCode
            expectedCodes = fromMaybe [] (tcdExpectedInterpreterExitCodes test)
        (result, diffOut) <- checkInterpreterResult code expectedCodes iOut (tcdExpectedStdoutFile test)
        return (mkReport parsRes result (Just code) (Just iOut) (Just iErr) diffOut)
    _ ->
      return (mkReport parsRes (tcrResult parsRes) Nothing Nothing Nothing Nothing)

  where
    -- | Helper function, that builds @TestCaseReport@ based on results of parser and interpret execution
    mkReport :: TestCaseReport -> TestResult -> Maybe Int -> Maybe String -> Maybe String -> Maybe String -> TestCaseReport
    mkReport parsRes result iExit iOut iErr diff =
      TestCaseReport
        { tcrResult = result,
          tcrParserExitCode = tcrParserExitCode parsRes,
          tcrInterpreterExitCode = iExit,
          tcrParserStdout = tcrParserStdout parsRes,
          tcrParserStderr = tcrParserStderr parsRes,
          tcrInterpreterStdout = iOut,
          tcrInterpreterStderr = iErr,
          tcrDiffOutput = diff
        }


-- ---------------------------------------------------------------------------
-- Process wrappers
-- ---------------------------------------------------------------------------

-- | Run the SOL26 parser by feeding @sourceCode@ on its stdin.
--
-- Returns @(exitCode, stdout, stderr)@.
runParser :: FilePath -> String -> IO (ExitCode, String, String)
runParser parserPath = readCreateProcessWithExitCode (proc parserPath [])

-- | Run the interpreter with @--source \<xmlFile\>@ and, optionally,
-- @--input \<stdinFile\>@.
--
-- Returns @(exitCode, stdout, stderr)@.
runInterpreter ::
  FilePath ->
  FilePath ->
  Maybe FilePath ->
  IO (ExitCode, String, String)
runInterpreter interpPath xmlFile mInputFile = do
  let args = ["--source", xmlFile] ++ maybe [] (\f -> ["--input", f]) mInputFile
  readCreateProcessWithExitCode (proc interpPath args) ""

-- | Run GNU @diff@ between two files (no additional flags).
--
-- Returns @(exitCode, diffOutput)@. Exit code 0 means no differences;
-- exit code 1 means differences were found.
runDiff :: FilePath -> FilePath -> IO (ExitCode, String)
runDiff actualFile expectedFile = do
  (exitCode, out, _) <- readCreateProcessWithExitCode (proc "diff" [actualFile, expectedFile]) ""
  return (exitCode, out)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Check the interpreter's result and optionally run diff.
--
-- Runs diff only when the interpreter exited with code 0 AND a @.out@ file
-- is present.
--
-- FLP: Implement this function.
checkInterpreterResult ::
  -- | Actual interpreter exit code.
  Int ->
  -- | Expected interpreter exit codes.
  [Int] ->
  -- | Interpreter stdout.
  String ->
  -- | Path to the @.out@ file, if present.
  Maybe FilePath ->
  IO (TestResult, Maybe String)
checkInterpreterResult actualCode expectedCodes iOut mOutFile = do
  case mOutFile of
    -- continue, just if mOutFile exists, else do not care for diff at all
    Just path -> do
      let success = actualCode == 0
      exists <- doesFileExist path
      if success && exists then do
        -- get output of the diff utility
        (difRess, diffOut) <- runDiffOnOutput iOut path
        return (difRess, diffOut)
      else return (compareActualVsExpected actualCode expectedCodes)
    Nothing ->
      return (compareActualVsExpected actualCode expectedCodes)

-- | Helper function for result building, so it is not repeated in the main function.
-- Returns success if @actualCode@ occur in @expectedCodes@. Otherwise returns failure.
compareActualVsExpected :: Int -> [Int] -> (TestResult, Maybe String)
compareActualVsExpected actualCode expectedCodes =
  if actualCode `elem` expectedCodes then
    (Passed, Nothing)
  else
    (IntFail, Nothing)
    

-- | Write a string to a temporary file and pass its path to an action.
-- The file is deleted when the action returns.
withTempSource :: String -> (FilePath -> IO a) -> IO a
withTempSource content action =
  withSystemTempFile "sol-source.xml" $ \tmpPath tmpHandle -> do
    hPutStr tmpHandle content
    hClose tmpHandle
    action tmpPath

-- | Write the interpreter stdout to a temp file and diff it against @.out@.
-- The file is deleted when the action returns.
--
-- FLP: Implement this function. It will start similarly to @withTempSource@.
runDiffOnOutput :: String -> FilePath -> IO (TestResult, Maybe String)
runDiffOnOutput iOut outFile = undefined

-- | Ensure an executable path is provided and the file is executable,
-- then run an action with it.  Returns 'Left' 'CannotExecute' if the
-- path is missing or the file is not executable.
withExecutable ::
  Maybe FilePath ->
  (FilePath -> IO (Either UnexecutedReason TestCaseReport)) ->
  IO (Either UnexecutedReason TestCaseReport)
withExecutable Nothing _ =
  return
    ( Left
        UnexecutedReason
          { urCode = CannotExecute,
            urMessage = Just "Required executable path was not provided"
          }
    )
withExecutable (Just path) action = do
  check <- checkExecutable path
  case check of
    Just reason -> return (Left reason)
    Nothing -> action path

-- | Check that a file exists and has its executable bit set.
-- The IO action returns 'Nothing' if the file is usable, or 'Just'
-- an 'UnexecutedReason' describing the problem.
--
-- FLP: Implement this function. The following functions may come in handy:
--      @doesFileExist@, @getPermissions@, @executable@
checkExecutable :: FilePath -> IO (Maybe UnexecutedReason)
checkExecutable path = do
  result <- try (doesFileExist path) :: IO (Either IOException Bool)
  case result of
    Left err -> return (Just (UnexecutedReason CannotExecute (Just (show err))))
    Right False -> undefined -- ???
    Right True -> undefined -- ???
  return Nothing -- this probably won't be here

-- | Convert 'ExitCode' to an 'Int'.
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
