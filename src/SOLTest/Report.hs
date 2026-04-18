-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.List (find)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions =
  -- Itrate througn all reports and generate per category map
  Map.foldlWithKey' (categorizeOneResult definitions) Map.empty

-- | Returns @acc@ updated with values of @report@.
-- If @testName@ is not defined in @deffinitions@, whole @report@ will be ignored.
categorizeOneResult :: [TestCaseDefinition] -> Map String CategoryReport -> String -> TestCaseReport -> Map String CategoryReport
categorizeOneResult definitions acc testName report =
  -- Find appropriate TestCaseDefinition, in case of failure, ignore given report
  case find (\def -> tcdName def == testName) definitions of
    Just def ->
      let cat = tcdCategory def
          points = tcdPoints def
          passedPoints = case tcrResult report of
            Passed -> points
            _ -> 0
          -- Create tmp CategoryReport containig data for given report for further use
          rep = CategoryReport {crTotalPoints=points, crPassedPoints=passedPoints, crTestResults=Map.fromList [(testName, report)]}
      in
        -- Insert tmp CategoryReport to acumulator CategoryReport of given category
        Map.insertWith joinCategoryReports cat rep acc
    Nothing -> acc

-- | Inserts values from CategoryRepor @result@ into @acc@
joinCategoryReports :: CategoryReport -> CategoryReport -> CategoryReport
joinCategoryReports result acc =
  CategoryReport {
    crTotalPoints = crTotalPoints result + crTotalPoints acc,
    crPassedPoints = crPassedPoints result + crPassedPoints acc,
    crTestResults = Map.union (crTestResults result) (crTestResults acc)
  }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults =
  let (results, count) = case mCategoryResults of
        Just res ->
          -- Sum of each category test count
          let cnt = sum [ countTestsPerCategory cr | cr <- Map.elems res ]
          in (res, cnt)
        Nothing -> (Map.empty, 0)
  in
    TestStats {
      tsFoundTestFiles=foundCount,
      tsLoadedTests=loadedCount,
      tsSelectedTests=selectedCount,
      tsPassedTests=count,
      tsHistogram=computeHistogram results
    }

-- | Count number of Passed test in CategoryReport @cr@
countTestsPerCategory :: CategoryReport -> Int
countTestsPerCategory cr =
  Map.size (Map.filter (\r -> tcrResult r == Passed) (crTestResults cr))

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram categories =
  -- Create empty histogram
  let hist = Map.fromList [ (rateToBin (x / 10), 0 :: Int) | x <- [0..9]]
  in
    -- Iterate through each category from @categories@ and add it's resulting score to histogram
    Map.foldlWithKey' (\acc _ cat -> evalOneCategory acc cat) hist categories

-- | Extends @hist@ with resulting score of @cat@
evalOneCategory :: Map String Int -> CategoryReport -> Map String Int
evalOneCategory hist cat =
  -- Increase appropriate bin by one
  let total = Map.size (crTestResults cat)
      passed = Map.size (Map.filter (\r -> tcrResult r == Passed) (crTestResults cat))
      ratio = if total == 0
        then 0
        else fromIntegral passed / fromIntegral total
  in Map.insertWith (+) (rateToBin ratio) 1 hist

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
