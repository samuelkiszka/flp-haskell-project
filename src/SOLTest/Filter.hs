-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import Data.List (partition)
import SOLTest.Types
import Text.Regex.TDFA ((=~))

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests =
  -- Extract include and exclude filters from the spec
  let fsIncl = fsIncludes spec
      fsExcl = fsExcludes spec
      useRegex = fsUseRegex spec
      -- Split tests into included and remaining
      -- based on include filters
      (included, remaining) =
        if null fsIncl
          then (tests, [])
          else partition (matchesAny useRegex fsIncl) tests

      -- Split included tests into selected and filtered-out
      -- based on exclude filters
      (selected, filteredOut) = partition (not . matchesAny useRegex fsExcl) included
  in
    (selected, filteredOut ++ remaining)

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion useRegex test criterion =
  -- Extract name, category and tags from the test
  let name = tcdName test
      category = tcdCategory test
      tags = tcdTags test
  in
  -- Apply matching based on the specific filter criterion
  case criterion of
    ByAny pattern -> matchesAnyString useRegex pattern (tags ++ [name] ++ [category])
    ByCategory pattern -> matchesAnyString useRegex pattern [category]
    ByTag pattern -> matchesAnyString useRegex pattern tags

-- | Return True if the given pattern matches any of the provided strings
matchesAnyString :: Bool -> String -> [String] -> Bool
matchesAnyString useRegex pattern = any (matchesSingleString useRegex pattern)

-- | Return True if the pattern matches the given string.
-- Matching uses regex if @useRegex@ is True, otherwise exact equality.
matchesSingleString :: Bool -> String -> String -> Bool
matchesSingleString useRegex pattern opt =
  let trimOpt = trimFilterId opt
  in
    -- Choose matching strategy based on @useRegex@
    if useRegex
      then trimOpt =~ pattern 
      else trimOpt == pattern

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
