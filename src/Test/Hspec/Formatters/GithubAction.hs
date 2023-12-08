{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a utility to allow you to have annotations on
-- test failures in GitHub Actions.
--
-- To use this, you'll call 'registerGithubActionsAnnotations' with
-- a 'Formatter' of your choice.
--
-- @
-- main :: IO ()
-- main = do
--     let hspecConfig =
--             'registerGithubActionsAnnotations' 'checks' 'defaultConfig'
--     'hspecWith' hspecConfig spec
-- @
--
-- This will make the @github@ formatter available.
--
-- Then, to actually use the formatter, you can provide the argument
-- @--format=github@ on the command line or in a @.hspec@ file, or the
-- @HSPEC_FORMAT=github@ environment variable.
--
-- To do this locally with @cabal@, you must use @cabal run@, as @cabal
-- test@ evidently does not allow you to pass @--test-arguments@ anymore?
--
-- > $ cabal run $(test-target-name) -- --format=github
--
-- The environment variable is easier to set in CI, probably.
--
-- > $ HSPEC_FORMAT=github cabal test
--
-- And you can use a file, if you really want to, for some reason.
--
-- > $ echo "--format=github" >> .hspec
-- > $ cabal test
module Test.Hspec.Formatters.GithubAction
    ( registerGithubActionsAnnotations
    , withGithubActionFormatter
    -- * Re-exported 'Formatter'
    , silent
    , checks
    ) where

import Test.Hspec.Formatters.GithubAction.Internal
import Test.Hspec.Api.Formatters.V2

-- | This function adds the GitHub Annotations to the underlying
-- 'Formatter'
registerGithubActionsAnnotations :: Formatter -> Config -> Config
registerGithubActionsAnnotations format =
    registerFormatter ("github", withGithubActionFormatter format)

