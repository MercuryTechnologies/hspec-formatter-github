module Main where

import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Formatters.GithubAction

main :: IO ()
main = do
    hspecWith (registerGithubActionsAnnotations checks defaultConfig) $ do
        describe "it works" $ do
            it "unmark pending to test" $ do
                pendingWith "test is pending by default"
                expectationFailure "oh no"


