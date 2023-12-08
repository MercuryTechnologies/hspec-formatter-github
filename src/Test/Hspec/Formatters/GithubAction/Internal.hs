{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module is internal! Use at your own risk. Breaking changes to
-- this module will not necessarily be reflected in PVP versions.
module Test.Hspec.Formatters.GithubAction.Internal where

import Prelude
import Control.Applicative
import Test.Hspec.Core.Util (joinPath)
import Test.Hspec.Api.Formatters.V2

-- | This option enhances a given 'Formatter' with annotations that will
-- show up on the correct test item failure. This allows you to use the
-- test output you want, and additionally get Github Action annotations.
withGithubActionFormatter :: Formatter -> Formatter
withGithubActionFormatter fmtr = fmtr
  { formatterItemDone = \path item -> do
      formatterItemDone fmtr path item
      emitGithubActionAnnotation path item
  }

-- | A representation of the Github Actions error format.
data ErrorCommand = ErrorCommand
    { title   :: Maybe String
    , file    :: Maybe String
    , line    :: Maybe Int
    , col     :: Maybe Int
    , message :: String
    }

-- | Make a suitable error annotation from an hspec failure.
--
-- Not clear what to do with the Maybe Location here: do we use the one from
-- the Item, or this one? What if both or neither are available?
-- Also not clear whether to use itemInfo.
errorCommandFor :: Path -> Item -> Maybe Location -> FailureReason -> ErrorCommand
errorCommandFor specPath@(_nesting, requirement) item mFailureLocation reason = ErrorCommand
    { -- requirement is used because it should always be non-empty and meaningful
      title = Just requirement
    , file = locationFile   <$> mloc
    , line = locationLine   <$> mloc
    , col  = locationColumn <$> mloc
    , message = unlines (messageHeaderLines ++ messageBodyLines)
    }
  where
    -- prefer the location of the actual failure. if that is not present,
    -- fall  back to the test definition location
    mloc = mFailureLocation <|> itemLocation item
    -- Use the path to give a message header, so that the message is never empty.
    -- Empty messages seem to cause github actions to ignore the annotation.
    messageHeaderLines :: [String]
    messageHeaderLines = joinPath specPath : if itemInfo item == "" then [] else [itemInfo item]

    messageBodyLines :: [String]
    messageBodyLines = case reason of
        NoReason -> []
        Reason str -> [str]
        ExpectedButGot preface expected actual ->
            let bodyLines =
                    [ mconcat ["expected: ", expected]
                    , mconcat ["     got: ", actual  ]
                    ]
                headerLines = maybe [] (\x -> [x]) preface
            in  headerLines ++ bodyLines
        Error preface err ->
            let bodyLines = [show err]
                headerLines = maybe [] (\x -> [x]) preface
            in  headerLines ++ bodyLines

-- | The github actions command format.
formatErrorCommand :: ErrorCommand -> String
formatErrorCommand ec = mconcat
    [ "::error "
    , "title=", maybe "" escapeProperty (title ec), ","
    , "file=", maybe "" escapeProperty (file ec), ","
    , "line=", maybe "" show (line ec), ","
    , "col=", maybe "" show (col ec)
    , "::"
    , escapeData (message ec)
    -- FIXME should check if on windows and use \r\n, as this is what the
    -- github library does.
    , "\n"
    ]

{-
function escapeData(s: any): string {
  return toCommandValue(s)
    .replace(/%/g, '%25')
    .replace(/\r/g, '%0D')
    .replace(/\n/g, '%0A')
}
function escapeProperty(s: any): string {
  return toCommandValue(s)
    .replace(/%/g, '%25')
    .replace(/\r/g, '%0D')
    .replace(/\n/g, '%0A')
    .replace(/:/g, '%3A')
    .replace(/,/g, '%2C')
}
-}

escapeData :: String -> String
escapeData = (>>= replace)
  where
    replace '%'  = "%25"
    replace '\r' = "%0D"
    replace '\n' = "%0A"
    replace c    = [c]

escapeProperty :: String -> String
escapeProperty = (>>= replace)
  where
    replace '%'  = "%25"
    replace '\r' = "%0D"
    replace '\n' = "%0A"
    replace ':'  = "%3A"
    replace ','  = "%2C"
    replace c    = [c]

-- | If you want to extend
emitGithubActionAnnotation :: Path -> Item -> FormatM ()
emitGithubActionAnnotation path item = case itemResult item of
    Success -> pure ()
    Pending _ _ -> pure ()
    Failure mLoc reason -> write . formatErrorCommand $ errorCommandFor path item mLoc reason
