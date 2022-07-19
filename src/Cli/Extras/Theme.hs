{-# LANGUAGE OverloadedStrings #-}
module Cli.Extras.Theme where

import Data.Text (Text)

data CommandLineTheme = CommandLineTheme
  { _commandLineTheme_done :: Text
  , _commandLineTheme_failed :: Text
  , _commandLineTheme_arrow :: Text
  , _commandLineTheme_spinner :: SpinnerTheme
  }

type SpinnerTheme = [Text]

unicodeTheme :: CommandLineTheme
unicodeTheme = CommandLineTheme
  { _commandLineTheme_done = "✔"
  , _commandLineTheme_failed = "✖"
  , _commandLineTheme_arrow = "⇾"
  , _commandLineTheme_spinner = ["◐", "◓", "◑", "◒"]
  }

noUnicodeTheme :: CommandLineTheme
noUnicodeTheme = CommandLineTheme
  { _commandLineTheme_done = "DONE"
  , _commandLineTheme_failed = "FAILED"
  , _commandLineTheme_arrow = "->"
  , _commandLineTheme_spinner = ["|", "/", "-", "\\"]
  }
