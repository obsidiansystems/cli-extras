{-# LANGUAGE TemplateHaskell #-}
module Bindings.Cli.Coreutils where

import System.Which (staticWhich)

cp :: FilePath
cp = $(staticWhich "cp")
