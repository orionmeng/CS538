{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_warmup (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "warmup"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Programming Assignment 4 for CS 538"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
