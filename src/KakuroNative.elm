----------------------------------------------------------------------
--
-- KakuroNative.elm
-- Interface to native functions
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module KakuroNative exposing
  ( setTitle
  , sha256
  )

import Native.Kakuro

setTitle: String -> String
setTitle =
  Native.Kakuro.setTitle

sha256: String -> String
sha256 =
  Native.Kakuro.sha256
