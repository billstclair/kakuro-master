----------------------------------------------------------------------
--
-- Entities.elm
-- HTML entities, for fun and profit.
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Entities exposing (..)

import String
import Char

nbsp : String
nbsp = String.cons (Char.fromCode 160) "" -- \u00A0

copyright: String
copyright = String.cons (Char.fromCode 169) ""
