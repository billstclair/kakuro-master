----------------------------------------------------------------------
--
-- Puzzles.elm
-- "Database" of Kakuro puzzles
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
-- These were generated by ../krazydad/krazydad.hs, with:
--   cd ../krazydad
--   ghci
--   :l krazydad.hs
--   putPuzzleSpecs 100 firstPuzzle
-- The returned next Puzzle was:
--   Puzzle {puzzleKind = Kind8, puzzleVolume = 7, puzzleBook = 4, puzzleNumber = 1}
--
----------------------------------------------------------------------

module Puzzles exposing
  (puzzles
  )

puzzles : List (Int, Int, Int, Int, String) -- [(kind, volume, book, number, spec)]
puzzles =
  [ (6, 1, 1, 1, ".38.31.13.12..139..798..31.92.92.21."),
    (8, 1, 1, 1, "421..39.6982715393.19.61.59..13..71..34.21.31.7994187526.83..412"),
    (10, 1, 1, 1, ".79..21....183769.2413.18...3126..931.62.59..1295..4312..87.89.369..4912...37.8728.687931....95..59."),
    (6, 2, 1, 1, "96....89..21.7658918249.24..45....12"),
    (8, 2, 1, 1, "..974...72431.4191..2798.93.79....14.17.1298..9489.96251...721.."),
    (10, 2, 1, 1, "91..41.98.854193276...94.73...84..82143.31.39.897..896.69.97.27561..81...18.12...627985314.31.79..31"),
    (6, 3, 1, 1, "71....923817..57891296..857912....79"),
    (8, 3, 1, 1, "31.13.2189627453.32.12..24.12.7998.68.58..98.79.8175362997.31.13"),
    (10, 3, 1, 1, "...79..917..718365249584.19.3921.92..96.83.31..89..48..96.87.29..51.4949.16.945816259378..279..68..."),
    (6, 4, 1, 1, "91..13821349.427....381.16897598..59"),
    (8, 4, 1, 1, "..795....7986.2189..714312.982....679.873124..6197.6719....897.."),
    (10, 4, 1, 1, ".....14.5872.1325.3981752..17..89.73842.86...19.4193.38...98.53192.91..71..1276484.9235.8969.21....."),
    (6, 5, 1, 1, "..19...39857.78.1298.14.39748...93.."),
    (8, 5, 1, 1, "12...61.398..98..71.91..98.6231571598.31..78.95..18..521.29...96"),
    (10, 5, 1, 1, "35.29.98..98.3897645.71..82.89793....86.23.398152..451832.12.62....23981.12..97.9237415.97..18.29.54"),
    (6, 6, 1, 1, "....6417..1238217..6132919..3831...."),
    (8, 6, 1, 1, "13.21.5921498367.48..98.79....3985....14.23..87.7651294898.79.21"),
    (10, 6, 1, 1, "...51.27...2613.1329.52..89.15.13425.24.13.96..351987..59.92.49.19738.19.25..16.2761.3197...93.42..."),
    (6, 7, 1, 1, "...92.62183.865.8997.945.98712.53..."),
    (8, 7, 1, 1, "85..89..97..217.71.89.98.812.951123.761.29.29.21.621..42..97..38"),
    (10, 7, 1, 1, "87..91.79.921853.15...42..1342.13..73.39.49.18.31..87.89.56.82.97..12.7985..87...36.187293.79.39..31"),
    (6, 8, 1, 1, "29..89123457..21....57..52187313..98"),
    (8, 8, 1, 1, ".91..17..728456913.92.18251..12..42..83192.21.597186324..89..18."),
    (10, 8, 1, 1, ".12.98.12.794.31.69.98.986..17.283..7239.79.8291....2793.98.2819..182.19..879.79.31.51.398.68.95.16."),
    (6, 9, 1, 1, ".15.97.89.72..798..981..13.89.56.51."),
    (8, 9, 1, 1, "97...16.65.2134..3192.98.89...7972...21.94.2135..9483.31.12...89"),
    (10, 9, 1, 1, "..192.79...25761389481...24.6296.91...81.5283..23..79..8967.13...12.4929.81...14483965712...17.798.."),
    (6, 1, 2, 1, "29.61.12345...17....48...12983.85.71"),
    (8, 1, 2, 1, ".21.....9132.87983.97218.89.97....13.32.85697.59512.9214.....53."),
    (10, 1, 2, 1, "21..91....928163..31..92..7125.1234567..13..13..4132..92..93..9561423.8679..21..71..421853....83..91"),
    (6, 2, 2, 1, "18..75319.61.9718..6879.97.21882..26"),
    (8, 2, 2, 1, "96.71.7982194765.92..54....123....239....68..79.3978521418.79.69"),
    (10, 2, 2, 1, "....45.893.37.986721.79..92.3241328..71.95.698.98..61.231.61.23..9684749.59..65.289145.97.146.89...."),
    (6, 3, 2, 1, "87....13..97.9231878124.12..28....79"),
    (8, 3, 2, 1, "92.32.7154319762.17.52.....31......48.....71.29.5196238495.21.41"),
    (10, 3, 2, 1, ".15.19.....39657.61...21.8942661..327.8923619.18....78.4891242.318..6991428.69...31.29786.....31.61."),
    (6, 4, 2, 1, "83....916.79.2914856829.79.489....14"),
    (8, 4, 2, 1, "98..142.4217356...29..59.83...1573...23.92..61...7589416.129..32"),
    (10, 4, 2, 1, "96.89..93.89347..31..41..241...82195743.71.35...2183...17.63.72695831...412..95..31..17843.95..59.71"),
    (6, 5, 2, 1, "...312.14523.39.6989.18.46789.279..."),
    (8, 5, 2, 1, "79.32.2386957241.861.19....29......48....19.289.5247168993.97.16"),
    (10, 5, 2, 1, "21.49.....74.124..21.297.31592..829.41..19.68.93142719.76.29..23.921..59681.792.18..653.92.....98.81"),
    (6, 6, 2, 1, "589.15367958..53....68..12345779.189"),
    (8, 6, 2, 1, ".17..96.8496512.32.81.79..69.498987.39..72.98.12.1276349.74..79."),
    (10, 6, 2, 1, ".18..14.97.69175843289.69.71..173...9875.21.....3123.....39.7852...762..79.89.13492831675.12.79..94."),
    (6, 7, 2, 1, "78..39379612.182....136.39412717..79"),
    (8, 7, 2, 1, "91..13..52698173..48..21.32..21..79..53.78..83..29384165..17..21"),
    (10, 7, 2, 1, "27....13..159..38926.48.24..4893..98.98.21.96.96738217.17.98.63.19..5118..53.71.59478..961..98....39"),
    (6, 8, 2, 1, "..198..1234.89..6112..72.7189..869.."),
    (8, 8, 2, 1, "..86.13..431.28621.21.43931.381..283.96189.81.98189.837..51.91.."),
    (10, 8, 2, 1, "...95..14...37215894169..31.5338.21..67.97.13.193..974.12.68.41..79.2412.12..98773986524...12..95..."),
    (6, 9, 2, 1, "..681...8957.89.3913.12.5412...987.."),
    (8, 9, 2, 1, "58..92..123.316..197..7113.53.8239.42.9626..142..712.639..31..17"),
    (10, 9, 2, 1, ".18...98...297.5321.13.182.93137..94..689815..152..978..794196..91..53852.127.87.4138.197...79...89."),
    (6, 1, 3, 1, ".24.98.96185..72....84..14938.39.59."),
    (8, 1, 3, 1, "31..14..45623978..86..12.21..12..79..89.94..15..21953784..87..32"),
    (10, 1, 3, 1, "15.....27132.....793.12.24135.69347851..18.89..49..34..79.51..91627843.98341.93.987.....13271.....75"),
    (6, 2, 3, 1, ".13....2186.13.798541.31.5921....97."),
    (8, 2, 3, 1, "..37....35981.8312.97.71.98.879..142.85.73.89.1694.41269....89.."),
    (10, 2, 3, 1, "97..79....83..162.49.83.98341289472.51..15.94..897261..61.73..39.97321192348.98.27.123..17....97..69"),
    (6, 3, 3, 1, "..23...2179858..1221..6949128...31.."),
    (8, 3, 3, 1, "81...62.921..81..589.98.17.85.4236.71.59.87.689..98..178.49...61"),
    (10, 3, 3, 1, "..216..79..56893127.921.852.1281.17...29..895..18..19..149..97...49.2982.439.218.65218437..49..798.."),
    (6, 4, 3, 1, "..721.27986.48.98..25.21.18293.597.."),
    (8, 4, 3, 1, "...13.8138.21.981234567...89.32..61.92...928716313.61.3198.98..."),
    (10, 4, 3, 1, "89.97..18.97.86.5218.281.51.79..3219754.....34.13..59.21.....8734219..79.49.279.8795.29.29.18..18.14"),
    (6, 5, 3, 1, ".26.75.14793.496....892.73241.12.84."),
    (8, 5, 3, 1, ".81.71...92.97....9586279278..1984..2138512674....49.98...13.21."),
    (10, 5, 3, 1, "....61.398.62.96.162.316.93.2165.91.987.37..21.14..42.42..87.918.98.5912.39.123.312.12.31.981.29...."),
    (6, 6, 3, 1, "..689...1923.59.1797.16.1286...142.."),
    (8, 6, 3, 1, ".....93.97.3641.58912.68..62.321198.21..26.29871.8613.92.31....."),
    (10, 6, 3, 1, "493..97.62241.512.51..231..14...492..39.439.89..1498..36.289.29..589...12..214..57.821.17519.61..398"),
    (6, 7, 3, 1, "58..9814..31.6321..7938.92..7989..28"),
    (8, 7, 3, 1, "..39..12.9128.8916..127.483.36....76.182.148..9763.9271.12..14.."),
    (10, 7, 3, 1, "16...98...243.384.12..812.72619713..98..89.612..9861..798.12..23..15795219.431..98.897.712...18...89"),
    (6, 8, 3, 1, "71..21859.93..187..923..38.13512..98"),
    (8, 8, 3, 1, "..12..18..36182921..89..92.7641..3142.71..49..92592876..18..98.."),
    (10, 8, 3, 1, ".39..21.21.27965841394.51.793.8193.26.....68.459....843.48.....21.3214.897.79.29895173264.97.69..86."),
    (6, 9, 3, 1, "89..1296.123.873....421.135.2939..31"),
    (8, 9, 3, 1, "..12..12..97..41914..12.62.2435189217.74.61..43227..29..98..18.."),
    (10, 9, 3, 1, "14.38..12.52.14..98..31.69.41.812.1682..96.12.231..983.73.87..1243.893.97.21.14..89..83.37.12..41.29"),
    (6, 1, 4, 1, "87.17.46789...15....27...18967.59.15"),
    (8, 1, 4, 1, ".31...64.76..197.986.27.31.97.3962.58.18.62.978.381..12.14...95."),
    (10, 1, 4, 1, ".175.21.12.897.98123...821.93...8917....2351.62.5971.25.9718....4312...81.983...59821.698.16.98.879."),
    (6, 2, 4, 1, "14....95.679.1782312893.239.49....13"),
    (8, 2, 4, 1, "13...78.75.3124..1286.94.81...6117...45.29.9683..6987.29.25...13"),
    (10, 2, 4, 1, ".21.94..81.98.127594..98.186....71..418989.7589.2896.3291.174862..59....895.67..189627.12.29..19.29."),
    (6, 3, 4, 1, "...13..2179.942.7971.128.7231..39..."),
    (8, 3, 4, 1, "..12.86.6897.94.29.92.98.79.96315213.85.16.21.89.59.4273.31.59.."),
    (10, 3, 4, 1, "....13.912.478692351.896.21......41.819.13.53.96782189.86.46.217.14......39.371.326154798.198.82...."),
    (6, 4, 4, 1, "..38..32149.18..1221..86.34127..93.."),
    (8, 4, 4, 1, ".28..31.4367152.21.12...187.39....32.497...14.28.1258769.79..98."),
    (10, 4, 4, 1, "31..75..69869.61..97..63.25.316179..157.98.86297....75312.39.791..791269.28.31..18..71.71224..97..31"),
    (6, 5, 4, 1, "..124.89341.97.79..15.27.29738.869.."),
    (8, 5, 4, 1, "...71.39.7984315.21.94..98...729798...13..12.13.8597136.98.98..."),
    (10, 5, 4, 1, "89..21..49928.932815..67..193....86...21.47916.958129.29586.37...87....128..97..534981.21879..92..39"),
    (6, 6, 4, 1, ".38....1982414.973968.9232148....26."),
    (8, 6, 4, 1, "..13..71.256849721..69..13...312891...34..31..896159432.42..79.."),
    (10, 6, 4, 1, "..89...97..893...197162..79.89497238561.32.79..25..48..12.49.39875612495.59..968213...123..79...87.."),
    (6, 7, 4, 1, "12....39..89.1326827139.68..31....49")
  ]