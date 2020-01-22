module String.Sort exposing (sort)

{-| Sort:
Additional String manipulation function to get you working fast

# Sorting
@docs sort
-}

{-
Copyright (C) 2020 hyphen <[redacted]>
License: MIT
-}

import String exposing (uncons, filter, fromChar)
import List   exposing (head)

{-| Sort a string's characters by ASCII value (quicksort algorithm)
sort "" == ""
sort "A" == "A"
sort "CBADREWQ" == "ABCDEQRW"
-}
sort : String -> String
sort str =
    case str of
        "" ->
            ""
        _ ->
            let
                pxs =
                    case uncons str of
                    Just (c,s) -> (c,s)
                    Nothing -> ('_', "_") -- Flow never goes here

                (p, xs) = pxs

                lower =
                    filter ((>=) p) xs

                upper =
                    filter ((<) p) xs
            in
                sort lower ++ fromChar p ++ sort upper
