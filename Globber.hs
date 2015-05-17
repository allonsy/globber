module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: String -> String -> Bool
matchGlob [] [] = True
matchGlob [] (a:as) = False
matchGlob (a:as) []
    | a == '*' && as == [] = True
    | otherwise = False
matchGlob (a:as) (b:bs)
    | a =='\\' = matchLiteral as (b:bs)
    | a == '?' = matchGlob as bs
    | a == '*' = if matchGlob as (b:bs)
                then True
                else matchGlob (a:as) bs
    | a == b = matchGlob as bs
    | otherwise = False where
        matchLiteral (a:as) (b:bs)
            | a==b = matchGlob as bs
            | otherwise = False
