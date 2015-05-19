{- Alec Snyder
- hw10 Globber
- github repo: https://github.com/allonsy/globber
-}
module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob [] [] = True --empty match
matchGlob [] (a:as) = False --empty Glob doesn't match non-empty string
matchGlob (a:as) [] --for non-null glob, only the asterisk matches empty string
    | a == '*' && as == [] = True  
    | otherwise = False
matchGlob (a:as) (b:bs)
    | a =='\\' = matchLiteral as (b:bs) --escape the next character as a literal
    | a == '?' = matchGlob as bs --question mark
    | a == '*' = if matchGlob as (b:bs) --if whatever follows the asterisk is a match, then true
                then True
                else matchGlob (a:as) bs --otherwise, discard one letter from the target string and try again
    | a == b = matchGlob as bs --match literal character (non-special)
    | otherwise = False where --catchall false
        matchLiteral [] (b:bs) = False --matches a literal character (used for escaped characters)
        matchLiteral (a:as) (b:bs)
            | a==b = matchGlob as bs
            | otherwise = False
