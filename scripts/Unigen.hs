-- runghc Unigen.hs ∣ xmodmap -

module Main where

import Data.Char
import Numeric
import Data.List

type Mod = (Int,Char,Char,Char,Char)



rows :: [Mod]
rows = concat
   [ zip5 [10..19]
         "1234567890"
         "!@#$%^&*()"
         "₁₂₃₄₅₆₇₈₉₀"
         "¹²³⁴⁵⁶⁷⁸⁹⁰"
   , zip5 [24..35]
         "',.pyfgcrl/="
        "\"<>PYFGCRL?+"
         "′«»⅋∧∀γ⊗→λ≟≡"
         "″⟨⟩Π ≗Γ⨂ρ ∣∅⁺"
   , zip5 [38..49]
         "aoeuidhtns-"
         "AOEUIDHTNS_"
         "åäöε∈δ‽⊤≢σ⊖"
         "ÅÄÖ  Δ   Σ⁻"
   , zip5 [52..61]
         ";qjkxbmwvz"
         ":QJKXBMWVZ"
         "α∎∘∙∃β⊥⊸∨⊕"
         "∷①⓪≤≥   ＆⨁"
   ] ++
   [ mk 20 "[{⟦⟦"
   , mk 21 "]}⟧ "
   , mk 51 "\\|λ "
   ]

mk :: Int -> String -> Mod
mk x [a,b,c,d] = (x,a,b,c,d)
mk x s = error $ show x ++ " should be associated with four characters, not " ++ show s

main :: IO ()
main = putStrLn (concatMap make rows)

make :: Mod -> String
make (n,a,b,c,d) =
    unwords ("!":map return abcd) ++ "\n" ++
    "keycode " ++ show n ++ " = " ++ unwords (map sh abcd) ++ "\n"
  where
    abcd = [a,b,c,d]

sh :: Char -> String
sh c
    | isAsciiLower c || isAsciiUpper c = [c]
    | otherwise                        = 'U':map toUpper (showHex (ord c) "")
