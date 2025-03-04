{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion :: List Chars
illion =
  let preillion :: List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion :: List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit :: Digit -> Chars
showDigit Zero = "zero"
showDigit One = "one"
showDigit Two = "two"
showDigit Three = "three"
showDigit Four = "four"
showDigit Five = "five"
showDigit Six = "six"
showDigit Seven = "seven"
showDigit Eight = "eight"
showDigit Nine = "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 = D1 Digit
            | D2 Digit Digit
            | D3 Digit Digit Digit
            deriving Eq

-- Possibly convert a character to a digit.
fromChar :: Char -> Optional Digit
fromChar '0' = Full Zero
fromChar '1' = Full One
fromChar '2' = Full Two
fromChar '3' = Full Three
fromChar '4' = Full Four
fromChar '5' = Full Five
fromChar '6' = Full Six
fromChar '7' = Full Seven
fromChar '8' = Full Eight
fromChar '9' = Full Nine
fromChar _   = Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion
--  one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion
--  seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion
--  six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion
--  five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion
--  four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion
--  three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion
--  two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"

showDigit3 :: Digit3 -> Chars
showDigit3 = \case
    (D1 d1)            -> showDigit d1
    (D2 Zero d1)       -> showDigit d1
    (D2 One Zero)      -> "ten"
    (D2 One One)       -> "eleven"
    (D2 One Two)       -> "twelve"
    (D2 One Three)     -> "thirteen"
    (D2 One d1)        -> showDigit d1 ++ "teen"
    (D2 Two d1 )       -> "twenty" ++ showUnits d1
    (D2 Three d1 )     -> "thirty" ++ showUnits d1
    (D2 Four d1 )      -> "forty" ++ showUnits d1
    (D2 Five d1 )      -> "fifty" ++ showUnits d1
    (D2 Six d1 )       -> "sixty" ++ showUnits d1
    (D2 Seven d1 )     -> "seventy" ++ showUnits d1
    (D2 Eight d1 )     -> "eighty" ++ showUnits d1
    (D2 Nine d1 )      -> "ninety" ++ showUnits d1
    (D3 Zero d2 d1)    -> showDigit3 (D2 d2 d1)
    (D3 d3 Zero Zero)  -> showHundreds d3
    (D3 d3 d2 d1)      -> showHundreds d3 ++ " and " ++ showDigit3 (D2 d2 d1)
  where
    showHundreds Zero = ""
    showHundreds d    = showDigit d ++ " hundred"
    showUnits Zero    = ""
    showUnits d       = "-" ++ showDigit d

dollarBuildStr :: List (Chars, Digit3) -> Chars
dollarBuildStr = foldRight func "zero dollars"
  where
    func ("", d@(D1 One)) lst        = "one dollar"
    func ("", d@(D2 Zero One)) lst   = "one dollar"
    func ("", d@(D3 _ Zero One)) lst = showDigit3 d ++ " dollar"
    func ("", d) lst                 = showDigit3 d ++ " dollars"
    func (_, D1 Zero) lst            = lst
    func (_, D2 Zero Zero) lst       = lst
    func (_, D3 Zero Zero Zero) lst  = lst
    func (str, d) lst                = showDigit3 d ++ " " ++ str ++ " " ++ lst

centBuildStr :: Digit3 -> Chars
centBuildStr = \case
  d@(D2 Zero One) -> showDigit3 d ++ " cent"
  d               -> showDigit3 d ++ " cents"

centsToD3 :: List Digit -> Digit3
centsToD3 = \case
  Nil             -> D2 Zero Zero
  (d2 :. Nil)     -> D2 d2 Zero
  (d2 :. d1 :. _) -> D2 d2 d1

dollarsToD3Lst :: List Digit -> List Digit3
dollarsToD3Lst = foldRight func Nil
  where
    func d = \case
      (D1 d1) :. lst    -> (D2 d d1) :. lst
      (D2 d2 d1) :. lst -> (D3 d d2 d1) :. lst
      lst               -> (D1 d) :. lst

withGroupName :: List Digit3 -> List (Chars, Digit3)
withGroupName = reverse . zip illion . reverse

parseChars :: Chars -> (List Digit, List Digit)
parseChars lst = (reverse ds,reverse cs)
  where
    ((ds, cs), comma) = foldLeft f ((Nil,Nil),False) lst
    f rest@((dollars,cents), comma) c
      | c == '.'   = ((dollars,cents), True)
      | otherwise  = case (fromChar c, comma) of
                      (Full d, True)  -> ((dollars, d :. cents), True)
                      (Full d, False) -> ((d :. dollars, cents), False)
                      _               -> rest

dollars :: Chars -> Chars
dollars lst = dsString ++ " and " ++ csString
  where
    (ds, cs)  = parseChars lst
    csD3      = centsToD3 cs
    dsD3Named = withGroupName $ dollarsToD3Lst ds
    dsString  = dollarBuildStr dsD3Named
    csString  = centBuildStr csD3
