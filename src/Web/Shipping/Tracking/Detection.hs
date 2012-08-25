{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.Tracking.Detection (detectCarrier,
                                        verifyFedExCheck,
                                        verifyUSPSCheck,
                                        verifyUPSCheck) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 ()
import Data.Char
import Data.List (foldl')
import Data.Maybe (isJust, catMaybes)
import Text.Regex.PCRE.Light

import Web.Shipping.Carriers (Carrier(..))
import Web.Shipping.Tracking.Numbers

detectCarrier :: ByteString -> Maybe Carrier
detectCarrier track | verifyUSPSCheck track'  = Just USPS
                    | verifyUPSCheck track'   = Just UPS
                    | verifyFedExCheck track' = Just FedEx
                    | otherwise               = Nothing
  where track' = stripWS track
--detectCarrier track | any (isMatch track) usps  = Just USPS
--                    | any (isMatch track) ups   = Just UPS
--                    | any (isMatch track) fedex = Just FedEx
--                    | otherwise                 = Nothing

usps :: [Regex]
usps = map regex [
    "/(\\b\\d{30}\\b)|(\\b91\\d+\\b)|(\\b\\d{20}\\b)",
    "/^E\\D{1}\\d{9}\\D{2}$|^9\\d{15,21}$/",
    "/^91[0-9]+$/",
    "/^[A-Za-z]{2}[0-9]+US$/",
    "/^(EA|EC|CP|RA)\\d{9}(\\D{2})?$|^(7\\d|03|23|91)\\d{2}\\s?\\d{4}\\s?\\d{4}\\s?\\d{4}\\s?\\d{4}(\\s\\d{2})?$|^82\\s?\\d{3}\\s?\\d{3}\\s?\\d{2}$/i"
  ]

ups :: [Regex]
ups = map regex [
  "/\\b(1Z ?[0-9A-Z]{3} ?[0-9A-Z]{3} ?[0-9A-Z]{2} ?[0-9A-Z]{4} ?[0-9A-Z]{3} ?[0-9A-Z]|[\\dT]\\d\\d\\d ?\\d\\d\\d\\d ?\\d\\d\\d)\\b/"
  ]

fedex :: [Regex]
fedex = []

regex :: ByteString -> Regex
regex r = compile r options
  where options = [caseless]
isMatch :: ByteString -> Regex -> Bool
isMatch str reg = isJust $ match reg str []

verifyFedExCheck :: ByteString -> Bool
verifyFedExCheck str | BS8.length str == 12 = actualCheckDigit == expectedCheckDigit
                     | otherwise            = False
  where (toZip, checkNum)  = BS8.splitAt 11 str
        multipliers        = "31731731731"
        products           = BS8.zipWith mult toZip multipliers
        mult char1 char2   = digitToInt char1 * digitToInt char2
        remainder          = sum products `rem` 11
        expectedCheckDigit = if remainder == 10 then 0
                             else remainder
        actualCheckDigit   = char2i . BS8.head $ checkNum

verifyUPSCheck :: ByteString -> Bool
verifyUPSCheck str | looksOK   = verify
                   | otherwise = False
  where verify = actualCheckDigit == expectedCheckDigit
        (prefix, rest)          = BS8.splitAt 2 str
        coreTrackingNumber      = BS8.init rest
        total                   = sum odds + (sum . map (*2)) evens
        expectedCheckDigit      = char2i $ BS8.last str
        actualCheckDigit        = case highestMultiple - total of
                                    10 -> 0
                                    cd -> cd
        (evens, odds)           = evensOdds numbers
        numbers                 = catMaybes $ BS8.foldr' (\c ns -> (charValue c):ns) [] coreTrackingNumber
        charValue c | isAlpha c = lookup c matrix
                    | otherwise = Just $ char2i c
        matrix                  = zip ['A'..'Z'] $ [2..9] ++ cycle [0..9]
        highestMultiple | (total `mod` 10) == 0 = total
                        | otherwise             = ((total `div` 10) + 1) * 10
        looksOK = prefix == "1Z" && notNull rest && BS8.all isAlphaNum rest


-- evens/odds backwards from official documentation because they pad the matrix
-- with the check digit then ignore it
verifyUSPSCheck :: ByteString -> Bool
verifyUSPSCheck str | looksOK    = actualCheckDigit == expectedCheckDigit
                    | otherwise  = False
  where coreTrackingNumber = init chars
        (evens, odds) = evensOdds $ coreTrackingNumber
        expectedCheckDigit = char2i $ BS8.last str
        chars = BS8.unpack str
        total = 3 * (sumChars odds) + sumChars evens
        actualCheckDigit = highestMultiple - total
        highestMultiple  | (total `mod` 10) == 0 = total
                         | otherwise             = ((total `div` 10) + 1) * 10
        sumChars = sum . map char2i
        looksOK = notNull str && BS8.all isDigit str
        
notNull :: ByteString -> Bool
notNull = not . BS8.null

char2i :: Char -> Int
char2i c | isDigit c = ord c - ord '0'
         | otherwise = 0

evensOdds :: [a] -> ([a], [a])
evensOdds xs = (reverse es, reverse os)
  where (es, os) = evensOdds' 1 xs ([], [])
        evensOdds' _ [] (evens, odds)       = (evens, odds)
        evensOdds' num (x:xs) (evens, odds) | even num  = evensOdds' (num + 1) xs (x:evens, odds)
                                            | otherwise = evensOdds' (num + 1) xs (evens, x:odds)

stripWS :: ByteString -> ByteString
stripWS = BS8.filter isSpace
