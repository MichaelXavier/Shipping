{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Shipping.Tracking.Detection (VerifiableTrackingNumber(..)) where

import ClassyPrelude hiding (toUpper)
import Data.Char ( digitToInt
                 , isDigit
                 , toUpper
                 , isAlpha
                 , isAlphaNum )
import Data.List (cycle)
import Data.Maybe (isJust, catMaybes)
import Data.Vector ( (!)
                   , ifoldl' )
import Text.Regex.PCRE.Light

import Web.Shipping.Carriers (Carrier(..))
import Web.Shipping.Types
import Web.Shipping.Utils ((<||>))

import Debug.Trace (traceShow)

class VerifiableTrackingNumber a where
  verifyTrackingNumber :: a -> Bool

instance VerifiableTrackingNumber UPSTracking where
  verifyTrackingNumber (UPSTracking t) | looksOK   = traceShow (givenCD, calculatedCD, total) $ digitToInt givenCD == calculatedCD
                                       | otherwise = False
    where (preamble, rest)        = splitAt 2 t'
          (core, givenCD:_)       = splitAt 15 t'
          total                   = sum odds + (sum . map (*2)) evens
          calculatedCD            = case highestMultiple - total of
                                      10 -> 0
                                      cd -> cd
          (evens, odds)           = evensOdds numbers
          numbers                 = map charValue core
          charValue c | isAlpha c = fromMaybe 0 $ lookup c matrix
                      | otherwise = digitToInt c
          matrix                  = zip ['A'..'Z'] $ [2..9] ++ cycle [0..9]
          highestMultiple | (total `mod` 10) == 0 = total
                          | otherwise             = ((total `div` 10) + 1) * 10
          looksOK = longEnough && correctPreamble && validChars
          validChars         = all isAlphaNum t
          longEnough         = length t == 18
          correctPreamble    = preamble == "1Z"
          t'      = unpack t

  -- verifyTrackingNumber (UPSTracking t)
  --   | looksOK   = traceShow (core, digitSum, calculatedCD, givenCD) $ givenCD == calculatedCD
  --   | otherwise = traceShow (length t, longEnough, correctPreamble, validChars) False
  --   where looksOK            = longEnough && correctPreamble && validChars
  --         validChars         = all isAlphaNum t
  --         longEnough         = length t == 18
  --         correctPreamble    = preamble == "1Z"
  --         -- calculatedCD       = case digitSum `divMod` 10 of
  --         --                        (_, 0) -> 0
  --         --                        (d, _) -> let nextMultiple = (d + 1) * 10
-- -- --                         | otherwise             = ((total `div` 10) + 1) * 10
  --         --                                  in traceShow (nextMultiple, digitSum, nextMultiple `mod` digitSum) $ nextMultiple `mod` digitSum

  --         calculatedCD = case digitSum `mod` 10 of
  --                          0 -> 0
  --                          x -> 10 - x
  --         highestMultiple | (digitSum `mod` 10) == 0 = digitSum
  --                         | otherwise                = ((digitSum `div` 10) + 1) * 10

  --         digitSum           = ifoldl' digitAdd 0 weightVec
  --         digitAdd acc i n
  --           | i == 0    = acc
  --           | even i    = acc + 2 * n
  --           | otherwise = acc + n
  --         weightVec          = fromList . map charWeight $ ('0':base)
  --         givenCD            = charWeight cdChar
  --         (core, cdChar:_)   = splitAt 15 . map toUpper $ base
  --         (preamble, base)   = splitAt 2 $ unpack t
  --         charWeight c
  --           | isDigit c = digitToInt c
  --           | otherwise = fromMaybe 0 $ lookup c matrix
  --         matrix             = zip ['A'..'Z'] $ [2..9] ++ cycle [0..9]

instance VerifiableTrackingNumber USPSTracking where
  verifyTrackingNumber = verifyUSPSMod10 <||> verifyUSPSMod11

verifyUSPSMod10 :: USPSTracking -> Bool
verifyUSPSMod10 (USPSTracking t) | looksOk = givenCD == calculatedCD
                                 | otherwise  = False
  where longEnough = length t == 20
        allDigits  = all isDigit t
        looksOk    = longEnough && allDigits
        givenCD    = tVec ! 1
        calculatedCD
          | digitSum `mod` 10 == 0 = 0
          | otherwise              = let d = digitSum `div` 10
                                         nextMultiple = d * 10 + 10
                                     in nextMultiple `mod` digitSum
        digitSum = ifoldl' digitAdd 0 tVec
        digitAdd acc i n
          | i == 0 || i == 1 = acc
          | even i           = acc + 3 * n
          | otherwise        = acc + n
        tVec :: Vector Int
        tVec = fromList . map digitToInt $ ('0':reverse t')
        t' = unpack t

-- domestic mail only, USS 39 barcode
verifyUSPSMod11 :: USPSTracking -> Bool
verifyUSPSMod11 (USPSTracking t) | looksOk = givenCD == calculatedCD
                                 | otherwise  = False
  where looksOk           = longEnough && length digitSection == 9
        longEnough        = length t == 13 -- probably?
        digitSection      = takeWhile isDigit . dropWhile notDigit . unpack $ t
        (core, givenCD:_) = splitAt 8 . map digitToInt $ digitSection
        calculatedCD      = case cdSum `mod` 11 of
                              0 -> 5
                              1 -> 0
                              n -> 11 - n
        cdSum             = sum $ zipWith (*) multipliers core
        multipliers       = [8, 6, 4, 2, 3, 5, 9, 7]

notDigit :: Char -> Bool
notDigit = not . isDigit

-- verifyCheckDigit :: TrackingNumber -> Bool
-- verifyCheckDigit (USPSTracking t)  = verifyUSPSMod10 t || verifyUSPSMod11 t
-- verifyCheckDigit (UPSTracking t)   = verifyUPS t
-- verifyCheckDigit (FedExTracking t) = verifyFedEx t

-- verifyUSPSMod11 :: Text -> Bool
-- verifyUSPSMod11 = undefined

-- verifyUPS :: Text -> Bool
-- verifyUPS = undefined

-- verifyFedEx :: Text -> Bool
-- verifyFedEx =


-- detectCarrier :: Text -> Maybe Carrier
-- detectCarrier track | verifyUSPSCheck track'  = Just USPS
--                     | verifyUPSCheck track'   = Just UPS
--                     | verifyFedExCheck track' = Just FedEx
--                     | otherwise               = Nothing
--   where track' = stripWS track
-- --detectCarrier track | any (isMatch track) usps  = Just USPS
-- --                    | any (isMatch track) ups   = Just UPS
-- --                    | any (isMatch track) fedex = Just FedEx
-- --                    | otherwise                 = Nothing

-- usps :: [Regex]
-- usps = map regex [
--     "/(\\b\\d{30}\\b)|(\\b91\\d+\\b)|(\\b\\d{20}\\b)",
--     "/^E\\D{1}\\d{9}\\D{2}$|^9\\d{15,21}$/",
--     "/^91[0-9]+$/",
--     "/^[A-Za-z]{2}[0-9]+US$/",
--     "/^(EA|EC|CP|RA)\\d{9}(\\D{2})?$|^(7\\d|03|23|91)\\d{2}\\s?\\d{4}\\s?\\d{4}\\s?\\d{4}\\s?\\d{4}(\\s\\d{2})?$|^82\\s?\\d{3}\\s?\\d{3}\\s?\\d{2}$/i"
--   ]

-- ups :: [Regex]
-- ups = map regex [
--   "/\\b(1Z ?[0-9A-Z]{3} ?[0-9A-Z]{3} ?[0-9A-Z]{2} ?[0-9A-Z]{4} ?[0-9A-Z]{3} ?[0-9A-Z]|[\\dT]\\d\\d\\d ?\\d\\d\\d\\d ?\\d\\d\\d)\\b/"
--   ]

-- fedex :: [Regex]
-- fedex = []

-- regex :: Text -> Regex
-- regex r = compile r options
--   where options = [caseless]
-- isMatch :: ByteString -> Regex -> Bool
-- isMatch str reg = isJust $ match reg str []

-- verifyFedExCheck :: ByteString -> Bool
-- verifyFedExCheck str | BS8.length str == 12 = actualCheckDigit == expectedCheckDigit
--                      | otherwise            = False
--   where (toZip, checkNum)  = BS8.splitAt 11 str
--         multipliers        = "31731731731"
--         products           = BS8.zipWith mult toZip multipliers
--         mult char1 char2   = digitToInt char1 * digitToInt char2
--         remainder          = sum products `rem` 11
--         expectedCheckDigit = if remainder == 10 then 0
--                              else remainder
--         actualCheckDigit   = char2i . BS8.head $ checkNum

-- verifyUPSCheck :: ByteString -> Bool
-- verifyUPSCheck str | looksOK   = verify
--                    | otherwise = False
--   where verify = actualCheckDigit == expectedCheckDigit
--         (prefix, rest)          = BS8.splitAt 2 str
--         coreTrackingNumber      = BS8.init rest
--         total                   = sum odds + (sum . map (*2)) evens
--         expectedCheckDigit      = char2i $ BS8.last str
--         actualCheckDigit        = case highestMultiple - total of
--                                     10 -> 0
--                                     cd -> cd
--         (evens, odds)           = evensOdds numbers
--         numbers                 = catMaybes $ BS8.foldr' (\c ns -> (charValue c):ns) [] coreTrackingNumber
--         charValue c | isAlpha c = lookup c matrix
--                     | otherwise = Just $ char2i c
--         matrix                  = zip ['A'..'Z'] $ [2..9] ++ cycle [0..9]
--         highestMultiple | (total `mod` 10) == 0 = total
--                         | otherwise             = ((total `div` 10) + 1) * 10
--         looksOK = prefix == "1Z" && notNull rest && BS8.all isAlphaNum rest


-- -- evens/odds backwards from official documentation because they pad the matrix
-- -- with the check digit then ignore it
-- -- other page says mod 10 may be used only for domstic, mod 11 must be used for
-- -- express intl, may be used for domestic
-- verifyUSPSCheck :: Text -> Bool
-- verifyUSPSCheck str | looksOK    = actualCheckDigit == expectedCheckDigit
--                     | otherwise  = False
--   where coreTrackingNumber = init chars
--         (evens, odds) = evensOdds $ coreTrackingNumber
--         expectedCheckDigit = char2i $ BS8.last str
--         chars = BS8.unpack str
--         total = 3 * (sumChars odds) + sumChars evens
--         actualCheckDigit = highestMultiple - total
--         highestMultiple  | (total `mod` 10) == 0 = total
--                          | otherwise             = ((total `div` 10) + 1) * 10
--         sumChars = sum . map char2i
--         looksOK = notNull str && BS8.all isDigit str
        
-- notNull :: Text -> Bool
-- notNull = not . BS8.null

-- char2i :: Char -> Int
-- char2i c | isDigit c = ord c - ord '0'
--          | otherwise = 0

evensOdds :: [a] -> ([a], [a])
evensOdds xs = (reverse es, reverse os)
  where (es, os) = evensOdds' 1 xs ([], [])
        evensOdds' _ [] (evens, odds)       = (evens, odds)
        evensOdds' num (x:xs) (evens, odds) | even num  = evensOdds' (num + 1) xs (x:evens, odds)
                                            | otherwise = evensOdds' (num + 1) xs (evens, x:odds)

-- stripWS :: Text -> Text
-- stripWS = T.filter (not . isSpace)
