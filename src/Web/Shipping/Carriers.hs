module Web.Shipping.Carriers (Carrier(..)) where

data Carrier = UPS | USPS | FedEx deriving (Show, Eq)
