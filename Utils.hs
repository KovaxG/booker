module Utils where

import Text.Printf

type TVA = Double

newtype Money = Ron Double deriving (Read)

instance Show Money where
    show = doubleShow

doubleShow :: Money -> String
doubleShow (Ron d) = printf "%.2f lej" d
    
-- TODO might want to enforce in type monthly and per item scores
type RentPrice = Money
type OtherMontlyCost = Money

type FruitPrice = Money
type CupPrice = Money
type OtherCost = Money

type Customers = Int

discount :: Double -> Money -> Money
discount p m = m $-$ (m $*% p)

addcount :: Double -> Money -> Money
addcount p m = m $+$ (m $*% p)

($+$) :: Money -> Money -> Money
(Ron a) $+$ (Ron b) = Ron (a + b)

($-$) :: Money -> Money -> Money
(Ron a) $-$ (Ron b) = Ron (a - b)

($*#) :: Money -> Int -> Money
(Ron a) $*# nr = Ron (a * fromIntegral nr)

($*%) :: Money -> Double -> Money
(Ron a) $*% p = Ron (a * p)

(#*%) :: Int -> Double -> Double
i #*% p = fromIntegral i * p

($/#) :: Money -> Int -> Money
(Ron a) $/# nr = Ron (a / fromIntegral nr)

($/%) :: Money -> Double -> Money
(Ron a) $/% p = Ron (a / p)
