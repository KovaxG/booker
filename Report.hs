module Report where

import Utils

data Report = Report {
    montlyCost :: Money,
    perItemCost :: Money,
    itemCost :: Money,
    totalCost :: Money,
    perItemIncome  :: Money,
    totalIncome :: Money,
    profit :: Money,
    profitPerFresh :: Money,
    breakEvenPrice :: Money,
    profitPrice :: Money
} deriving (Show)

calcMonth :: Money -> Money -> Int -> Money -> TVA -> Report
calcMonth batchCost freshCost customers freshPrice tva = 
    Report {
        montlyCost = batchCost,
        perItemCost = perItemCost,
        itemCost = itemCost,
        totalCost = totalCost,
        perItemIncome = freshPrice,
        totalIncome = totalIncome,
        profit = totalIncome $-$ totalCost,
        profitPerFresh = profitPerFresh,
        breakEvenPrice = breakEvenPrice,
        profitPrice = profitPrice
    }
    where 
        itemCost = perItemCost $*# customers 
        perItemCost = freshCost $+$ (freshPrice $*% tva)
        totalCost = batchCost $+$ itemCost
        totalIncome = freshPrice $*# customers
        profitPerFresh = freshPrice $-$ perItemCost $-$ (batchCost $/# customers)
        breakEvenPrice = breakEven customers 1 freshCost tva batchCost
        profitPrice = breakEven customers 0.74 freshCost tva batchCost

breakEven :: Int -> Double -> Money -> Double -> Money -> Money
breakEven n b po a pm = 
    ((po $*# n) $+$ pm) $/% ((fromIntegral n * b) - (fromIntegral n * a)) 
 
printReport :: Report -> IO ()
printReport r = do
    putStrLn "=====Report======"
    putStrLn $ "Monthly Cost: " ++ show (montlyCost r)
    putStrLn $ "PerItemCost: " ++ show (perItemCost r)
    putStrLn $ "ItemsCost: " ++ show (itemCost r)
    putStrLn   "---------------------------"
    putStrLn $ "TotalCost: " ++ show (totalCost r)
    putStrLn ""
    putStrLn $ "PerItemIncome: " ++ show (perItemIncome r)
    putStrLn $ "Income:    " ++ show (totalIncome r)
    putStrLn ""
    putStrLn $ "Profit per fresh: " ++ show (profitPerFresh r)
    putStrLn $ "Profit:    " ++ show (profit r)
    putStrLn ""
    putStrLn $ "Breakeven price: " ++ show (breakEvenPrice r)
    putStrLn "================="
 
