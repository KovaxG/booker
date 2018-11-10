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
    putStrLn   "===============Report================="
    putStrLn   "--------------------------------------"
    putStrLn $ "- Honapos Szogezett kiadas: " ++ show (montlyCost r)    
    putStrLn $ "- Osszes freshek gyartasi ara (+tva): " ++ show (itemCost r)
    putStrLn $ "  (Egy freshnek a gyartasi ara (+tva): " ++ show (perItemCost r) ++ ")"
    putStrLn $ "- Osszes kiadas: " ++ show (totalCost r)
    putStrLn   "--------------------------------------"
    putStrLn $ "+ Osszes bevetel:    " ++ show (totalIncome r)
    putStrLn $ "  (Egy freshbeli bevetel: " ++ show (perItemIncome r) ++ ")"
    putStrLn   "--------------------------------------"
    putStrLn $ "$ Profit egy freshnel: " ++ show (profitPerFresh r)
    putStrLn $ "$ Profit:    " ++ show (profit r)
    putStrLn   "--------------------------------------"
    putStrLn $ "Ennyibe kene adni, hogy nullan legyel: " ++ show (breakEvenPrice r)
    putStrLn   "======================================"
