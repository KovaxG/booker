{-
    n * Pi * b = (Po + a * Pi) * n + Pm
    
    where
        n = number of customers
        Pi = the selling price of a fresh
        b in (0, 1) = the profit margin
        Po = the price of producing one fresh
        a in (0,1) = the tax on a fresh
        Pm = cost paid in a month for upkeep and other
-}
module Main where

import Text.Read
import Report
import Utils
import System.IO -- I need to flush the buffer

batchPrice :: RentPrice -> OtherMontlyCost -> Money
batchPrice rp oc = rp $+$ oc

freshPrice :: CupPrice -> FruitPrice -> Money
freshPrice cp fp = cp $+$ fp 

testData = calcMonth 
    (batchPrice (Ron 2500) (Ron 500))
    (freshPrice (Ron 0.5) (Ron 6))
    1500
    (Ron 14)
    0.09
        
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    displayStartScreen

    rent <- askForRent
    other <- askForOtherMonth
    let bp = batchPrice rent other

    fruitCost <- askForFruitCost
    cupCost <- askForCupCost
    let fp = freshPrice cupCost fruitCost
    
    customers <- askForCustomers
    
    currentPrice <- askForCurrentPrice
    
    let report = calcMonth bp fp customers currentPrice 0.09
    printReport report
    
    putStrLn "\n\nNyomj meg egy gombot hogy ujra kezdjuk."
    _ <- getLine
    putStrLn "\n"
    main
    
askForRent :: IO RentPrice
askForRent = 
    askFor "Mennyi a ber egy honapra?" Ron

askForOtherMonth :: IO OtherMontlyCost
askForOtherMonth = 
    askFor "Mennyi koltseg van meg a honapban?" Ron
    
askForFruitCost :: IO FruitPrice
askForFruitCost =
    askFor "Mennyibe kerul a gyumolcs 1 freshhez?" Ron
    
askForCupCost :: IO CupPrice
askForCupCost =
    askFor "Mennyibe kerul egy pohar?" Ron
    
askForCustomers :: IO Int
askForCustomers = 
    askFor "Atlagban hany kliens vasarol fresht egy honapban?" id
    
askForCurrentPrice :: IO Money
askForCurrentPrice = 
    askFor "Mennyibe arulod most a fresheket?" Ron
   
askFor :: Read a => String -> (a -> b) -> IO b
askFor descr f = do
    putStrLn $ descr
    putStr "> "
    value <- readMaybe <$> getLine
    maybe wrongInput (return . f) value
    where
        wrongInput = do
            putStrLn "Kerlek irj egy szamot."
            askFor descr f

displayStartScreen :: IO ()
displayStartScreen = do
    putStrLn "//==============================\\\\"
    putStrLn "||                              ||"
    putStrLn "||        _     _               ||"
    putStrLn "||       | |   | |              ||"
    putStrLn "||       | |   | |              ||"
    putStrLn "||       | \\___/ |              ||"
    putStrLn "||        \\_____/ nifresh       ||"
    putStrLn "||                              ||"
    putStrLn "||      Verzio: 0.1             ||"
    putStrLn "||                              ||"
    putStrLn "\\\\==============================//"

