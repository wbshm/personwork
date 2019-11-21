module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where

-- cabal install split
-- cabal install tuple
import Data.Tuple.Select
import Data.List
import Data.List.Split

type Transaction = (Char, Int, Int, String, Int)

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11),
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A


transaction_to_string :: Transaction -> String
transaction_to_string transaction =
    if sel1(transaction) =='B' then
        "Bought " ++ show(sel2(transaction)) ++ " units of " ++ sel4(transaction)  ++ " for " ++ show(sel3(transaction)) ++ " pounds each on day " ++ show(sel5(transaction))
    else do
        "Sold " ++ show(sel2(transaction)) ++ " units of " ++ sel4(transaction)  ++ " for " ++ show(sel3(transaction)) ++ " pounds each on day " ++ show(sel5(transaction))


trade_report_list :: [Transaction] -> [String]
trade_report_list transactionList = map (transaction_to_string) transactionList


stock_test :: String -> Transaction -> Bool
stock_test target transaction = target == sel4(transaction)


get_trades :: String -> [Transaction] -> [Transaction]
get_trades target transactionList = filter (\n ->  target ==sel4(n) ) transactionList


trade_report :: String -> [Transaction] -> String
trade_report target transactionList = unlines (trade_report_list(get_trades target transactionList))


-- Part B


update_money :: Transaction -> Int -> Int
update_money transaction total =
    if sel1(transaction) == 'B' then
        total - sel2(transaction) * sel3(transaction)
    else do
        total + sel2(transaction) * sel3(transaction)


profit :: [Transaction] -> String -> Int
profit transactionList target = sum (map (\n -> update_money n 0 ) (get_trades target transactionList))



profit_report :: [String] -> [Transaction] -> String
profit_report targetList transactionList = unlines (map (\n -> n ++ ": " ++ show(profit transactionList n)) targetList)

-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"

type Prices = [(String, [Int])]
test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]

complex_profit_report :: String -> Prices -> String
complex_profit_report inputLog prizeList = unlines ( map(\n -> fst(n)++": " ++ show(explain_log inputLog n)) prizeList)

explain_log :: String -> (String, [Int]) -> Int
explain_log inputLog prizeInfo = do
    let prizeList = sel2(prizeInfo)
    let tmpLog = filter (\n -> do (n!!2) == sel1(prizeInfo)) (map (\n -> splitOn " " n) (splitOn "\n" inputLog))
    let formatLog = map(\n -> do
        let m = (n!!0,read (n!!1)::Int,n!!2,read (n!!3)::Int)
        m ) tmpLog
    let logList = map(\n -> if sel1(n)=="BUY" then -sel2(n)*prizeList!!(sel4(n)-1) else sel2(n)*prizeList!!(sel4(n)-1)) formatLog
    sum logList


main = do
-- Question 1
--    putStrLn(transaction_to_string('S', 150, 1240, "VTI", 9))

-- Question 2
    putStrLn(show(trade_report_list(take 3 test_log)))

-- Question 3
--    putStrLn(show(stock_test "VTI" ('B', 100, 1104, "VTI", 1)))
--    putStrLn(show(stock_test "ONEQ" ('B', 100, 1104, "VTI", 1)))

-- Question 4
--    putStrLn(show(get_trades "VTI" test_log))
--    putStrLn(show(get_trades "ONEQ" test_log))

-- Question 5
--    putStr( trade_report "VTI" test_log )

-- Question 6
--    putStrLn(show(update_money ('B', 1, 10, "VTI", 5) 100 ))
--    putStrLn(show(update_money ('S', 2, 10, "VTI", 5) 100 ))

-- Question 7
--    putStrLn(show(profit test_log "VTI"))
--    putStrLn(show( profit test_log "ONEQ" ))

-- Question 8
--    putStrLn(profit_report ["VTI", "ONEQ"] test_log)
--    putStrLn(profit_report ["VTI", "ONEQ", "IWRD"] test_log)

-- Question 9
    putStrLn(complex_profit_report test_str_log test_prices )
