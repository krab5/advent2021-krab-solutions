module Main where

import Inst
import System.Environment

main :: IO ()
main = do
    (filename:_) <- getArgs
    ls <- lines <$> readFile filename
    let pos = Pos 0 0
        pos2 = Pos2 0 0 0
        insts = parseAll ls in
        let result = execAll pos insts 
            result2 = execAll2 pos2 insts in do
            putStrLn $ "Num. inst: " ++ (show $ length insts)
            putStrLn $ "====="
            putStrLn $ "Initial: " ++ (show pos)
            putStrLn $ "Final: " ++ (show result)
            putStrLn $ "====="
            putStrLn $ "Initial2: " ++ (show pos2)
            putStrLn $ "Final2: " ++ (show result2)



