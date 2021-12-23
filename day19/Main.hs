module Main where

import System.Environment
import Control.Monad
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S
import Geom
import Parser
import Scan

threshold = 12

main :: IO ()
main = do
    (filename:_) <- getArgs
    ct <- parse_file filename
    let ov = find_overlaps threshold ct in do
        forM_ ov $ \((n1,n2),t,v) -> do
            printf "Sections %d and %d overlap (transformation: %s, translation: %s)\n" n1 n2 (show t) (show v)
        let red = reduce ov in
            let assocred = M.assocs red in do
                putStrLn $ "Coordinates for " ++ (show $ M.size red) ++ " scanners:"
                forM_ (M.assocs red) $ \(k,(t,v)) -> printf "  Scanner %d: %s, %s\n" k (show t) (show v)
                let (maxv1,maxv2,maxdist) = max_manhattan $ map (\(_,(_,v)) -> v) assocred in
                    printf "Maximum Manhattan distance is %d between %s and %s\n" maxdist (show maxv1) (show maxv2)
                let pos = accumulate_positions red ct in do
                    forM_ pos (putStrLn . show)
                    printf "Found %d unique beacon positions\n" (S.size pos)


