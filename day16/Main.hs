module Main where

import Packet
import Packet.Parser
import Bit
import System.Environment

process :: Bits -> IO ()
process stream = do
    putStrLn $ "Processing " ++ (show $ length stream) ++ " bits"
    let (n,p,rem) = parse_packet stream in do
        putStrLn $ "Parsed packet: "
        putStrLn $ show_nice p
        putStrLn $ "Read " ++ (show n) ++ " bits"
        putStrLn $ "Remaining junk: " ++ (showBits rem) ++ " [" ++ (show $ length rem) ++ "]"
        putStrLn $ "Sum of versions: " ++ (show $ sum_versions p)
        putStrLn $ "Result of evalutation: " ++ (show $ eval p)

do_file :: String -> IO Bits
do_file filename =
    parse_hex <$> readFile filename

do_packet :: String -> IO Bits
do_packet pack =
    return $ parse_hex pack

main :: IO ()
main = do
    (op:rem) <- getArgs
    dispatch op rem
    where dispatch "-f" (arg:_) = do_file arg >>= process
          dispatch "-p" (arg:_) = do_packet arg >>= process
          dispatch "-h" _ = do
              putStrLn $ "Use -p <patt> to specify a pattern directly."
              putStrLn $ "Use -f <file> to specify a pattern from a file."



