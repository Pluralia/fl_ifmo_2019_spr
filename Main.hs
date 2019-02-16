module Main where

import Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
  putStrLn input
  putStrLn $ show $ tokenize input
  putStrLn ""

main :: IO ()
main = do
  -- random
  runTokenizer " 1 2 abc if "
  runTokenizer " "
  runTokenizer "7     2147483647 3 Kkjhkd wd_wd__d______w_    79228162514264337593 finally 543_950336       100_000_000_000 "
  -- keywords && idents
  runTokenizer "import     _if       nonlocal_ ex_cept   ___deoij    _32     AShk__j3029__ "
  -- numbers
  runTokenizer "0    0_0     00000     0_00    00_0_000_0 "
  runTokenizer "2      32_323     23_2093_90 392_3_3 "
  -- fails
  runTokenizer "2039dm "
  runTokenizer "-owie "
  runTokenizer "23*kjhd "
  runTokenizer "0____ "
  runTokenizer "0__000_0 "
  runTokenizer "23___324 "
  runTokenizer "2093_23_ "
