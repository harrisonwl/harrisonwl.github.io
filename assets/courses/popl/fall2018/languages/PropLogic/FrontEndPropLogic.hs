module FrontEndPropLogic where

import System.Environment
import PropLogic
import ParserPropLogic

repl :: IO ()
repl = do
         putStr "PropLogic> "
         file <- getLine
         case file of
              "quit" -> return ()
              _      -> do iline <- readFile file
                           process iline

process :: String -> IO ()
process iline    = do putStrLn iline
                      putStrLn ("Parsed: " ++ show (parse iline))
                      repl

