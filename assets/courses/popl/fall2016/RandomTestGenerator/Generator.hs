module Generator where

import System.IO
import System.Process
import System.Random
import Data.List
import Preamble

shuffle :: [a] -> IO [a]
shuffle l = do
  g <- newStdGen
  let nl = take ll $ randomRs (x,y) g
  let shuffled_l = map snd $ sortBy ordering $ zip nl l
  return $ shuffled_l
    where
      ll = length l
      x :: Float
      x = 0.0
      y :: Float
      y = 1.0
      ordering (x,_) (y,_) | x<y  = LT
                           | x==y = EQ
                           | x>y  = GT

shuffleTest :: Test -> IO Test
shuffleTest []     = return []
shuffleTest (q:qs) = do
  case q of
    TF dir questions -> do
      sq  <- shuffle questions
      qs' <- shuffleTest qs
      return $ TF dir sq : qs'
    Mult dir questions  -> do
      questions' <- mapM shuffleQ questions
      sq  <- shuffle questions'
      qs' <- shuffleTest qs
      return $ Mult dir sq : qs' 
    Match dir questions matches -> do
      ms  <- shuffle matches
      qs' <- shuffleTest qs
      return $ Match dir questions ms : qs' 
    Plain s -> do
      qs' <- shuffleTest qs
      return $ Plain s : qs' 


generate :: String -> Test -> IO ()
generate file test = do
  hdl <- openFile ("TestDir/" ++ file) WriteMode
  hPutStrLn hdl frontmatter
  test' <- shuffleTest test
  hPutStrLn hdl $ foldr (\ q qs -> show q ++ qs) "" test'
  hPutStrLn hdl endmatter
  hClose hdl

generateTests tname test 0 = return ()
generateTests tname test n = do
  generate (tname ++ "_" ++ show n ++ ".tex") test
  generateTests tname test (n-1)

genTests tname test last 0 = return ()
genTests tname test last n = do
  let testname = tname ++ "_" ++ show n ++ ".tex"
  test' <- shuffle test
  generate testname (test'++last)
  system $ "/Library/TeX/texbin/pdflatex " ++ "TestDir/" ++ testname
  system $ "/usr/bin/lpr -o sides=two-sided-long-edge " ++ tname ++ "_" ++ show n ++ ".pdf"
  genTests tname test' last (n-1)


type Test = [QuestionGroup]
data QuestionGroup = TF String [String]
                   | Mult String [Quest]
                   | Match String [String] [String]
                   | Plain String
data Quest         = MultChoice String [String]
                   | ShortAnswer String Int

shuffleQ (MultChoice s qs) = do
  qs' <- shuffle qs
  return $ MultChoice s qs'
shuffleQ s@(ShortAnswer _ _) = return s  


instance Show Quest where
  show (ShortAnswer q s)    = "\\item " ++ q ++ "\\vspace{" ++ show s ++ "ex}\n"
  show (MultChoice stmt qs) = "\\item \n" ++ stmt ++ "\n" ++      
                              "\\begin{enumerate}[(a)]\n" ++
                              (foldr (++) "" $ map (\ q -> "\\item " ++ q) qs) ++
                              "\\end{enumerate}\n"

instance Show QuestionGroup where
  show (TF dir qs) =
      "\\item [] {\\bf Directions.} " ++ dir ++ "\n" ++
      (foldr (++) "" $ map (\ q -> "\\item {\\bf True} or {\\bf False}: " ++ q) qs) ++
      "\\newpage"
  show (Match d qs ms) = "\\item []  {\\bf Directions.} " ++ d ++ "\n" ++
                         "\\begin{minipage}{2.5in}"   ++
                         (foldr (++) "" $ map (\ q -> "\\item " ++ q) qs) ++
                         "\\end{minipage}\\hspace{1.2in}\n" ++
                         "\\begin{minipage}{2.75in}\n"
                         ++ "\\begin{enumerate}[(a)]\n"
                         ++ (foldr (++) "" $ map (\ q -> "\\item " ++ q) ms) 
                         ++ "\\end{enumerate}\n"
                         ++ "\\end{minipage}\n" ++
                         "\\newpage"
  show (Mult stmt qs) =
      "\\item [] " ++ stmt ++
      (foldr (\ q qs -> q ++ "\n\n" ++ qs) [] $ (map show qs)) ++ 
      "\\newpage"
  show (Plain stmt) =
      "\\item [] " ++ stmt ++ "\\newpage"
