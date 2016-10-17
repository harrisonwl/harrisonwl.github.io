module Arith2 where

import ArithAST
import ExpParser

ex1 = Aexp Plus (Const 1) (Const 2)
ex2 = Aexp Div (Const 1) (Const 0)

interp2 :: Exp -> Maybe Int
interp2 (Const v)         = Just v
interp2 (Aexp Plus e1 e2)  = case (interp2 e1,interp2 e2) of
                                 (Just v1,Just v2) -> Just (v1 + v2)
                                 (_,_)             -> Nothing
interp2 (Aexp Minus e1 e2) = case (interp2 e1,interp2 e2) of
                                 (Just v1,Just v2) -> Just (v1 - v2)
                                 (_,_)             -> Nothing
interp2 (Aexp Times e1 e2) = case (interp2 e1,interp2 e2) of
                                 (Just v1,Just v2) -> Just (v1 * v2)
                                 (_,_)             -> Nothing
interp2 (Aexp Div e1 e2)   = case (interp2 e1,interp2 e2) of
                                 (Just v1,Just 0)  -> Nothing
                                 (Just v1,Just v2) -> Just (v1 `div` v2)
                                 (_,_)             -> Nothing

run = interp2 . parser
