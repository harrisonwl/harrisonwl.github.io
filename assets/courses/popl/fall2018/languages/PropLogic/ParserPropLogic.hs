module ParserPropLogic where

import PropLogic
import Parsing

parseAtom  = do x <- identifier
                return (Atom x)

parseImply = do symbol "("
                wf1 <- parsePL
                symbol "=>"
                wf2 <- parsePL
                symbol ")"
                return (Imply wf1 wf2)

parseNot   = do symbol "("
                symbol "-"
                wf <- parsePL
                symbol ")"
                return (Not wf)

parsePL = parseAtom +++ parseImply +++ parseNot +++
          parseOr   +++ parseAnd +++   parseIff

parseOr    = do symbol "("
                wf1 <- parsePL
                symbol "\\/"
                wf2 <- parsePL
                symbol ")"
                return (error "\\/ undefined")

parseAnd   = do symbol "("
                wf1 <- parsePL
                symbol "/\\"
                wf2 <- parsePL
                symbol ")"
                failure     -- /\ undefined

parseIff   = do symbol "("
                wf1 <- parsePL
                symbol "<=>"
                wf2 <- parsePL
                symbol ")"
                failure          -- <=> undefined

deP (P x) = x

parse input = case (deP parsePL input) of
                   ((wf,rest):_) -> Just wf
                   []            -> Nothing
