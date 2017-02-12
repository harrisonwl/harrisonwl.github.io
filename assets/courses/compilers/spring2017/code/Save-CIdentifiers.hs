module CIdentifiers where

import Parsing

run (P x) inp = x inp

-- From K&R, 2nd Edition, Appendix A, page 192:
--   An identifier is a sequence of letters and digits. The
--   first character must be a letter; the underscore _ counts
--   as a letter. Upper and lower case letters are different. Identifiers
--   may have any length...

-- letters    = (a | ... | z | A | ... | Z | _)
-- digits     = (0 | ... | 9)
-- identifier = letters (letters | digits)*

cletter = letter +++ char '_'

cidents :: Parser String
cidents = do
  l  <- cletter
  ls <- many (cletter +++ digit)
  return (l:ls)
