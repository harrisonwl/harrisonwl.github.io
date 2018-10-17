module Substitute where
import Base

data Exp  = Number    Int
          | Add       Exp Exp
          | Subtract  Exp Exp
          | Multiply  Exp Exp
          | Divide    Exp Exp
          | Variable  String        -- added
   deriving (Eq, Show)

substitute1:: (String, Int) -> Exp -> Exp
substitute1 (var, val) exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) = if var == name
                          then Number val
                          else Variable name

type Env = [(String, Int)]

substitute :: Env -> Exp -> Exp
substitute env exp = subst exp where
  subst (Number i)      = Number i
  subst (Add a b)       = Add (subst a) (subst b)
  subst (Subtract a b)  = Subtract (subst a) (subst b)
  subst (Multiply a b)  = Multiply (subst a) (subst b)
  subst (Divide a b)    = Divide (subst a) (subst b)
  subst (Variable name) =
    case lookup name env of
      Just val -> Number val
      Nothing  -> Variable name

substitute1R env exp = foldr substitute1 exp env
