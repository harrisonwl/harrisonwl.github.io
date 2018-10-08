module IntBool where
import Prelude hiding (LT, GT, EQ, id)
import Base
import Value
import Operators
import Data.Maybe

data Exp  = Literal   Value
          | Unary     UnaryOp Exp
          | Binary    BinaryOp Exp Exp
          | If        Exp Exp Exp
          | Variable  String
          | Declare   String Exp Exp
  deriving Show

-- Evaluate an expression in an environment
evaluate :: Exp -> Env -> Value
evaluate (Literal v) env      = v
evaluate (Unary op a) env     = unary op (evaluate a env)
evaluate (Binary op a b) env  = binary op (evaluate a env) (evaluate b env)
evaluate (Variable x) env     = fromJust (lookup x env)
evaluate (Declare x exp body) env = evaluate body newEnv
  where newEnv = (x, evaluate exp env) : env
evaluate (If a b c) env =
  let BoolV test = evaluate a env in
    if test then evaluate b env
            else evaluate c env

execute exp = evaluate exp []

