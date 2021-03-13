module InternalRepr ( Program
                    , Block (
                        AssignBlock,
                        IfBlock,
                        RepeatBlock
                    )
                    , Op (
                        Add,
                        Mul,
                        Div,
                        Pow,
                        Eq,
                        Neq,
                        LeqThan,
                        GeqThan,
                        LessThan,
                        GreaterThan
                    )
                    , Expr (
                        ValE,
                        VarE,
                        BinOpE
                    )
                    ) where

type Program = [Block]
data Block
    = AssignBlock {
        assignVar   :: String,
        assignExpr  :: Expr
    }
    | IfBlock {
        ifCond      :: Expr,
        ifBody      :: [Block],
        elifBlocks  :: [(Expr, [Block])],
        ifElse      :: [Block]
    }
    | RepeatBlock {
        times       :: Expr,
        repeatBody  :: [Block]
    }
    deriving Show

data Op
    = Add
    | Mul
    | Div
    | Pow
    | Eq
    | Neq
    | LeqThan
    | GeqThan
    | LessThan
    | GreaterThan
    deriving ( Eq, Enum, Bounded, Show )

data Expr
    = ValE Int
    | VarE String
    | BinOpE Op Expr Expr
    deriving Show