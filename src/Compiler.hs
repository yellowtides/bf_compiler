module Compiler where

import InternalRepr
import Errors

import Control.Error.Util ( note )

import Data.Maybe ( fromJust, fromMaybe )
import Data.List.Split ( splitOn )
import Data.List ( lookup )

type Pointer = Int
type Memory = [(String, Pointer)]

-- (current pointer location, first unusued pointer, constructed bf string)
type BFProg = (Pointer, Pointer, String)

compile :: Program -> Either Err String
compile prog = (\((_, _, bfProgram), mm) -> bfProgram) 
                    <$> compile' prog ((0, 1, ""), [])

compile' :: Program -> (BFProg, Memory) -> Either Err (BFProg, Memory)
compile' [] bfmem = pure bfmem

compile' (AssignBlock var e : prog) ((p, unused, bf), mem) 
        = do
            (newp0, bfeval0) <- eval e ((p, unused), mem)
            -- Evaluates @e@ at position `unused`. Pointer is now at newp0.

            let newmem = let loc = lookup var mem in 
                            if fromMaybe 0 loc == 0 
                                then (var, unused) : mem    
                                -- if encountering var for the first time, insert it in memory
                                else mem

            let indexM = lookup var newmem
            let (newp1, bfeval1) = assign (fromJust indexM) unused [unused+1] newp0
            -- Pointer is now at newp1.

            let assignblock = bfeval0 ++ bfeval1 
            
            compile' prog ((newp1, unused+1, bf ++ assignblock), newmem)

compile' (RepeatBlock timesE body : prog) ((p, unused, bf), mem) 
        = do
            (newp0, bfeval0) <- eval timesE ((p, unused), mem)
            -- Evaluates @timesE@ at position `unused`. Pointer is now at newp0.

            ((newp1, un1, bfeval1), mem1) <- compile' body ((unused, unused+1, ""), mem)
            -- Evaluates the instructions within the @body@, with the pointer's starting position being `x`.
            -- Makes sure to increment `unused`, as `unused` now stores an important value and mustn't be overwritten. 
            -- Pointer is now at newp1.

            let x = unused

            let whileblock = concat $ bfeval0 : [              -- repeat (bfeval0) times:
                                goto newp0 x  ++ "[",
                                    bfeval1,                         --      bfeval1
                                    goto newp1 x,
                                    "-]" 
                             ]
            
            compile' prog ((x, un1, bf ++ whileblock), mem)
            -- Continue the compilation by setting the smallest unused cell to the one yielded by the evaluation of @body@,

            -- and appending the resulting BF String.
            -- Note: mem is passed instead of mem1 in order to perserve scope.
            
compile' (IfBlock cond body elif els : prog) ((p, unused, bf), mem)
        = do
            (newp0, bfeval0) <- eval cond ((p, unused), mem)
            -- Evaluates @cond@ at position `unused`. Pointer is now at newp0.

            ((newp1, un1, bfeval1), mem1) <- compile' body ((unused, unused+3, ""), mem)
            -- Evaluates the instructions within the @body@, with the pointer's starting position being `x`.
            -- Makes sure to pad unused by an entire 3 cells, as we make use of unused+1 and unused+2 as 
            -- temporary values when constructing the `if` block. The pointer is now at newp1.
   
            ((newp2, un2, bfeval2), mem2) <- case elif of 
                []      -> compile' els ((unused+1, un1, ""), mem)
                e:elifs -> compile' [uncurry IfBlock e elifs els] ((unused+1, un1, ""), mem)
            -- Evaluates the else/elif instruction, with the pointer's starting position being @x+1@.

            let x  = unused
            let t0 = unused+1
            let t1 = unused+2
            -- @x@ now stores the position where @cond@ was evaluated. @t0@ and @t1@ are temporary cells.

            let ifblock = concat $ bfeval0 : [                          -- if (bfeval0)
                            goto newp0 t0 ++ "[-]+",
                            goto t0 t1 ++ "[-]",
                            goto t1  x ++ "[",                          --      then (bfeval1)
                                bfeval1,
                                goto newp1 t0 ++ "-",
                                goto t0  x ++ "[" ++ goto x t1 ++ "+" ++ goto t1 x ++ "-]",
                            "]",
                            goto  x t1 ++ "[" ++ goto t1 x ++ "+" ++ goto x t1 ++ "-]",
                            goto t1 t0 ++ "[",                          --      else (bfeval2)
                                bfeval2,
                            goto newp2 t0 ++ "-]"
                       ]
     
            compile' prog ((t0, un2, bf ++ ifblock), mem)
            -- Continue the compilation by setting the smallest unused cell to the one yielded by the evaluation of @elif@,
            -- and appending the resulting BF String.
            -- Note: mem is passed instead of mem1 or mem2 in order to perserve scope.

-- | `eval` evaluates an @expr@, returning a pair consisting of the position of the pointer post-evaluation and the evaluated bf string. 
-- As arguments, it takes a pair consisting of a pair of the pointer's location and the location of the first unused cell, and a memory map.
-- The evaluation's computational result will always be placed in `unused`.
eval :: Expr -> ((Pointer, Pointer), Memory) -> Either Err (Pointer, String)
eval (ValE x) ((p, unused), _)
    | x < 0     = Left NegativeIntegerError -- Again, this is done to prevent underflows at runtime. No negatives allowed!
    | otherwise = pure (unused, goto p unused ++ outputPlus x)
    -- To evaluate a literal integer @x@, have the pointer go to the first unused location and output @x@ pluses.
    -- The final pointer's position will be that unused location.
                                            
eval (VarE v) ((p, unused), mem)
    = do 
    -- To evaluate a variable, you need to convert the Maybe monad returned by M.lookup to an error in case:
            -- 1. It returns Nothing
            -- 2. It returns Just 0 (0 is a header location, and still means that the variable is uninitialised in the global scope)
        varp <- note (UninitialisedMemory v) . checkifZero . lookup v $ mem
        -- A sanity check of sorts; checkifZero handles the Just 0 issue.
        pure $ assign unused varp [unused+1] p
        -- If all goes well, copy the variable's cell data to `unused`.
        where   
            checkifZero :: Maybe Int -> Maybe Int
            checkifZero (Just 0) = Nothing
            checkifZero x        = x

eval (BinOpE   Add x y) s = eval' x y addYToX 2 s
eval (BinOpE   Mul x y) s = eval' x y mulYToX 3 s
eval (BinOpE   Pow x y) s = eval' x y powXToY 4 s
eval (BinOpE   Div x y) s = eval' x y divXByY 5 s

eval (BinOpE    Eq x y) s = eval' x y  eqXByY 3 s
eval (BinOpE   Neq x y) s = eval' x y neqXByY 3 s

-- | Coming soon!
eval (BinOpE LessThan    _ _)  _ = Left $ InvalidOperation LessThan
eval (BinOpE GreaterThan _ _)  _ = Left $ InvalidOperation GreaterThan
eval (BinOpE LeqThan     _ _)  _ = Left $ InvalidOperation LeqThan
eval (BinOpE GeqThan     _ _)  _ = Left $ InvalidOperation GeqThan

-- | Given two expressions, a function that computes (pointer, BF String) pairs via assignment and consecutive temporary cells,
-- the number of said cells required, and a ((pointer, unused), memorymap) pair, `eval'` returns a pair consisting of the pointer's
-- location post-evaluation and the constructed BF string.
eval' :: Expr 
      -> Expr 
      -> (Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String))
      -> Pointer 
      -> ((Pointer, Pointer), Memory) 
      -> Either Err (Pointer, String)
eval' x y bfOp n ((p, unused), mem)
    = do 
        (destp0, bfeval0) <- eval x ((p, unused), mem)        -- expresion 1 is put in unused
        (destp1, bfeval1) <- eval y ((destp0, unused+1), mem) -- expresion 2 is put in unused+1
        let (destp2, bfeval2) = bfOp unused (unused+1) [unused+2..unused+n] destp1

        -- [unused] := [unused] bfOp [unused+1]
        -- (makes use of [unused+2]..[unused+n] as temporary cells)
        pure (destp2, bfeval0 ++ bfeval1 ++ bfeval2)

-- | ASSIGN |

-- [X] := [Y]
assign :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
assign x y [t] p 
    = if x /= y then (t, concat [
        goto p t ++ "[-]",
        goto t x ++ "[-]",
        goto x y ++ "[" ++ goto y x ++ "+" ++ goto x t ++ "+" ++ goto t y ++ "-]",
        goto y t ++ "[" ++ goto t y ++ "+" ++ goto y t ++ "-]"
    ]) else (p, "")
-- | Attribution: Anon

-- | ARITHMETIC OPERATIONS |

-- [X] := [X] + [Y]
addYToX :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
addYToX x y [t] p
    = (t, concat [
        goto p t ++ "[-]",
        goto t y ++ "[" ++ goto y x ++ "+" ++ goto x t ++ "+" ++ goto t y ++ "-]",
        goto y t ++ "[" ++ goto t y ++ "+" ++ goto y t ++ "-]"
    ])
-- | Attribution: Anon

-- [X] := [X] * [Y]
mulYToX :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
mulYToX x y [t0, t1] p
    = (t1, concat [
        goto p  t0 ++ "[-]",
        goto t0 t1 ++ "[-]",
        goto t1  x ++ "[" ++ goto x t1 ++ "+" ++ goto t1 x ++ "-]",
        goto  x t1 ++ "[",
            goto t1 y ++ "[" ++ goto y  x ++ "+" ++ goto x t0 ++ "+" ++ goto t0 y ++ "-]",
            goto y t0 ++ "[" ++ goto t0 y ++ "+" ++ goto y t0 ++ "-]",
        goto t0 t1 ++ "-]"
    ])
-- | Attribution: Anon

-- [X] := [X] ** [Y]
powXToY :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
powXToY x y [t0, t1, t2] p
    = (y, concat [
        goto  p t0 ++ "[-]",
        goto t0  x ++ "[" ++ goto x t0 ++ "+" ++ goto t0 x ++ "-]",
        "+",
        goto x y ++ "[",
            goto  y t1 ++ "[-]",
            goto t1 t2 ++ "[-]",
            goto t2  x ++ "[" ++ goto x t2 ++ "+" ++ goto t2 x ++ "-]",
            goto  x t2 ++ "[",
                goto t2 t0 ++ "[" ++ goto t0 x ++ "+" ++ goto x t1 ++ "+" ++ goto t1 t0 ++ "-]",
                goto t0 t1 ++ "[" ++ goto t1 t0 ++ "+" ++ goto t0 t1 ++ "-]",
            goto t1 t2 ++ "-]",
        goto t2 y ++ "-]"
    ])
-- | Attribution: chad3814
    
-- [X] := [X] / [Y]
divXByY :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
divXByY x y [t0, t1, t2, t3] p
    = (t0, concat [
        goto  p t0 ++ "[-]",
        goto t0 t1 ++ "[-]",
        goto t1 t2 ++ "[-]",
        goto t2 t3 ++ "[-]",
        goto t3  x ++ "[" ++ goto x t0 ++ "+" ++ goto t0 x ++ "-]",
        goto  x t0 ++ "[",
            goto t0  y ++ "[" ++ goto y t1 ++ "+" ++ goto t1 t2 ++ "+" ++ goto t2 y ++ "-]",
            goto  y t2 ++ "[" ++ goto t2 y ++ "+" ++ goto y t2 ++ "-]",
            goto t2 t1 ++ "[",
                goto t1 t2 ++ "+",
                goto t2 t0 ++ "-[" ++ goto t0 t2 ++ "[-]" ++ goto t2 t3 ++ "+" ++ goto t3 t0 ++ "-]",
                goto t0 t3 ++  "[" ++ goto t3 t0 ++ "+" ++ goto t0 t3 ++ "-]",
                goto t3 t2 ++ "[",
                    goto t2 t1 ++ "-",
                    "[" ++ goto t1 x ++ "-" ++ goto x t1 ++ "[-]" ++ "]" ++ "+",
                goto t1 t2 ++ "-]",
            goto t2 t1 ++ "-]",
            goto t1 x ++ "+",
        goto x t0 ++ "]"
    ])
-- | Attribution: Jeffry Johnston

-- | BOOLEAN OPERATIONS |

-- [X] := [X] == [Y]
eqXByY :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
eqXByY x y [t0, t1] p
    = (t1, concat [
        goto  p t0 ++ "[-]",
        goto t0 t1 ++ "[-]",
        goto t1  x ++ "[" ++ goto x t1 ++ "+" ++ goto t1  x ++ "-]+",
        goto  x  y ++ "[" ++ goto y t1 ++ "-" ++ goto t1 t0 ++ "+" ++ goto t0 y ++ "-]",
        goto  y t0 ++ "[" ++ goto t0 y ++ "+" ++ goto y t0 ++ "-]",
        goto t0 t1 ++ "[" ++ goto t1 x ++ "-" ++ goto x t1 ++ "[-]]"
    ])
-- | Attribution: Jeffry Johnston

-- [X] := [X] /= [Y]
neqXByY :: Pointer -> Pointer -> [Pointer] -> Pointer -> (Pointer, String)
neqXByY x y [t0, t1] p
    = (t1, concat [
        goto  p t0 ++ "[-]",
        goto t0 t1 ++ "[-]",
        goto t1  x ++ "[" ++ goto x t1 ++ "+" ++ goto t1  x ++ "-]",
        goto  x  y ++ "[" ++ goto y t1 ++ "-" ++ goto t1 t0 ++ "+" ++ goto t0 y ++ "-]",
        goto  y t0 ++ "[" ++ goto t0 y ++ "+" ++ goto y t0 ++ "-]",
        goto t0 t1 ++ "[" ++ goto t1 x ++ "+" ++ goto x t1 ++ "[-]]"
    ])
-- | Attribution: Jeffry Johnston

outputPlus :: Int -> String
outputPlus = (++) "[-]" . flip replicate '+'

goto :: Pointer -> Pointer -> String
goto src dest 
    | dest < src = replicate (src - dest) '<'
    | otherwise  = replicate (dest - src) '>'