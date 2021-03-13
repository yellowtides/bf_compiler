module Errors where

import InternalRepr ( Op )

-- | Enumerates reasons for errors.
data Err
    = UninitialisedMemory String        -- ^ Tried to read from a variable
                                        -- that does not exist.
    | NegativeIntegerError              -- ^ Negative Integer encountered 
                                        -- during parsing
    | InvalidOperation Op               -- ^ Operation not (yet!) implemented
    deriving ( Eq, Show )