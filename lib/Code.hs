{-# LANGUAGE GeneralisedNewtypeDeriving, Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Code where
import Data.Int
import Data.Word
-- import Data.Map qualified as M
-- import Data.Map (Map)
import Data.Sequence.NonEmpty as NES

-- import Data.Vector.NonEmpty -- use once compiled up
-- import Data.Sequence.NonEmpty -- use when putting together in pseudo-monads or free monads

data Reg = A | B | C | D | O | IP
    deriving stock (Show)

{-}
consolidate? lens?
data Regs a = Regs {
    a :: a,
    b :: a,
    c :: a,
    d :: a,
    o :: a,
    ip :: a
}
-}

data Flag = Zero | Carry
    deriving stock (Show)

{-
consolidate? lens?
data Flags = Flags {
    zero :: Bool,
    carry :: Bool
}
-}

data Cond = Is Flag | Not Flag | Always
    deriving stock (Show)

type MWord = Int8
type Addr = Word8

data FromIOAddr = FromKeyboard Addr | FromNetworkInterface Addr | FromAudioInterface Addr
    deriving stock (Show)

data ToIOAddr = ToScreen Addr | ToNetworkInterface Addr | ToAudioInterface Addr
    deriving stock (Show)

data OpFrom = OpImm MWord | OpFromReg Reg | OpFromRAM Addr | OpFromROM Addr | OpFromIO FromIOAddr -- @TODO consolidate and translate from/to bus?
    deriving stock (Show)

data OpWith = OpWithReg Reg | OpWithRAM Addr | OpWithROM Addr | OpWithIO FromIOAddr -- @TODO consolidate and translate from/to bus?
    deriving stock (Show)

data OpTo = OpToReg Reg | OpToRAM Addr | OpToIO ToIOAddr    
    deriving stock (Show)

data UnaryOperation = Const OpFrom | Negate OpFrom
    deriving stock (Show)

data BinaryOperation = Add OpFrom OpWith | Sub OpFrom OpWith | Mult OpFrom OpWith | Mod OpFrom OpWith
    deriving stock (Show)

data Operation = Unary UnaryOperation | Binary BinaryOperation
     deriving stock (Show)

data Instruction =
    Copy Cond OpTo Operation |
    Branch Cond Addr |
    Halt Cond
    deriving stock (Show)

newtype Code = Code {
    getCode :: NES.NESeq Instruction
}
    deriving stock (Show)
    deriving newtype (Semigroup)

one :: (a -> Instruction) -> a -> Code
one inst a = Code . NES.singleton $ inst a

two :: (a -> b -> Instruction) -> a -> b -> Code
two inst a b = Code . NES.singleton $ inst a b

three :: (a -> b -> c -> Instruction) -> a -> b -> c -> Code
three inst a b c = Code . NES.singleton $ inst a b c

copyIf :: Cond -> OpTo -> Operation -> Code
copyIf = three Copy

copy :: OpTo -> Operation -> Code
copy = copyIf Always

branchIf :: Cond -> Addr -> Code
branchIf = two Branch

branch :: Addr -> Code
branch = branchIf Always

haltIf :: Cond -> Code
haltIf = one Halt

halt :: Code
halt = haltIf Always

-- ehhh it's okay, it's not a monad
(>>) :: Code -> Code -> Code
(>>) = (<>)