{-# LANguAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Machine where

import Code
import Data.Map qualified as M
import Data.Map (Map)

data CPU = CPU {
    regs :: Map Reg MWord,
    flags :: Map Flag Bool
}

type ROM = Code

newtype RAM = RAM {
    getRAM :: Map Addr MWord
}

newtype IORegion = IORegion {
    getIORegionShadowRAM :: Map Addr MWord
}

 -- @TODO translate bus address to rom and ram address and do IO if using the IO region.

data Machine = Machine {
    cpu :: CPU,
    rom :: ROM,
    ram :: RAM,
    ioRegion :: IORegion
}

totalRAM :: Addr
totalRAM = 16

initial :: ROM -> Machine
initial rom' = Machine {
    cpu = CPU {
        regs = M.empty,
        flags = M.empty
    },
    rom = rom',
    ram = RAM $ M.fromList (zip [0..totalRAM - 1] (repeat 0)),
    ioRegion = IORegion M.empty
}