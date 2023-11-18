{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Machine where

import Code
import Data.Map (Map)
import Data.Map qualified as M

data CPU = CPU {
    regs  :: Map Reg MWord,
    flags :: Map Flag Bool
}

type ROM = Code

newtype RAM = RAM {
    getRAM :: Map Addr MWord
}

{-}
data KeyboardStatus = KeyboardStatusBusy | KeyboardStatusIdle | KeyboardStatusReady

type KeyboardScanCode = Int

data Keyboard = Keyboard {
    status :: KeyboardStatus,
    scanCode :: Maybe KeyboardScanCode
}

initialKeyboard :: Keyboar
initialKeyboard = Keyboard KeyboardStatusIdle Nothing

data NetworkStatus = NetworkStatusBusy | NetworkStatusIdle | NetworkStatusReady

type NetworkData = String 

data Network = Network {
    status :: NetworkStatus,
    data :: Maybe NetworkData
}

newtype Shadowed a = Shadowed {
    getIO :: a,
    getRAM :: RAM
-}

 -- @TODO translate bus address to rom and ram address and do IO if using the IO region.

data Machine = Machine {
    cpu      :: CPU,
    rom      :: ROM,
    ram :: RAM
    {-}
    generalRAM :: RAM,
    videoRAM :: RAM,
    keyboard :: Shadowed Keyboard,
    network :: Shadowed Network
    -}
    
    -- ioRegion :: IORegion
}

{-}
totalGeneralRAM ∷ Addr
totalGeneralRAM = 16

totalVideoRAM :: Addr
totalVideoRAM = columns * rows * bytesPerCharacterSprite where
    columns = 80
    rows = 25
    bytesPerCharacterSprite = bytesPerCharacter + bytesForColour where
        bytesPerCharacter = 1
        bytesForColour = 1
-}

totalRAM :: Addr
totalRAM = 16

initial ∷ ROM → Machine
initial rom' = Machine {
    cpu = CPU {
        regs = M.empty,
        flags = M.empty
    },
    rom = rom',
    ram = RAM $ M.fromList (map (, 0) [0..totalRAM - 1])
    {-}
    generalRAM = RAM $ M.fromList (map (, 0) [0..totalGeneralRAM - 1]),
    videoRAM = RAM $ M.fromList (map (, 0) [0..totalVideoRAM - 1]),
    keyboard = RAM $ M.fromList [keyboardStatus, keyboardValue] where
        keyboardStatus = 0,
        keyboardValue = 0
    -- ioRegion = IORegion M.empty
    -}
}


data FromIOAddr = FromKeyboard Addr | FromNetworkInterface Addr | FromAudioInterface Addr | FromSystemConfigurationInterface Addr
    deriving stock (Show)

data ToIOAddr = ToScreen Addr | ToNetworkInterface Addr | ToAudioInterface Addr | ToSystemConfigurationInterface Addr
    deriving stock (Show)