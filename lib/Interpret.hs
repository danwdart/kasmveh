{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Interpret where
 
import Code
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Machine

interpretInstruction :: (MonadReader ROM m, MonadState (CPU, RAM) m, MonadWriter [String] m) => Code -> m ()
 
interpret :: (MonadReader ROM m, MonadState (CPU, RAM) m, MonadWriter [String] m) => m ()
interpret m@Machine { regs = regs' } = do
    -- get reset vector - for now it's 0
    case regs' ! IP
    i