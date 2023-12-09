{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Interpret where
-- 
-- {-
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Writer
-- import Machine
-- 
-- 
-- {-
-- interpret :: (MonadReader ROM m, MonadState (CPU, RAM) m, MonadWriter [String] m) => Machine -> m Machine
-- interpret m@Machine {} = do
--     -- get reset vector - for now it's 0
--     instr <- case lookup regs' IP o-}