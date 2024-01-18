{-# LANGUAGE Unsafe #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Interpret where

import Code
-- import Control.Monad.Reader
import Control.Monad.RWS
-- import Control.Monad.State
-- import Control.Monad.Writer
import Data.Array        as Array
import Data.Foldable     (toList)
-- import Data.Sequence.NonEmpty qualified as NES
-- import Data.Vector (Vector)
-- import Data.Vector qualified as V
import Machine

interpretWhen ∷ (Monad m) ⇒ Cond → Flags → m () → m ()
interpretWhen cond flags' action = case cond of
    Is flag  -> when (flags' ! flag) action
    Not flag -> unless (flags' ! flag) action
    Always   -> action

interpretInstruction ∷ (MonadState (CPU, RAM) m, MonadWriter [String] m) ⇒ Instruction → m ()
interpretInstruction instruction = do
    flags' <- gets (flags . fst)
    case instruction of
        Copy cond opTo op -> interpretWhen cond flags' $ do
            tell [show opTo <> " " <> show op]
        Branch cond addr -> interpretWhen cond flags' $ do
            tell ["branch to " <> show addr]
        Halt cond -> interpretWhen cond flags' $ do
            tell ["halt"]

interpret ∷ (MonadReader (Array Addr Instruction) m, MonadState (CPU, RAM) m, MonadWriter [String] m) ⇒ m ()
interpret = do
    ip' <- gets (ip . fst)
    instr <- asks (! ip')
    interpretInstruction instr

toArr ∷ ROM → Array Addr Instruction
toArr rom = listArray (0, fromIntegral (length instrs - 1)) instrs where
    instrs = toList . getCode $ rom

run ∷ MonadIO m ⇒ Machine → m (Machine, [String])
run m@Machine { cpu = cpu', ram = ram', rom = rom' } = do
    -- get reset vector - for now it's 0
    let code = toArr rom'

    ((), (cpu'', ram''), written) <- runRWST interpret code (cpu', ram')

    Prelude.mapM_ (liftIO . print) written

    pure (m { cpu = cpu'', ram = ram'' }, written)
