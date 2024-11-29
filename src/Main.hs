{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE Unsafe      #-}
{-# OPTIONS_GHC -Wno-unsafe #-}

module Main (main) where

import Code
import Data.Char
import Data.Foldable
import Interpret
import Machine

code ∷ Code
code = Code.do
    copy screenRAMStart a
    halt
        where
            screenRAMStart = OpToIO (ToScreen 0)
            a = Unary (Const (OpImm (fromIntegral (ord 'A'))))

main ∷ IO ()
main = do
    (machine, debug) <- run . initial $ code
    print machine
    traverse_ print debug
