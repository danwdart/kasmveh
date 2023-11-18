{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE Unsafe      #-}
{-# OPTIONS_GHC -Wno-unsafe -Wno-unused-top-binds #-}

module FizzBuzz where

import Code
import Data.Char
-- import Machine
import Text.Pretty.Simple

example1 ∷ Code
example1 = Code.do
    copy screenRAMStart a
    halt
    where
        screenRAMStart = OpToIO (ToScreen 0)
        a = Unary (Const (OpImm (fromIntegral (ord 'A'))))

showExample ∷ IO ()
showExample = pPrint example1
