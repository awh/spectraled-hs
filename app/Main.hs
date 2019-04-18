module Main where

import Math.FFT (dftRC)
import Data.Array.CArray (CArray)
import Data.Complex (Complex)

import Sound.Pulse.Simple

foo :: CArray Int Float -> CArray Int (Complex Float)
foo = dftRC

main :: IO ()
main = do
    s <- simpleNew Nothing "example" Record Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    xs <- simpleRead s $ 44100*10 :: IO [Float]
    simpleFree s
