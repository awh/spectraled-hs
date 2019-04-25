module Main where

import Math.FFT (dftRC)
import Data.Array.IArray (listArray, amap)
import Data.Array.CArray (CArray)
import Data.Complex (Complex, magnitude)

import Sound.Pulse.Simple

foo :: CArray Int Float -> CArray Int (Complex Float)
foo = dftRC

bar :: [Float] -> CArray Int Float
bar fs = listArray (0, (length fs - 1)) fs

main :: IO ()
main = do
    s <- simpleNew Nothing "example" Record Nothing "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    xs <- simpleRead s (128) :: IO [Float]
    simpleFree s
    let res = foo . bar
    putStrLn $ show $ amap magnitude $ res xs
