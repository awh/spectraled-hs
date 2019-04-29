module Main where

import Math.FFT (dftRC)
import Data.Array.IArray (listArray, amap, elems)
import Data.Array.CArray (CArray)
import Data.Complex (Complex, magnitude)

import Sound.Pulse.Simple

import GHC.Conc (TVar,writeTVar,atomically,newTVar,forkIO,readTVar)
import Control.Monad (forever)

import Graphics.Gloss.Interface.IO.Animate (animateIO)
import Graphics.Gloss.Data.Color (black,white)
import Graphics.Gloss.Data.Controller (Controller)
import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Picture (Picture(..),Point,blank,line)

source = "alsa_output.pci-0000_00_1b.0.analog-stereo.monitor"

sampleWindow :: Int
sampleWindow = 1024


-- Copy samples from source into a TVar indefinitely
capture :: Maybe String -> TVar [Float] -> IO ()
capture source sink = do
    s <- simpleNew Nothing "spectraled" Record source "capture" sampleSpec Nothing bufferAttr
    forever $ copySamples s
    where
        bufferAttr = Just $ BufferAttr Nothing Nothing Nothing Nothing (Just $ sampleWindow * 4)
        sampleSpec = SampleSpec (F32 LittleEndian) 44100 1

        copySamples s = do
            samples <- simpleRead s sampleWindow :: IO [Float]
            atomically $ writeTVar sink samples

render :: TVar [Float] -> IO ()
render samples = 
    animateIO display white renderFrame callback
    where
        display = InWindow "spectraled" (sampleWindow, 200) (0, 0)

        renderFrame :: Float -> IO Picture
        renderFrame _ = do
            ss <- atomically $ readTVar samples
            return $ line $ samplesToPath (-256) $ fft ss

        fft :: [Float] -> [Float]
        fft ss = elems $ amap magnitude $ dftRC $ listArray (0, (length ss - 1)) ss

        samplesToPath :: Float -> [Float] -> [Point]
        samplesToPath _ [] = []
        samplesToPath i (s:ss) = (i, s * 50) : samplesToPath (i + 1) ss

        callback :: Controller -> IO ()
        callback _ = return ()

main :: IO ()
main = do
    samples <- atomically $ newTVar $ take sampleWindow $ repeat (0::Float)
    captureTID <- forkIO $ capture Nothing samples
    renderTID <- forkIO $ render samples
    _ <- getChar
    return ()

{-
foo :: CArray Int Float -> CArray Int (Complex Float)
foo = dftRC

bar :: [Float] -> CArray Int Float
bar fs = listArray (0, (length fs - 1)) fs

    s <- simpleNew Nothing "example" Record (Just source) "this is an example application"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    xs <- simpleRead s sampleWindow :: IO [Float]
    simpleFree s
    let res = foo . bar
    putStrLn $ show $ amap magnitude $ res xs
-}
