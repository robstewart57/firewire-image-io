
{-#LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Main where

import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import Canny
import System.Environment
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate hiding (fromIntegral,fst,(++))
import qualified Data.Array.Accelerate.CUDA as CUDA
import qualified Data.Array.Accelerate.IO as A
import qualified Data.Array.Accelerate.IO.Firewire as A
import Data.Array.Accelerate.Interpreter
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.BMP as Repa
import Data.Enumerator (run_)
import Data.Maybe
       
main :: IO ()
main = do
    numFrames <- (read . head) <$> getArgs 
    withFirewireCamera ISO_400 Mode_640x480_RGB8 Rate_30 4 defaultFlags $ \camera -> do
      -- computation on one frame
      Right frame <- A.getFrame camera 640 480
      let rgbaImg = (A.map A.packRGBA32 . A.flatten3dTo2d 480 640 . use) frame
          computation = (A.map A.rgba32OfLuminance .fst . canny 0.3 0.5) rgbaImg
          newImg  = CUDA.run computation
      A.writeImageToBMP "out_accel.bmp" newImg  

      -- computation on frame sequence
      let computation :: A.RGBImageDIM3 -> Acc (Array DIM2 A.RGBA32)
          computation = A.map A.rgba32OfLuminance -- convert graycale to RGB image
                        . fst                     -- get the resulting grayscale image
                        . canny 0.7 0.9           -- compute Canny edge detection
                        . A.map A.packRGBA32      -- convert to 2D RGB (R,G,B) triple
                        . A.flipV                 -- flip it in preparation for BMP file output
                        . A.flatten3dTo2d 480 640 -- flatten 3D (Word8) image to 2D (Word8,Word8,Word8) image
                        . use                     -- makes frame available for processing
      run_ $ A.withFrames camera 640 480 numFrames $ \frame frameCount -> do
         A.writeImageToBMP
              (show frameCount ++ "_out.bmp")
              (CUDA.run (computation frame)) -- run on the GPU
