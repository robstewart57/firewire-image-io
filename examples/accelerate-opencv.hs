
{-#LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Main where

import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import qualified CV.Image as CV
import qualified CV.Image.IO as CV
import Data.Array.Accelerate hiding (fromIntegral,fst)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import Data.Array.Accelerate.IO.Firewire
import qualified Data.Array.Accelerate.IO.Firewire as A -- (getFrame,formatFrame)
import qualified Data.Array.Accelerate.CUDA as CUDA
import Data.Enumerator (run_)
import Data.Maybe
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.BMP as Repa
import Canny

index3 i j k = lift (Z :. i :. j :. k)
             
main :: IO ()
main = do
    -- map a firewire frame into an accelerate array, write image to BMP file.
    withFirewireCamera ISO_400
                       Mode_640x480_RGB8 Rate_30
                       4
                       defaultFlags $ \camera -> do
      Right frame <- A.getFrame camera 640 480
      let rgbaImg = (A.map A.packRGBA32 . A.flatten3dTo2d 480 640 . use) frame
          computation = (A.map A.rgba32OfLuminance .fst . canny 0.7 0.9) rgbaImg
          -- run Canny edge detection on a GPU
          newImg  = CUDA.run computation
      A.writeImageToBMP "out_accelerate.bmp" newImg  

    -- map a firewire frame into an OpenCV image, write image to PNG file.
    withFirewireCamera ISO_400
                       Mode_640x480_RGB8 Rate_30
                       4
                       defaultFlags $ \camera -> do
      -- just save OpenCV image to file
      fromJust <$> CV.getFrame camera 640 480 >>= CV.saveImage "out_openCV.png"
