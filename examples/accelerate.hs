
{-#LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}
module Main where

import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import Data.Array.Accelerate hiding (fromIntegral,fst)
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.IO as A
import Data.Array.Accelerate.IO.Firewire
import qualified Data.Array.Accelerate.IO.Firewire as A (getFrame)
-- import qualified Data.Array.Accelerate.CUDA as CUDA
import Data.Array.Accelerate.Interpreter
import Data.Enumerator (run_)
import Data.Maybe
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.IO.BMP as Repa
import Canny

index3 i j k = lift (Z :. i :. j :. k)
             
main :: IO ()
main =
    withFirewireCamera ISO_400 Mode_640x480_RGB8 Rate_30 4 defaultFlags $ \camera -> do
      Right frame <- A.getFrame camera
      let rgbaImg = (A.map A.packRGBA32
                    -- foreign pointer populates accelerate array in reverse for some reason
                    . reverseThen3dTo2d 480 640
                    . use) frame
          computation = (A.map A.rgba32OfLuminance .fst . canny 0.7 0.9) rgbaImg
          newImg  = run computation
      A.writeImageToBMP "out_accel.bmp" newImg  

