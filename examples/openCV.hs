
{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Bindings.DC1394.CameraOps
import Bindings.DC1394.Types
import qualified CV.Image.IO as CV
import System.Firewire.IO (withFirewireCamera)
import Data.Enumerator (run_)
import Data.Maybe

main :: IO ()
main =
    withFirewireCamera ISO_400 Mode_640x480_RGB8 Rate_30 $ \camera -> do                   
         -- record 20 frames to output_0.png, output_1.png, output_2.png,...
         run_ (CV.saveClip camera 20 "output")
         -- save two still images
         fromJust <$> CV.getFrame camera >>= CV.saveImage "test_1.png"
         fromJust <$> CV.getFrame camera >>= CV.saveImage "test_2.png"
