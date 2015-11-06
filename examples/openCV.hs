
{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import qualified CV.Image as CV
import qualified CV.Edges as CV
import qualified CV.Image.IO as CV
import Data.Enumerator (run_)
import Data.Maybe

main :: IO ()
main =
    withFirewireCamera ISO_400 Mode_640x480_RGB8 Rate_30 4 defaultFlags $ \camera -> do
         -- record 20 frames to output_0.png, output_1.png, output_2.png,...
         let numFrames = 60
         run_ $ CV.withFrames camera 640 480 numFrames $ \frame frameCount -> do
           CV.saveImage (show frameCount ++ "_edges.png") $ do
             let grayImage = (CV.unsafeImageTo8Bit . CV.rgbToGray . CV.unsafeImageTo32F) frame
             CV.unsafeImageTo32F (CV.canny 20 40 5 grayImage)
                   
         -- save two still images
         fromJust <$> CV.getFrame camera 640 480 >>= CV.saveImage "test_1.png"
         fromJust <$> CV.getFrame camera 640 480 >>= CV.saveImage "test_2.png"
