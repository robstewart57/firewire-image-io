
{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Bindings.DC1394.CameraOps
import Bindings.DC1394.Types
import qualified CV.Image.IO as CV
import Data.Enumerator (run_)
import Data.Maybe

main :: IO ()
main = do
    -- use dc1394 bindings to set up camera
    dc <- getDC1394 --c'dc1394_new 
    (e:_) <- getCameras dc
    print e
    print ("Trying camera", e)
    cam <- cameraFromID dc e-- c'dc1394_camera_new dc guid
    print ("Camera can do oneshots", oneShotCapable cam)
    setISOSpeed  cam ISO_400
    setVideoMode cam Mode_640x480_RGB8
    setFrameRate cam Rate_30
    setupCamera cam 4 defaultFlags
    startVideoTransmission cam

    -- use CV.Image.IO in firewire-image-io library to..
    -- 1. record 20 frames to output_0.png, output_1.png ...
    run_ (CV.saveClip cam 20 "output")
    -- 2. save two still images
    fromJust <$> CV.getFrame cam >>= CV.saveImage "test_1.png"
    fromJust <$> CV.getFrame cam >>= CV.saveImage "test_2.png"
    
    -- use dc1394 bindings to stop the camera
    stopVideoTransmission cam
    stopCapture cam


