
module System.Firewire.IO (withFirewireCamera) where

import Bindings.DC1394.CameraOps
import Bindings.DC1394.Types
import qualified CV.Image.IO as CV
import Data.Enumerator (run_)
import Data.Maybe

-- | wrapper around dc1394 bindings that sets up a firewire
--   camera, performs the user action with the camera, then
--   stops the transmission and stops the camera.
withFirewireCamera :: ISOSpeed         -- ^ ISO rating for light sensitivityo
           -> VideoMode        -- ^ video mode
           -> Framerate        -- ^ capture frame rate
           -> (Camera -> IO a) -- ^ user camera action
           -> IO ()
withFirewireCamera speed mode rate action = do
    -- use dc1394 bindings to set up camera
    dc <- getDC1394 --c'dc1394_new 
    (e:_) <- getCameras dc
    cam <- cameraFromID dc e -- c'dc1394_camera_new dc guid
    setISOSpeed  cam speed
    setVideoMode cam mode
    setFrameRate cam rate
    setupCamera
      cam          -- camera
      4            -- DMA buffers
      defaultFlags -- capture flag
    startVideoTransmission cam

    -- perform user action with the camera
    action cam

    -- use dc1394 bindings to stop the camera
    stopVideoTransmission cam
    stopCapture cam


