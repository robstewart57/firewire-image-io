{-#LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module CV.Image.IO where

import Bindings.DC1394
import Bindings.DC1394.Types
import Bindings.DC1394.CameraOps
import CV.Conversions
import CV.Image hiding (saveImage)
import qualified CV.Image as CV
import Data.Enumerator hiding (peek)
import qualified Data.Enumerator.List as E
import Control.Monad
import Control.Monad.Trans
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable


-- | Grab a frame from the camera. Currently does only 640x480 RGB D8 Images.
getFrame :: Camera -> IO (Maybe (Image RGB D8))
getFrame camera' = alloca $ \(framePtr :: Ptr (Ptr C'dc1394video_frame_t)) ->
                    withCameraPtr camera' $ \camera -> do

    mode <- getMode camera
    when (mode /= c'DC1394_VIDEO_MODE_640x480_RGB8) $ error "Unsupported mode. Use 640x480 RGB8 for now"

    c'dc1394_capture_dequeue camera  c'DC1394_CAPTURE_POLICY_WAIT framePtr
    frameP  :: Ptr C'dc1394video_frame_t <- peek framePtr

    if frameP == nullPtr  -- Perhaps there was no frame available?
        then c'dc1394_capture_enqueue camera frameP >> return Nothing
        else do   
                --corrupt <- c'dc1394_capture_is_frame_corrupt camera frameP -- Or it was corrupted?
                -- For some reason this seems always true on os x
                let corrupt = 0
                case corrupt of
                    0 -> do -- Yay! A frame!
                           dataPtr <- c'dc1394video_frame_t'image <$> (peek framePtr >>= peek)
                           r <- unsafe8UC_RGBFromPtr (640,480) dataPtr
                           c'dc1394_capture_enqueue camera frameP
                           return . Just $ r
                    _ -> c'dc1394_capture_enqueue camera frameP >> return Nothing

enumCamera
  :: MonadIO m =>
     Camera -> Step (Image RGB D8) m b -> Iteratee (Image RGB D8) m b
enumCamera camera = loop 
    where
     loop (Continue k) = do
                            x <- liftIO $ getFrame camera
                            case x of
                                Just i -> k (Chunks [i]) >>== loop  
                                Nothing -> k (Chunks []) >>== loop 
     loop s = returnI s

-- | re-export 'saveImage' from the CV library
saveImage :: Save (CV.Image c d) => FilePath -> CV.Image c d -> IO ()
saveImage = CV.saveImage

saveIm :: String -> Iteratee (Image RGB D8) IO ()
saveIm filename = continue $ go 0
 where
    go :: Int -> Stream (Image RGB D8) -> Iteratee (Image RGB D8) IO ()
    go n (Chunks [])  = continue (go n)
    go n (Chunks [i]) = liftIO (saveImage (filename++show n++".png") i) >> continue (go (n+1))
    go _ a            = yield () a 

saveClip c n filename = enumCamera c $$ (E.isolate n =$ saveIm filename)
