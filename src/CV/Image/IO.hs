{-#LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module CV.Image.IO where

import Bindings.DC1394
import Bindings.DC1394.Types
import Bindings.DC1394.Camera
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
getFrame :: Camera -> Int -> Int -> IO (Maybe (Image RGB D8))
getFrame camera' width height = alloca $ \(framePtr :: Ptr (Ptr C'dc1394video_frame_t)) ->
                    withCameraPtr camera' $ \camera -> do

    mode <- getMode camera
    when (mode /= c'DC1394_VIDEO_MODE_640x480_RGB8) $ error "Unsupported mode. Use 640x480 RGB8 for now"

    c'dc1394_capture_dequeue camera  c'DC1394_CAPTURE_POLICY_WAIT framePtr
    frameP  :: Ptr C'dc1394video_frame_t <- peek framePtr

    if frameP == nullPtr
        then c'dc1394_capture_enqueue camera frameP >> return Nothing
        else do   
                let corrupt = 0
                case corrupt of
                    0 -> do
                           dataPtr <- c'dc1394video_frame_t'image <$> (peek framePtr >>= peek)
                           r <- unsafe8UC_RGBFromPtr (width,height) dataPtr
                           c'dc1394_capture_enqueue camera frameP
                           return . Just $ r
                    _ -> c'dc1394_capture_enqueue camera frameP >> return Nothing

enumCamera
  :: MonadIO m =>
     Camera -> Int -> Int -> Step (Image RGB D8) m b -> Iteratee (Image RGB D8) m b
enumCamera camera width height = loop 
    where
     loop (Continue k) = do
                            x <- liftIO $ getFrame camera width height
                            case x of
                                Just i -> k (Chunks [i]) >>== loop  
                                Nothing -> k (Chunks []) >>== loop 
     loop s = returnI s

applyFrameAction :: (Image RGB D8 -> Int -> IO ()) -> Iteratee (Image RGB D8) IO ()
applyFrameAction frameAction = continue $ go 0
 where
    go :: Int -> Stream (Image RGB D8) -> Iteratee (Image RGB D8) IO ()
    go n (Chunks [])  = continue (go n)
    go n (Chunks [img]) = liftIO (frameAction img n) >> continue (go (n+1))
    go _ a            = yield () a 

withFrames :: Camera -> Int -> Int -> Integer -> (Image RGB D8 -> Int -> IO ()) -> Iteratee (Image RGB D8) IO ()
withFrames c width height n frameAction = enumCamera c width height $$ (E.isolate n =$ applyFrameAction frameAction)
