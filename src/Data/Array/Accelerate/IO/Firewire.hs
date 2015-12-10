{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}

module Data.Array.Accelerate.IO.Firewire
    (
    -- * Get firewire frame and format to 'RGBAImageDIM2'
      getFrame
    -- * Actions on frame sequences
    , withFrames
    -- * Functions to manipulate images inside accelerate arrays
    , flatten3dTo2d
    , flipV
    -- * Types
    , RGBAImageDIM2, RGBImageDIM3
    )
   where
     
import Bindings.DC1394
import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import Control.Monad
import Control.Monad.Trans hiding (lift)
import Data.Array.Accelerate hiding ((++))
import Data.Array.Accelerate.IO
import Data.Enumerator hiding (peek)
import qualified Data.Enumerator.List as E
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (reverse)

type RGBAImageDIM2 = Array DIM2 (Word8,Word8,Word8,Word8)
type RGBImageDIM3 = Array DIM3 Word8

-- | flatten a 3D (Word8) image to a 2D (Word8,Word8,Word8) image.
flatten3dTo2d :: Int -> Int -> Acc RGBImageDIM3 -> Acc RGBAImageDIM2
flatten3dTo2d = img3dTo2dTriplePattern 0 1 2 -- (R,G,B)

-- | flip an (R,G,B) 2D image vertically.
--   This may appear in the accelerate library at some future date, see
--   https://github.com/AccelerateHS/aaccelerate/issues/289#issuecomment-163448880
flipV
    :: Elt a
    => Acc (Array DIM2 a)
    -> Acc (Array DIM2 a)
flipV arr =
  let Z :. h :. _ = unlift (shape arr)          :: Z :. Exp Int :. Exp Int
      p ix        = let Z :. y :. x = unlift ix
                    in  index2 (h - y - 1) x
  in
  backpermute (shape arr) p arr


-- | Grab a frame from the camera. Currently does only 640x480 RGB Images.
--   This currently consumes from the foreign pointer from the camera in
--   reverse order, so the user should do:
--
-- @
--     Right frame <- getFrame camera
--     let rgbAImg = (map packRGBA32 . reverseThen3dTo2d 480 640) frame
-- @
getFrame :: Camera -> Int -> Int -> IO (Either String RGBImageDIM3)
getFrame camera' width height = alloca $ \(framePtr :: Ptr (Ptr C'dc1394video_frame_t)) ->
                    withCameraPtr camera' $ \camera -> do

    mode <- getMode camera
    when (mode /= c'DC1394_VIDEO_MODE_640x480_RGB8) $ error "Unsupported mode. Use 640x480 RGB8 for now"

    void $ c'dc1394_capture_dequeue camera  c'DC1394_CAPTURE_POLICY_WAIT framePtr
    frameP  :: Ptr C'dc1394video_frame_t <- peek framePtr

    if frameP == nullPtr
        then c'dc1394_capture_enqueue camera frameP >> return (Left "no firewire frame available")
        else do   
                let corrupt = 0
                case corrupt of
                    (0::Int) -> do
                           dataPtr <- c'dc1394video_frame_t'image <$> (peek framePtr >>= peek)
                           (arr :: Array (Z :. Int :. Int :. Int) Word8) <- fromPtr  (Z :. height :. width :. 3) ((),dataPtr)
                           void $ c'dc1394_capture_enqueue camera frameP
                           return (Right arr)
                    _ -> c'dc1394_capture_enqueue camera frameP >> return (Left "firewire image corrupt")


enumCamera
  :: MonadIO m =>
     Camera -> Int -> Int -> Step RGBImageDIM3 m b -> Iteratee RGBImageDIM3 m b
enumCamera camera width height = loop 
    where
     loop (Continue k) = do
                            x <- liftIO $ getFrame camera width height
                            case x of
                                Right img -> k (Chunks [img]) >>== loop  
                                Left _err -> k (Chunks []) >>== loop 
     loop s = returnI s

applyFrameAction :: (RGBImageDIM3 -> Int -> IO ()) -> Iteratee RGBImageDIM3 IO ()
applyFrameAction frameAction = continue $ go 0
 where
    go :: Int -> Stream RGBImageDIM3 -> Iteratee RGBImageDIM3 IO ()
    go n (Chunks [])  = continue (go n)
    go n (Chunks [img]) = liftIO (frameAction img n) >> continue (go (n+1))
    go _ a            = yield () a 

withFrames c width height n frameAction = enumCamera c width height $$ (E.isolate n =$ applyFrameAction frameAction)

                         
--------------------------
-- not exported

index3 i j k = lift (Z :. i :. j :. k)

img3dTo2dTriplePattern :: Int -> Int -> Int -> Int -> Int -> Acc RGBImageDIM3 -> Acc RGBAImageDIM2
img3dTo2dTriplePattern idx0 idx1 idx2 h' w' img3d = 
    let height = lift h'
        width = lift w'
    in generate (index2 height width)
                    (\ix -> let (Z :. (h::Exp Int) :. (w::Exp Int)) = unlift ix
                                r = img3d ! index3 h w (constant idx0) :: Exp Word8
                                g = img3d ! index3 h w (constant idx1) :: Exp Word8
                                b = img3d ! index3 h w (constant idx2) :: Exp Word8
                              in lift (r,g,b,constant 0))
