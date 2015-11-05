{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts #-}

module Data.Array.Accelerate.IO.Firewire
    (
    -- * Get firewire frame
      getFrame
    -- * Functions to manipulate images inside accelerate arrays
    , img3dTo2d
    , reversedImg3dTo2d
    , reverseImg3d
    , reverseThen3dTo2d
    -- * Types
    , RGBAImageDIM2, RGBImageDIM3
    ) where

import Bindings.DC1394
import Bindings.DC1394.Camera
import Bindings.DC1394.Types
import Control.Monad
import Data.Array.Accelerate hiding ((++))
import Data.Array.Accelerate.IO
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (reverse)

type RGBAImageDIM2 = Array DIM2 (Word8,Word8,Word8,Word8)
type RGBImageDIM3 = Array DIM3 Word8

index3 i j k = lift (Z :. i :. j :. k)

reverseThen3dTo2d :: Int -> Int -> Acc RGBImageDIM3 -> Acc RGBAImageDIM2
reverseThen3dTo2d h w = reversedImg3dTo2d  h w . reverseImg3d h w
                        
img3dTo2d :: Int -> Int -> Acc RGBImageDIM3 -> Acc RGBAImageDIM2
img3dTo2d = img3dTo2dTriplePattern 0 1 2 -- (R,G,B)

reversedImg3dTo2d :: Int -> Int -> Acc RGBImageDIM3 -> Acc RGBAImageDIM2
reversedImg3dTo2d = img3dTo2dTriplePattern 2 1 0 -- (B,G,R)

reverseImg3d :: Int -> Int -> Acc RGBImageDIM3 -> Acc RGBImageDIM3
reverseImg3d h w = reshape (lift (Z :. (h::Int) :. (w::Int) :. (3::Int))) . reverse . flatten
                    
-- | Grab a frame from the camera. Currently does only 640x480 RGB Images.
--   This currently consumes from the foreign pointer from the camera in
--   reverse order, so the user should do:
--
-- @
--     Right frame <- getFrame camera
--     let rgbAImg = (map packRGBA32 . reverseThen3dTo2d 480 640) frame
-- @
getFrame :: Camera -> IO (Either String RGBImageDIM3)
getFrame camera' = alloca $ \(framePtr :: Ptr (Ptr C'dc1394video_frame_t)) ->
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
                           (arr :: Array (Z :. Int :. Int :. Int) Word8) <- fromPtr  (Z :. 480 :. 640 :. 3) ((),dataPtr)
                           void $ c'dc1394_capture_enqueue camera frameP
                           return (Right arr)
                    _ -> c'dc1394_capture_enqueue camera frameP >> return (Left "firewire image corrupt")

--------------------------
-- not exported

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
