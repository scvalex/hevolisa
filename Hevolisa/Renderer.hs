--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Renderer (drawingError,
                          withImageFromPNG,
                          drawingToFile) 
where

import Data.Array.Parallel.PArray
import Data.ByteString (unpack)
import Directory
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Shapes.DnaPolygon
import qualified Hevolisa.Shapes.DnaBrush as B
import Hevolisa.Shapes.DnaPoint
import qualified Hevolisa.Settings as S
import qualified Hevolisa.Vector as V


class Renderable a where
    render :: a -> C.Render ()

instance Renderable DnaPoint where
    render (DnaPoint x y) = C.lineTo x y

instance Renderable DnaPolygon where
    render p = do
      render $ brush p
      render $ points p
      C.fill

instance Renderable B.DnaBrush where
    render br = C.setSourceRGBA r g b a
        where r = normalize $ B.red br
              g = normalize $ B.green br
              b = normalize $ B.blue br
              a = normalize $ B.alpha br
              normalize = (/255) . fromIntegral

instance Renderable DnaDrawing where
    render = render . polygons

instance (Renderable a) => Renderable [a] where
    render = mapM_ render


-- | 1. Rasterize the drawing
--
-- 2. Load an image from a file and rasterize it
--
-- 3. Compare the color values of the drawing and the image pixel by pixel
drawingError :: DnaDrawing -- ^ the drawing is rasterized
             -> PArray Int -- ^ rasterize an image from a file
             -> IO Int -- ^ return the color pixel error
drawingError drawing image = toSurface (render drawing) >>= 
                             unpackSurface >>= 
                             return . Hevolisa.Renderer.error image

--error :: [Int] -> [Int] -> Int
--error c1 c2 = sum $ zipWith (\x y -> (x - y)^2) c1 c2

error :: PArray Int -> [Int] -> Int
error c1 c2 = V.error_wrapper c1 (fromList c2)
                          
toSurface :: C.Render () -> IO C.Surface
toSurface r = do surface <- C.createImageSurface C.FormatRGB24 width height
                 C.renderWith surface r
                 return surface

-- | remove the alpha channel
unpackSurface :: C.Surface -> IO [Int]
unpackSurface s = C.imageSurfaceGetData s >>=
                  return . removeAlpha . map fromIntegral . unpack
    where
      removeAlpha :: [a] -> [a]
      removeAlpha []           = []
      removeAlpha (r:g:b:a:xs) = r:g:b:removeAlpha xs
      removeAlpha _            = Prelude.error "wrong number of color values"

withImageFromPNG :: FilePath -> (PArray Int -> IO a) -> IO a
withImageFromPNG fp f = C.withImageSurfaceFromPNG fp unpackSurface >>= 
                        return . fromList >>= f
                 
drawingToFile :: DnaDrawing -> Int -> IO ()
drawingToFile d n = C.withImageSurface C.FormatRGB24 width height $ \result -> do
                      C.renderWith result $ render d
                      dirExists <- doesDirectoryExist subdir
                      if dirExists then return () else createDirectory subdir
                      C.surfaceWriteToPNG result (subdir ++ "/" ++ show n ++ ".png")

subdir = "images"

height = truncate S.maxHeight :: Int
width  = truncate S.maxWidth  :: Int