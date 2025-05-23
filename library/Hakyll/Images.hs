-- |
-- Module      : Hakyll.Images
-- Description : Hakyll utilities for image files
-- Copyright   : (c) Laurent P René de Cotret, 2019 - present
-- License     : BSD3
-- Maintainer  : laurent.decotret@outlook.com
-- Stability   : unstable
-- Portability : portable
--
-- This package defines a few Hakyll compilers. These compilers help deal with images
-- in the context of Hakyll programs, such as JPEG compression or image resizing.
--
-- Items must be loaded before compilers can be used, like so:
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , resizeImageCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Resize all profile pictures with .png extensions to 64x48
--         match "profiles/**.png" $ do
--             route idRoute
--             compile $ loadImage
--                 >>= resizeImageCompiler 64 48
--
--         (... omitted ...)
-- @
--
-- Compilers can be sequenced easily as well:
--
-- @
--     import Hakyll
--     import Hakyll.Images        ( loadImage
--                                 , compressJpgCompiler
--                                 , scaleImageCompiler
--                                 )
--
--     hakyll $ do
--
--         -- Resize all JPEgs to fit inside of 800x600
--         -- Also compress to a quality of 25/100
--         match "pictures/**.jpg" $ do
--             route idRoute
--             compile $ loadImage
--                 >>= scaleImageCompiler 800 600
--                 >>= compressJpgCompiler 25
--
--         (... omitted ...)
-- @
module Hakyll.Images
  ( -- Basic types and functions
    Image,
    loadImage,
    -- Handling metadata
    module Hakyll.Images.Metadata,
    -- Jpg compression
    JpgQuality,
    compressJpgCompiler,
    -- Webp compression
    WebpQuality,
    compressWebpCompiler,
    -- Image scaling
    Width,
    Height,
    resize,
    resizeImageCompiler,
    scale,
    scaleImageCompiler,
    ensureFit,
    ensureFitCompiler,
  )
where

import Hakyll.Images.Common
import Hakyll.Images.CompressJpg
import Hakyll.Images.CompressWebp
import Hakyll.Images.Metadata
import Hakyll.Images.Resize
