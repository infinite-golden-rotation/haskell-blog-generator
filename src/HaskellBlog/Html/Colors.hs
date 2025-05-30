module HaskellBlog.Html.Colors where

import Data.Word (Word8) -- u8 int type

data Color
  = RGB Word8 Word8 Word8

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data ANSIColor
  = ANSIColor Brightness EightColor

ansiColorToVGA :: ANSIColor -> Color
ansiColorToVGA (ANSIColor br c) = brighten br (colorBase c)

isBright :: ANSIColor -> Bool
isBright (ANSIColor b _) =
  case b of
    Bright -> True
    _ -> False

brighten :: Brightness -> Color -> Color
brighten lum (RGB r g b) =
  case lum of
    Bright -> RGB (r + 85) (g + 85) (b + 85)
    Dark -> RGB r g b

colorBase :: EightColor -> Color
colorBase c =
  case c of
    Black -> RGB 0 0 0
    Red -> RGB 170 0 0
    Green -> RGB 0 170 0
    Yellow -> RGB 170 85 0
    Blue -> RGB 0 0 170
    Magenta -> RGB 170 0 170
    Cyan -> RGB 0 170 170
    White -> RGB 170 170 170

-- another appproach is to case (c, b) pattern match and just list all of them out in one level
ansiColorToUbuntu :: ANSIColor -> Color
ansiColorToUbuntu col =
  case col of
    ANSIColor brightness color ->
      case brightness of
        Dark ->
          case color of
            Black -> RGB 1 1 1
            Red -> RGB 22 56 43
            Green -> RGB 57 181 74
            Yellow -> RGB 255 199 6
            Blue -> RGB 0 111 184
            Magenta -> RGB 118 38 113
            Cyan -> RGB 44 181 233
            White -> RGB 204 204 204
        Bright ->
          case color of
            Black -> RGB 128 128 128
            Red -> RGB 255 0 0
            Green -> RGB 0 255 0
            Yellow -> RGB 255 255 0
            Blue -> RGB 0 0 255
            Magenta -> RGB 255 0 255
            Cyan -> RGB 0 255 255
            White -> RGB 255 255 255
