{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonyms #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.String
import GHC.Records
import GHC.TypeLits

data Lisp =
  Cons LispObject LispObject |
  Nil |
  String String |
  Symbol Symbol |
  Number Number |
  Vector [Lisp]

instance Lispable Lisp where
  toLisp = id

data Number =
  Integer Integer |
  Double Double

instance Enum Number where
  fromEnum (Integer i) = fromEnum i
  fromEnum (Double d) = fromEnum d
  toEnum = Integer

newtype Symbol = Symbol String

class Lispable l where
  toLisp :: l → Lisp

  default toLisp :: (Generic l, GenericLispable (Rep l)) ⇒ l → Lisp
  toLisp = ...

class GenericLispable l where
  genericToLisp :: Proxy l → Lisp

type PropertizedChar = (Char, Properties)

data Properties = Properties {
  display :: [Display]
  -- incomplete
  }
  deriving (Eq, Generic)

instance Semigroup Properties where
  a <> b = Properties {
    display = a.display <> b.display
    }

instance Monoid Properties where
  mempty = Properties mempty

data Display =
  Replace PropertizedString |
  Space Space |
  Image Image

data Space = Space {
  width :: Maybe Width,
  height :: Maybe Dimension,
  ascent :: Maybe PixelSpecification
  }

data Dimension =
  Absolute PixelSpecification |
  Relative Number

data Width =
  Width Dimension |
  AlignTo PixelSpecification

data PixelSpecification =
  Character Int

instance Lispable Space where
  toLisp space =
    Cons (Symbol "space")
    $ toCons
    $ concat [
      concatMap widthForms space.width,
      concatMap heightForms space.height,
      concatMap (forms "ascent") space.ascent
    ]
    where
      widthForms (Width (Absolute spec)) = forms "width" spec
      widthForms (Width (Relative factor)) = forms "relative-width" factor
      widthForms (AlignTo spec) = forms "align-to" spec

      heightForms (Absolute spec) = forms "height" spec
      heightForms (Width (Relative factor)) = forms "relative-height" factor

      forms :: Lispable l ⇒ String → l → [Lisp]
      forms symbol form = [Symbol ":":symbol, toLisp form]

data Image = Image {
  type_ :: Type,
  source :: Source,
  margin :: Maybe (Number, Number),
  ascent ::,
  relief :: Maybe Number,
  width :: Maybe Number,
  height :: Maybe Number,
  maxWidth :: Maybe Number,
  maxHeight :: Maybe Number,
  scale :: Maybe Number,
  rotation :: Maybe Number,
  flip :: Bool,
  transformSmoothing :: Maybe Bool,
  index :: Maybe Int,
  conversion :: Maybe Conversion,
  mask :: Maybe Mask,
  pointer :: Maybe PointerShape,
  map_ :: [(Area, HotSpot)]
  }
  deriving Generic

instance HasField "type" Image Type where
  getField = type_

instance HasField "map" Image [(Area, HotSpot)] where
  getField = map_

data Type =
  PBM |
  XBM |
  XPM |
  GIF |
  JPEG |
  TIFF |
  PNG |
  SVG |
  WebP
  deriving Show

data Source =
  File String |
  Data String

data Conversion =
  Laplace |
  Emboss |
  EdgeDetection {
    matrix :: (Number, Number, Number, Number, Number, Number, Number, Number, Number),
    colorAdjust :: Maybe Number
    } |
  Disabled

data Color = Color {
  red :: Number,
  green :: Number,
  blue :: Number
  }

data Mask =
  Remove |
  Heuristic (Maybe Color)

data PointerShape =
  Text |
  Default |
  Arrow |
  VDrag |
  Modeline
  Hand |
  HDrag |
  NHDrag |
  Hourglass

data Point = Point {
  x :: Number,
  y :: Number
  }

instance Lispable Point where
  toLisp (Point x y) = Cons x y

data Area =
  Rectangle {
    upperLeft :: Point,
    bottomRight :: Point
    } |
  Circle {
    center :: Point,
    radius :: Number
    } |
  Polygon [Point]

instance Lispable Area where
  toLisp (Rectangle upperLeft bottomRight) =
    Cons (Symbol "rect") (Cons (toLisp upperLeft) (toLisp bottomRight))
  toLisp (Rect center radius) =
    Cons (Symbol "circle") (Cons (toLisp center) radius)
  toLisp (Polygon points) =
    Cons (Symbol "poly") $ Vector $ points >>= \(Point x y) → [x, y]

data HotSpot = HotSpot {
  id_ :: Symbol,
  helpEcho :: Maybe String,
  pointer :: Maybe PointerShape
  }

type PropertizedString = [PropertizedChar]

class IsString PropertizedString where
  fromString = map (, mempty)

class Show s ⇒ Propertized s where
  propertized :: s → PropertizedString
  propertized = fromString . show

instance Show PropertizedString where
  show (unzip → (raw, allProperties)) =
    ('#':)
    $ show
    $ toCons
    $ String raw:
      concat [[head i, last i, the properties]
        | i ← toLisp <$> [0..]
        | properties ← toLisp <$> allProperties,
        then group by properties using group]

toCons :: [Lisp] → Lisp
toCons [] = Nil
toCons head:tail = Cons head $ toCons tail

propertize :: Propertized s ⇒ s → Properties → PropertizedString
propertize s properties = fmap (<> properties) <$> (propertized s)
