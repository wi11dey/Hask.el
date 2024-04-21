{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

import Data.String
import GHC.Exts
import GHC.Records
import GHC.TypeLits

data Lisp =
  Cons LispObject LispObject |
  Nil |
  Vector [Lisp] |
  Symbol String |
  String String |
  Number Number

instance Show Lisp where
  showsPrec _ (Cons car cdr@(Cons _ _)) =
    ('(':) .
    (show car ++) .
    (' ':) .
    (show cdr ++) .
    (')':)
  showsPrec _ (Cons car cdr) =
    ('(':) .
    (show car ++) .
    (" . " ++) .
    (show cdr ++) .
    (')':)

  showsPrec _ Nil = ("nil" ++)

  showsPrec _ (Vector elements) = showList elements

  showsPrec _ (Symbol symbol) = (symbol ++) -- TODO use elisp escaping instead

  showsPrec _ (String s) = showList s -- TODO use elisp escaping instead

  showsPrec prec (Number n) = showsPrec prec n

data Number =
  Integer Integer |
  Double Double

instance Show Number where
  showsPrec prec (Integer i) = showsPrec prec i
  showsPrec prec (Double d) = showsPrec prec d

instance Enum Number where
  fromEnum (Integer i) = fromEnum i
  fromEnum (Double d) = fromEnum d
  toEnum = Integer

toCons :: [Lisp] → Lisp
toCons [] = Nil
toCons head:tail = Cons head $ toCons tail

class ToLisp l where
  toLisp :: l → Lisp

  default toLisp :: (Generic l, ToLisp (Rep l)) ⇒ l → Lisp
  toLisp = genericToLisp . from

instance ToLisp Lisp where
  toLisp = id

class Splice l where
  prefix :: l → ShowS

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
  Space {
    width :: Maybe Width,
    height :: Maybe Dimension,
    ascent :: Maybe PixelSpecification
    } |
  Image Image

data Dimension =
  Absolute PixelSpecification |
  Relative Number

instance Splice Dimension where
  prefix (Absolute _) = (':':)
  prefix (Relative _) = (":relative-" ++)

data Width =
  Width Dimension |
  AlignTo PixelSpecification

instance Splice Width =
  prefix (Width _) = id
  prefix (AlignTo _) = ":align-to"

data PixelSpecification =
  Characters Int

instance ToLisp PixelSpecification where
  toLisp (Characters c) = Number $ fromInt c

instance ToLisp Space where
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

      forms :: ToLisp l ⇒ String → l → [Lisp]
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

image type_ source = Image {
  type_,
  source,
  margin = Nothing,
  ascent = Nothing,
  relief = Nothing,
  width = Nothing,
  height = Nothing,
  maxWidth = Nothing,
  maxHeight = Nothing,
  scale = Nothing,
  rotation = Nothing,
  flip = False,
  transformSmoothing = False,
  index = Nothing,
  conversion = Nothing,
  mask = Nothing,
  pointer = Nothing,
  map_ = []
  }

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
  deriving (Show, ToLisp)

instance ToLisp Type where
  toLisp = Symbol . map toLower . show

data Source =
  File String |
  Data String

data Conversion =
  Laplace |
  Emboss |
  EdgeDetection {
    matrix :: (
      Number, Number, Number,
      Number, Number, Number,
      Number, Number, Number
      ),
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
  deriving (Show, Generic, ToLisp)

data Point = Point {
  x :: Number,
  y :: Number
  }

instance ToLisp Point where
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

instance ToLisp Area where
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

instance HasField "id" HotSpot Symbol where
  getField = id_

type PropertizedChar = (Char, Properties)
type PropertizedString = [PropertizedChar]

class IsString PropertizedString where
  fromString = map (, mempty)

class Propertized s where
  propertized :: s → PropertizedString

  default propertized :: Show s ⇒ s → PropertizedString
  propertized = fromString . show

instance {-# OVERLAPPABLE #-} Show s ⇒ Propertized s

instance {-# OVERLAPPING #-} Show PropertizedChar where
  showList (unzip → (raw, propertiesList)) =
    (++) $
    ('#':) $
    show $
    toCons $
    String raw:
      concat [[head i, last i, the properties]
        | (toLisp → i, toLisp → properties) ←
          zip [0..] propertiesList,
        then group by properties using groupBy . on (==)]

groupRuns f = groupBy ((==) `on` f)

propertize :: Propertized s ⇒ s → Properties → PropertizedString
propertize s properties = fmap (<> properties) <$> (propertized s)

toKebab :: String → String
toKebab "" = ""
toKebab "_" = ""
toKebab [letter] = [toLower letter]
toKebab (first:tail@(second:rest))
  | isLowerCase first && isUpperCase second =
      toLower first:'-':toLower second:toKebab rest
  | otherwise =
      toLower first:toKebab tail

propertize "asdf" $ Properties {
  display = [space { width = Just $ Width $ Relative 10 }]
  }
