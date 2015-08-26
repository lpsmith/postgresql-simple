{-|
Module      : Database.PostgreSQL.Simple.Geometry
Description : Geometry types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : Leon P Smith <leon@melding-monads.com>
Stability   : experimental
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Database.PostgreSQL.Simple.Geometry (

      Point
    , point
    , pointX
    , pointY

    ) where

data Point = Point {-# UNPACK #-} !Double {-# UNPACK #-} !Double

point :: Double -> Double -> Point
point = Point

pointX :: Point -> Double
pointX (Point x _) = x

pointY :: Point -> Double
pointY (Point _ y) = y
