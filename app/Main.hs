module Main where

import Data.List.Split (chunksOf)
import STL

data Vector   = V Double Double Double deriving (Eq, Show)
data Line     = L Vector Vector        deriving (Eq, Show)
data Plane    = P Vector Vector        deriving (Eq, Show)
data Triangle = T Vector Vector Vector deriving (Eq, Show)

(|+|) :: Vector -> Vector -> Vector
(|+|) (V a b c) (V d e f) = V (a + d) (b + e) (c + f)

(|-|) :: Vector -> Vector -> Vector
(|-|) (V a b c) (V d e f) = V (a - d) (b - e) (c - f)

(|*) :: Vector -> Double -> Vector
(|*) (V a b c) k = V (a * k) (b * k) (c * k)

(*|) :: Double -> Vector -> Vector
(*|) k (V a b c) = V (a * k) (b * k) (c * k)

(|/) :: Vector -> Double -> Vector
(|/) (V a b c) k = V (a / k) (b / k) (c / k)

neg :: Vector -> Vector
neg (V a b c) = V (-a) (-b) (-c)

dot :: Vector -> Vector -> Double
dot (V a b c) (V d e f) = a * d + b * e + c * f

cross :: Vector -> Vector -> Vector
cross (V a b c) (V d e f) = V (b * f - c * e) (c * d - a * f) (a * e - b * d)

magnitude :: Vector -> Double
magnitude a = sqrt $ dot a a

normalize :: Vector -> Vector
normalize a = a |/ magnitude a

sameSide :: Vector -> Vector -> Vector -> Vector -> Bool
sameSide a b c p = dot (cross ab ap) (cross ab ac) > 0
  where ab = b |-| a
        ac = c |-| a
        ap = p |-| a

planeIntersection :: Line -> Plane -> Maybe Vector
planeIntersection (L p_l n_l) (P p_p n_p)
             | s == 0    = Nothing
             | t > 0     = Just $ p_l |+| (n_l |* t)
             | otherwise = Nothing
             where s = dot n_l n_p
                   t = dot (p_p |-| p_l) n_p / s

triangleIntersection :: Line -> Triangle -> Maybe Vector
triangleIntersection (L p_l n_l) (T a b c) = case intersection of
                                             Nothing -> Nothing
                                             Just p  -> if sameSide a b c p && sameSide b c a p && sameSide c a b p
                                                        then intersection
                                                        else Nothing
  where plane        = P a (cross (b |-| a) (c |-| a))
        intersection = planeIntersection (L p_l n_l) plane

toVector :: Vertex -> Vector
toVector (Vertex a b c) = V (_n a) (_n b) (_n c)

toTriangle :: Facet -> Triangle
toTriangle (Facet _ (a, b, c)) = T (toVector a) (toVector b) (toVector c)

toTriangleList :: STL -> [Triangle]
toTriangleList (STL _ facets) = map toTriangle facets

data Camera = C { position     :: Vector
                , orientation  :: Vector
                , direction    :: Vector
                , screenWidth  :: Int
                , screenHeight :: Int
                } deriving Show

screen :: Camera -> [Vector]
screen (C p o d w h) = map (|+| middle) coords
  where middle   = p |+| d
        left     = normalize $ cross d o
        up       = normalize $ cross d left
        step     = 2.0 / fromIntegral w
        hX       = step * fromIntegral h
        coords   = [left |* (1.0 - x) |+| (up |* (hX / 2.0 - y)) | y <- [0.0, step..hX - step]
                                                                 , x <- [0.0, step..2.0 - step]]

rays :: Camera -> [Line]
rays camera = map lineTo $ screen camera
  where lineTo target = L (position camera) (target |-| position camera)

hits :: Camera -> [Triangle] -> [Maybe Vector]
hits camera facets = map (maybeHit facets) (rays camera)
  where maybeHit []     _   = Nothing
        maybeHit [t]    ray = triangleIntersection ray t
        maybeHit (t:ts) ray = case triangleIntersection ray t of
                              Nothing -> maybeHit ts ray
                              Just p  -> Just p

render :: Camera -> [Triangle] -> String
render camera facets = unlines $ ["P1", width ++ " " ++ height] ++ chunksOf 70 pixels
  where width     = show $ screenWidth camera
        height    = show $ screenHeight camera
        pixels    = concat $ map toPixel (hits camera facets)
        toPixel a = case a of
                    Nothing -> "1 "
                    Just _  -> "0 "

foo :: String -> String
foo stlFile = case result of
              Left _    -> ""
              Right stl -> render camera $ toTriangleList stl
  where result = parseSTLString stlFile
        camPos = V (-30) (-40) 90
        camera = C camPos (V 0 (-1) 0) (normalize . neg $ camPos) 640 480

main :: IO ()
main = interact foo
