module Main where

import Data.List.Split (chunksOf) -- z biblioteki `split` potrzebujemy funkcji `chunksOf`, która dzieli listę na części po n elementów
import STL                        -- app/STL.hs

data Vector   = V Double Double Double deriving (Eq, Show) -- trzy koordynaty x y z
data Line     = L Vector Vector        deriving (Eq, Show) -- punkt p_l i wektor n_l równoległy do linii
data Plane    = P Vector Vector        deriving (Eq, Show) -- punkt p_n i wektor n_p prostopadły do płaszczyzny
data Triangle = T Vector Vector Vector deriving (Eq, Show) -- trzy wierzchołki a b c

-- dodawanie wektorów
(|+|) :: Vector -> Vector -> Vector
(|+|) (V a b c) (V d e f) = V (a + d) (b + e) (c + f)

-- odejmowanie wektorów
(|-|) :: Vector -> Vector -> Vector
(|-|) (V a b c) (V d e f) = V (a - d) (b - e) (c - f)

-- mnożenie przez skalar
(|*) :: Vector -> Double -> Vector
(|*) (V a b c) k = V (a * k) (b * k) (c * k)

-- dzielenie przez skalar
(|/) :: Vector -> Double -> Vector
(|/) (V a b c) k = V (a / k) (b / k) (c / k)

-- wektor przeciwny
neg :: Vector -> Vector
neg (V a b c) = V (-a) (-b) (-c)

-- iloczyn skalarny wektorów
dot :: Vector -> Vector -> Double
dot (V a b c) (V d e f) = a * d + b * e + c * f

-- iloczyn wektorowy
cross :: Vector -> Vector -> Vector
cross (V a b c) (V d e f) = V (b * f - c * e) (c * d - a * f) (a * e - b * d)

-- długość wektora
magnitude :: Vector -> Double
magnitude a = sqrt $ dot a a

-- wektor znormalizowany (długości 1)
normalize :: Vector -> Vector
normalize a = a |/ magnitude a

-- funkcja pomocnicza sprawczająca czy punkty p i q są "po tej samej stronie" boku ab
sameSide :: Vector -> Vector -> Vector -> Vector -> Bool
sameSide a b p q = dot (cross ab ap) (cross ab aq) > 0
  where ab = b |-| a
        ap = p |-| a
        aq = q |-| a

-- przecięcie płaszczyzny przez promień (półprostą)
planeIntersection :: Line -> Plane -> Maybe Vector
planeIntersection (L p_l n_l) (P p_p n_p)
             | s == 0    = Nothing                   -- płaszczyzna równoległa do półprostej
             | t <= 0    = Nothing                   -- punkt nie leży na półprostej
             | otherwise = Just $ p_l |+| (n_l |* t) -- półprosta przecina płaszczyznę
             where s = dot n_l n_p
                   t = dot (p_p |-| p_l) n_p / s

-- przecięcie trójkąta przez promień (półprostą)
triangleIntersection :: Line -> Triangle -> Maybe Vector
triangleIntersection (L p_l n_l) (T a b c) = case intersection of
                                             Nothing -> Nothing           -- półprosta nie przecina płaszczyzny, na której jest trójkąt
                                             Just p  -> if sameSide a b c p && sameSide b c a p && sameSide c a b p
                                                        then intersection -- punkt leży wewnątrz trójkąta
                                                        else Nothing      -- punkt leży na zewnątrz trójkąta
  where plane        = P a (cross (b |-| a) (c |-| a))     -- płaszczyzna, na tórej leży trójkąt
        intersection = planeIntersection (L p_l n_l) plane -- przecięcie promienia i w/w płaszczyzny

-- konwersja struktury Vertex (z modułu STL) do struktury Vector
toVector :: Vertex -> Vector
toVector (Vertex a b c) = V (_n a) (_n b) (_n c)

-- konwersja struktury Facet (z modułu STL) do struktury Triangle
toTriangle :: Facet -> Triangle
toTriangle (Facet _ (a, b, c)) = T (toVector a) (toVector b) (toVector c)

-- lista trójkątów ze struktury STL
toTriangleList :: STL -> [Triangle]
toTriangleList (STL _ facets) = map toTriangle facets

data Camera = C { position     :: Vector -- pozycja obserwatora
                , orientation  :: Vector -- kierunek "w górę" dla obserwatora
                , direction    :: Vector -- kierunek patrzeia kamery
                , screenWidth  :: Int    -- szerokość ekranu w pikselach
                , screenHeight :: Int    -- wysokość ekranu w pikselach
                } deriving Show

-- punkty ekranu w przestrzeni 3D
screen :: Camera -> [Vector]
screen (C p o d w h) = map f coords
  where middle   = p |+| d                  -- punkt środkowy ekranu
        left     = normalize $ cross d o    -- wektor jednostkowy - kierunek "w lewo" dla ekranu
        up       = normalize $ cross d left -- wektor jednostkowy - kierunek "do góry" dla ekranu
        screenW  = 2                        -- szerokość ekranu w przestrzeni 3D (zakładamy, że ekran ma szerokość 2)
        step     = screenW / fromIntegral w -- odległość między pikselami
        screenH  = step * fromIntegral h    -- wysokość ekranu w przestrzeni 3D
        f (x, y) = middle |+| (left |* (1.0 - x)) |+| (up |* (screenH / 2.0 - y))
        coords   = [ (x, y) | y <- [0.0, step..screenH - step]
                            , x <- [0.0, step..2.0     - step] ]

-- promienie z pozycji obserwatora przechodzące przez każdy punkr ekranu
rays :: Camera -> [Line]
rays camera = map lineTo $ screen camera
  where lineTo target = L (position camera) (target |-| position camera)

-- funkcja zwracająca "jakieś" trafienia promieni w trójkąty
hits :: Camera -> [Triangle] -> [Maybe Vector]
hits camera facets = map (maybeHit facets) (rays camera)
  where maybeHit []     _   = Nothing                            -- gdy lista trójkątów jest pusta - nie ma trafienia
        maybeHit [t]    ray = triangleIntersection ray t         -- gdy jest jeden trójkąt - wynik trafienia w ten trójkąt
        maybeHit (t:ts) ray = case triangleIntersection ray t of -- gdy jest więcej trójkątów, rzucamy promień w pierwszy
                              Nothing -> maybeHit ts ray         --   gdy nie trafimy, rzucamy dalej
                              Just p  -> Just p                  --   gdy trafimy, zwracamy wynik trafienia

-- renderuj listę trafień do formatu PBM
render :: Camera -> [Triangle] -> String
render camera facets = unlines $                  -- posklejaj linijki:
                         [ "P1"                   -- znacznik formatu
                         , width ++ " " ++ height -- szerokość i wysokość oddzielone spacją
                         ] ++ chunksOf 70 pixels  -- 70 znakowa linijka z wyrenderowanymi pikselami
  where width     = show $ screenWidth camera
        height    = show $ screenHeight camera
        pixels    = concat $ map toPixel (hits camera facets)
        toPixel a = case a of
                    Nothing -> "1 " -- piksel jest czarny, gdy nie trafiliśmy
                    Just _  -> "0 " -- a biały jak trafiliśmy

foo :: String -> String
foo stlString = case result of
                Left _    -> ""                                 -- jak się nie uda sparsować nic nie wyplujemy
                Right stl -> render camera $ toTriangleList stl -- jak się uda sparsować STL, renderujemy dziada!
  where result = parseSTLString stlString       -- parsuj STL
        camPos = V (-30) (-40) 90               -- pozycja obserwatora
        camDir = normalize . neg $ camPos       -- kierunek patrzenia (patrzymy na początek układu wsółrzędnych, długość wektora to 1)
        camOri = V 0 (-1) 0                     -- orientacja obserwatora (góra jest w kierunku malejącym osi Y)
        camera = C camPos camOri camDir 640 480 -- rozdzielczość obrazu to 640x480

main :: IO ()
main = interact foo
