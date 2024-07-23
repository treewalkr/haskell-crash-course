module HW01 where

-- Define a data type Shape
data Shape
  = Circle Double
  | Square Double
  | CircleInCircle Double Double
  | CircleWithIncribeSquare Double
  | IncursiveSquareCircle Double
  deriving (Show)

-- Function to calculate the area of a shape
areaOf :: Shape -> Double
areaOf (Circle r) = areaOfCircle r
areaOf (Square s) = areaOfSquare s
areaOf (CircleInCircle r1 r2) = areaDiff (Circle r1) (Circle r2)
areaOf (CircleWithIncribeSquare d) = areaDiff (Circle (0.5 * d)) (Square (sideOfInscribedSquare d))
areaOf (IncursiveSquareCircle d) = areaOfIncursiveCirclesIncribedSquares d

-- Function to calculate the difference between the areas of two shapes
areaDiff :: Shape -> Shape -> Double
areaDiff s1 s2 = areaOf s1 - areaOf s2

-- Function to calculate the side of inscribed square given the diameter of the circle
sideOfInscribedSquare :: Double -> Double
sideOfInscribedSquare d = 0.5 * d * sqrt 2

-- 1. Function to calculate the area of circle from a given radius
areaOfCircle :: Double -> Double
areaOfCircle r = pi * r * r

-- 2. Function to calculate the area of square from a given side
areaOfSquare :: Double -> Double
areaOfSquare s = s * s

-- 3. Function to calculate the aeare diff of two circles
areaDiffOfTwoCircles :: Double -> Double -> Double
areaDiffOfTwoCircles r1 r2 = areaOf (CircleInCircle r1 r2)

-- 4. Function to calculate the aeare diff between a circle and its inscribed square given the diameter of the circle d
areaDiffOfCircleWithIncribeSquare :: Double -> Double
areaDiffOfCircleWithIncribeSquare d = areaOf (CircleWithIncribeSquare d)

-- 5. Function to recursively calculate the sum of area differences of recursive structure of squares incribed in circles given the side of the first square (diameter of the first circle)
areaOfIncursiveCirclesIncribedSquares :: Double -> Double
areaOfIncursiveCirclesIncribedSquares side
  | side <= 0.0001 = 0 -- base case
  | otherwise = areaDiffOfCircleWithIncribeSquare side + areaOfIncursiveCirclesIncribedSquares (sideOfInscribedSquare side)

-- Example: areaOfIncursiveCirclesIncribedSquares 1.0  -- Output: 0.5707963225421324

-- 6. General function to calculate the areas of a list of shapes given their defining dimension and shape constructor
areasOfShapes :: (Double -> Shape) -> [Double] -> [Double]
areasOfShapes shapeConstructor = map (areaOf . shapeConstructor)

-- Example: areasOfShapes IncursiveSquareCircle [1.0, 2.0, 3.0]  -- Output: [0.5707963225421324,2.283185302926822,5.1371669363697094]
