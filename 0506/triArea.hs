triArea :: Double -> Double -> Double
triArea a b = a * b / 2.0
   

main = do
   putStrLn "The base?"
   inpBase <- getLine
   putStrLn "The height?"
   inpHeight <- getLine
   let outArea = triArea (read inpBase :: Double) (read inpHeight :: Double)
   putStrLn "The area of that triangle is "
   print(outArea)