import System.IO

nameDo :: IO ()
nameDo = do
  putStr "What is your first name? "
  first <- getLine
  putStr "And your last name? "
  last <- getLine
  let full = first ++ " " ++ last
  putStrLn ("Pleased to meet you, " ++ full ++ "!")


main = do
hSetBuffering stdout NoBuffering
nameDo
