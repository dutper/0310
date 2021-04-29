--라인 주석
{-
	블럭 주석
-}

digits :: Int -> Int
digits = length.show

square :: Num a => a -> a
square = (^ 2)

lucky :: Integral a => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

ultimate :: Int -> String -- 함수 ultimate 를 정의한다
ultimate 42 = "FEEL THE UNIVERSE" -- 패턴 42 에 매치했을 경우
ultimate n = show n

maybeToList :: Maybe a -> [a]
maybeToList Nothing = [] -- 패턴 Nothing 에 매치한 경우
maybeToList (Just x) = x:[] -- or [x] 패턴 Just x 에 매치한 경우

listToMaybe :: [a] -> Maybe a
listToMaybe [ ] = Nothing -- 패턴 Nothing 에 매치한 경우
listToMaybe (a : as) = Just a -- 패턴 Just a 에 매치한 경우

deeping :: String -> String
deeping (' ':' ':xs)= " " ++ xs -- 선두 스페이스 2 문자 이상의 패턴
deeping (' ':xs ) = " " ++ xs -- 선두 스페이스 1 문자의 패턴
deeping xs = xs -- 그 외의 패턴

deepingBeta :: String -> String
deepingBeta s@(' ':' ':xs)= " " ++ s -- 선두 스페이스 2 문자 이상의 패턴
deepingBeta s@(' ':xs ) = " " ++ s -- 선두 스페이스 1 문자의 패턴
deepingBeta s = s -- 그 외의 패턴

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

deepingDelta :: String -> String
deepingDelta s@(' ':' ':_) = " " ++ s -- 선두 스페이스 2 문자 이상의 패턴
deepingDelta s@(' ':_) = " " -- 선두 스페이스 1 문자의 패턴
deepingDelta s = s -- 그 외의 패턴

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = ( fst a + fst b, snd a + snd b)

addVectorsBeta :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectorsBeta (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) -- 패턴 매칭

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
 |bmi <= 18.5 = "You're underweight, you emo, you!"
 |bmi <= 25.0 = "You're supposedly normal. Pfft , I bet you're ugly!"
 |bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
 |otherwise = "You're a whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
 |a > b = a
 |otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
 |a > b = GT
 |a == b = EQ
 |otherwise = LT

safeSqrt :: (Ord a, Floating a) => a -> Maybe a
safeSqrt x
 | x < 0 = Nothing
 | otherwise = Just (sqrt x)
-- 이 라인을 주석 처리하면 망라하지 않은 조건이다.
-- otherwise 는 항상 참이 되는 조건이다.

caseOfFirstLetter :: String -> String
caseOfFirstLetter "" = "empty" -- 빈 문자열에 패턴 매치
caseOfFirstLetter (x:xs)
 | 'a' <= x && x <= 'z' = "lower"
 | 'A' <= x && x <= 'Z' = "upper"
 | otherwise = "other"

caseOfFirstLetterBeta :: String -> String
caseOfFirstLetterBeta str =
 case str of
 "" -> "" -- 빈 문자열에 패턴 매치
 (x:xs) -> if 'a' <= x && x <= 'z'
 then "lower"
 else if 'A' <= x && x <= 'Z'
 then "upper"
 else "other"

caseOfFirstLetterDelta :: String -> String
caseOfFirstLetterDelta str =
 case str of
 "" -> "" -- 빈 문자열에 패턴 매치
 (x:xs) | 'a' <= x && x <= 'z' -> "lower"
        | 'A' <= x && x <= 'Z' -> "upper"
        | otherwise -> "other"

cylinder :: Double -> Double -> Double
cylinder r h =
 let sideArea = 2 * pi * r * h
     topArea = pi * r ^ 2
     in sideArea + 2 * topArea

bmiTellBeta :: (RealFloat a) => a -> a -> String
bmiTellBeta weight height
 |weight / (height ^2) <= 0.00185 = "You're underweight, you emo, you!"
 |weight / (height ^2) <= 0.0025 = "You're supposedly normal. Pfft, I bet you're ugly!"
 |weight / (height ^2) <= 0.003 = "You're fat! Lose some weight, fatty!"
 |otherwise = "You're a whale, congratulations!"

bmiTellDelta :: (RealFloat a) => a -> a -> String
bmiTellDelta weight height
 |bmi <= 0.00185 = "You're underweight, you emo, you!"
 |bmi <= 0.0025 = "You're supposedly normal. Pfft, I bet you're ugly!"
 |bmi <= 0.003 = "You're fat! Lose some weight, fatty!"
 |otherwise = "You're a whale, congratulations!"
 where bmi = weight / (height ^2)

-- 이름과 성을 받아서 이니셜을 반환하는 함수
initials :: String -> String -> String
initials firstname lastname = [f] ++ " . " ++ [l] ++ "."
 where (f:fs) = firstname
       (l:ls) = lastname

