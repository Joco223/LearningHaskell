import Data.List

double_me x = x + x
factorial :: Integer -> Integer
factorial n = product [1..n]

say_me :: (Integral a) => a -> String
say_me 1 = "One!"
say_me 2 = "Two!"
say_me 3 = "Three!"
say_me x = "Not between 1 and 3"

head' :: [a] -> a
head' [] = error "Can't get head on an empty list"
head' (_:x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmi_tell :: (RealFloat a) => a -> String
bmi_tell bmi
	| bmi <= 18.5 = "Underweigth"
	| bmi <= 25.0 = "Normal"
	| bmi <= 30.0 = "Overweight"
	| otherwise = "Obese"

fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-1) + fibonacci(x-2)

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

sum_list' :: (Num a) => [a] -> [a] -> [a]
sum_list' _ [] = []
sum_list' [] _ = []
sum_list' (x:xs) (y:ys) = (x+y):sum_list' xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smaller = quicksort [a | a <- xs, a <= x]
        bigger = quicksort [a | a <- xs, a > x]
    in smaller ++ [x] ++ bigger

zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
zip_with _ [] _ = []
zip_with _ _ [] = []
zip_with f (x:xs) (y:ys) = f x y : zip_with f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
   | even n = n:chain(n `div` 2)
   | odd n = n:chain(n*3 + 1)

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

split_string :: (Ord a) => a -> (a, a)
split_string x = span (/=' ') x
