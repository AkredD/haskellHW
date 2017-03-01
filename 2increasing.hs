module Homework where
--Анисимов Алексей Васильевич A3201
--Вариант 1
--
--
--	Написать функцию increasing :: Ord a => [a] -> [a],
-- которая в списке значений находит неубывающий отрезок максимальной длины.
-- Например, в списке [1,3,4,3,6,7,3] таким отрезком может быть [1,3,4] или [3,6,7]
-- (можно выдать любой из этих двух), а в списке [5,4,2,1] результатом может быть любой из
-- одноэлементных списков [5], [4], [2] или [1].
--
--Для решения данной я использую 2 различных списка, а так же их длины. Answer - максимальный подсписок на
--данный момент(lengA соответственно его длина), а так же local - подсписок, с которым мы работаем в данный
--момент. Как только наша неубывающая подпоследовательность заканчивается - эти два списка сравниваются по длине.
--И если local по длине оказался больше answer, то заменяю answer --
-- lengL > lengA = findMaxSubList xs local lengL [x] 1
-- otherwise = findMaxSubList xs answer lengA [x] 1
--Когда мы полностью прошли наш изначальный список, возвращаю подпоследовательность, сравнивая текущую и максимальную
--заодно переворачивая список.
--lengL > lengA = reverse local
--otherwise = reverse answer
--
--UPD хотел не заводить 2 лишние переменные под длины, но не получилось реализовать конструкцию
--answer@(length:aa@(x:xs)). Не знаю, можно ли так делать
main :: [[Integer]]
main = [
		increasing [6,7,6,7,8],
		increasing [1,2,3,4,5],
		increasing [5,4,3,2,1],
		increasing [1,2,3,1,2,3,4,0,1,2,3,4,5],
		increasing [1,2,3,4,5,1,2,3,1,2,3,4]]

increasing :: Ord a => [a] -> [a]
findMaxSubList :: Ord a => [a] -> [a] -> Integer -> [a] -> Integer -> [a]
increasing defaultList@(x:xs) = findMaxSubList xs [] 0 [x] 1
findMaxSubList list@(x:xs) answer lengA local@(y:ys) lengL  | x > y = findMaxSubList xs answer lengA (x:local) (lengL + 1)
															| lengL > lengA = findMaxSubList xs local lengL [x] 1
															| otherwise = findMaxSubList xs answer lengA [x] 1
findMaxSubList [] answer lengA local lengL  | lengL > lengA = reverse local
											| otherwise = reverse answer


--В заданной строке символов будем считать числом произвольную последовательность цифр,
-- слева и справа от которой не находится цифра. Написать функцию sumNumbers :: String -> Integer
--, которая вычисляет сумму всех “чисел” в заданной строке. Например, для аргумента "0012 3xaxa-1000"
-- результатом должно быть число 1015 (12 + 3 + 1000)
--
--Пробегаюсь по строке S и смотрю каждый символ, является ли он цифрой(((x >= '0') && (x<='9'))
--если же да, то добавляю в локальную строку growing(которая аккумулирует текущее число)
--если же нет, то перевожу строку growing в тип Integer и добавляю к итоговому ответу sum
--после того, как пробежал по всей строке - добавляю growing и вывожу ответ
main' :: [Integer]
main' = [
		sumNumbers "1234",
		sumNumbers "0001a1",
		sumNumbers "1k2k3k5",
		sumNumbers "0000dasklf0000"]
sumNumbers :: String -> Integer
getSum :: String -> String -> Integer -> Integer
sumNumbers s = getSum s "0" 0
getSum s@(x:xs) growing sum | ((x >= '0') && (x<='9')) = getSum xs (growing ++ (x:"")) sum
							| otherwise = getSum xs "0" (sum + (read growing :: Integer))
getSum "" growing sum = sum + (read growing :: Integer)
