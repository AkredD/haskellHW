--Анисимов Алексей Васильевич A3201
--
--
-- Задание: 
-- Написать функцию bitOnes :: Integer -> Int, которая выдает 
-- количество единиц в битовом представлении заданного 
-- натурального числа. Например, число 36 имеет битовое представление 
-- 100100, поэтому bitOnes 36 => 2.
--
--
--Для нахождения кол-во единиц в двоичном представлении числа, я сначала перевожу десятичное число
--в двоичную систему счисления(getOnes). Для этого постоянно делю исходное число на 2 и заполняю
--список(list), добавляя то, что получается в остатке
-- number `mod` (2) == 0 = getOnes (0:list) (number `div` 2)
-- otherwise = getOnes (1:list) (number `div` 2)
-- После того, как number становится равным 0, возвращаю сумму всего листа(sum list).


module Homework where
main :: [Bool] 
main = [
	bitOnes 9 == 2,
	bitOnes 9 == 2,
    bitOnes 1 == 1,
    bitOnes 2 == 2,
    bitOnes 0 == 0,
	bitOnes 32 == 1,
	bitOnes 31 == 5,
	bitOnes 31 == 6,
	bitOnes 36 == 2]
				
bitOnes :: Integer -> Int
getOnes :: [Integer] -> Integer -> Int
bitOnes number =  getOnes [] number
getOnes list number | number == 0 = fromIntegral (sum list)
					| number `mod` (2) == 0 = getOnes (0:list) (number `div` 2)
					| otherwise = getOnes (1:list) (number `div` 2)