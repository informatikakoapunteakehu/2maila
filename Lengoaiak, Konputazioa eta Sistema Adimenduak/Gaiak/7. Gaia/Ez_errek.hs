module Ez_errek where

-- Sarrerako daturik hartzen ez duen eta beti 3.1415 balioa itzultzen duen funtzioa
pi2:: Float
pi2 = 3.1415

-- Sarrerako datu bezala zenbaki oso bat hartu eta beti 100 balioa itzultzen duen funtzioa
f:: Int -> Int
f x = 100

-- Zenbaki oso bat emanda, zenbakia bikoitia al den ala ez erabakitzen duen funtzioa
bikoitia:: Int -> Bool
bikoitia x = (x `mod` 2) == 0

-- Zenbaki oso bat emanda, zenbakia bakoitia al den ala ez erabakitzen duen funtzioa
bakoitia:: Int -> Bool
bakoitia x = not (bikoitia(x))

-- Hiru zenbaki oso emanda, balio handiena itzultzen duen funtzioa
hand3:: Int -> Int -> Int -> Int
hand3 x y z
	| x >= y && x >= z		= x
	| y > x && y >= z		= y
	| otherwise			= z
