module Errek_g where

--Errekurtsibitate gurutzatua

--Errekurtsibitate gurutzatuaren bidez, 0 baino handiagoa edo berdina 
--den zenbaki oso bat bikoitia ala bakoitia den erabakitzen duten funtzioak
bikoitiag:: Int -> Bool

bikoitiag x
   | x < 0		= error "Balio negatiboa"
   | x == 0		= True
   | otherwise		= bakoitiag (x - 1)

---
---
bakoitiag:: Int -> Bool

bakoitiag x
   | x < 0		= error "Balio negatiboa"
   | x == 0		= False
   | otherwise		= bikoitiag (x - 1)



--Errekurtsibitate gurutzatuaren bidez, posizio bikoitietako eta 
--posizio bakoitietako elementuak kentzen dituzten funtzioak
pos_bik_kendug:: [t] -> [t]

pos_bik_kendug [] = []
pos_bik_kendug (x:s) = x:(pos_bak_kendug s)


pos_bak_kendug:: [t] -> [t]

pos_bak_kendug [] = []
pos_bak_kendug (x:s) = pos_bik_kendug s