module Errek_zenb where

import Ez_errek

--Oinarrizko errekurtsibitatea zenbakiekin

--x balioa y aldiz batuz x * y kalkulatzen duen funtzioa. 
--Kasu berezi bezala, y balioa negatiboa baldin bada, errore-mezua aurkeztuko du.

bider:: Int -> Int -> Int

bider x y
   | y < 0 		= error "Bigarren balioa negatiboa da."
   | x == 0 || y == 0	= 0
   | x == 1		= y
   | otherwise		= x + bider x (y - 1)


--x * y balioa batuketaren bidez kalkulatzen duen funtzioa. 
--y balioa negatiboa denean ere kalkulua ondo egiten da.
--Aurretik definitu den "bider" funtzioa erabiltzen da laguntzaile bezala.
--"bneg" funtzioa berez ez da errekurtsiboa, ez baitio bere buruari deitzen.
--Errekurtsibitatea "bider" funtzioan dago.

bneg:: Int -> Int -> Int

bneg x y
   | x == 0 || y == 0	= 0
   | (x < 0) && (y < 0)		= bider (-x) (-y)
   | (x > 0) && (y > 0)		= bider x y
   | (x < 0) && (y > 0)		= bider x y
   | (x > 0) && (y < 0)		= bider (-x) (-y)


--Errusiar biderkaketa bezala ezagutzen den metodoa jarraituz x * y 
--kalkulatzen duen funtzioa. 
--Kasu berezi bezala, y balioa negatiboa baldin bada, errore-mezua aurkeztuko du.

errusiar:: Int -> Int -> Int

errusiar x 0 = 0

errusiar x y
   | y < 0 		= error "Bigarren balioa negatiboa da."
   | bikoitia y		= errusiar (x + x) (y `div` 2)
   | otherwise		= x + errusiar (x + x) (y `div` 2)


-- y balioa x balioari zenbat aldiz kendu dakiokeen zenbatuz, x eta y-ren arteko 
-- zatidura osoa kalkulatzen duen funtzioa.
-- Kasu berezi bezala, zatitzailea zero denean, errore-mezua aurkeztuko da. 
-- Gainera x edo y negatiboa baldin bada ere errore-mezua aurkeztuko da.

zatios:: Int -> Int -> Int

zatios x y
   | y == 0		= error "Zatitzailea 0"
   | (x < 0) || (y < 0)	= error "Gutxienez bietako bat negatiboa da."
   | x < y		= 0
   | otherwise		= 1 + zatios (x - y) y

-- y balioa baino txikiagoa den balio bat eduki arte y balioa x balioari eta horrela 
--lortuz joango diren balioei kenduz, x eta y-ren arteko 
-- zatidura osoaren hondarra kalkulatzen duen funtzioa.
-- Kasu berezi bezala, zatitzailea zero denean, errore-mezua aurkeztuko da. 
-- Gainera x edo y negatiboa baldin bada ere errore-mezua aurkeztuko da.


zatihond:: Int -> Int -> Int

zatihond x y
   | y == 0		= error "Zatitzailea 0"
   | (x < 0) || (y < 0)	= error "Gutxienez bietako bat negatiboa da."
   | x < y		= x
   | otherwise		= zatihond (x - y) y