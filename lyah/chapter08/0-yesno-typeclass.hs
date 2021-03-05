class YesNo a where
   yesno :: a -> Bool

instance YesNo Int where
   yesno 0 = False
   yesno _ = True

instance YesNo [a] where
   yesno [] = False
   yesno _  = True

instance YesNo Bool where
   yesno = id

instance YesNo (Maybe a) where
   yesno (Just _) = True
   yesno Nothing  = False

data TrafficLight = Red | Yellow | Green

instance YesNo TrafficLight where
   yesno Red = False
   yesno _   = True

-- *Main> :t yesno
-- yesno :: YesNo a => a -> Bool


-- *Main> yesno $ length []
-- False
-- *Main> yesno "haha"
-- True
-- *Main> yesno ""
-- False
-- *Main> yesno $ Just 0
-- True
-- *Main> yesno True
-- True
-- *Main> yesno []
-- False
-- *Main> yesno [0,0,0]
-- True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf val onYes onNo = if yesno val then onYes else onNo

-- *Main> yesnoIf [] "Yes" "No"
-- "No"
-- *Main> yesnoIf [0] "Yes" "No"
-- "Yes"



