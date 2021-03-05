unfold0 final h t x | final x       = []
                    | otherwise = h x : unfold0 final h t (t x)

int2bin = unfold0 (== 0) (`mod` 2) (`div` 2)




--

chop8 = unfold0 null (take 8) (drop 8)


map0 f = unfold0 null (f . head) tail


iterate0 f = unfold0 (const False) id f


-- *Main> take 10 (iterate (2*) 1)
-- [1,2,4,8,16,32,64,128,256,512]
