repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x : xs) n = replicate n x ++ repli xs n

{-
(**) Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs
or, in Pointfree style:

repli = flip $ concatMap . replicate
alternatively, without using the replicate function:
repli :: [a] -> Int -> [a]
repli xs n = concatMap (take n . repeat) xs
or, using the list monad:

repli :: [a] -> Int -> [a]
repli xs n = xs >>= replicate n
or, a more verbose solution without the use of replicate:
repli :: [a] -> Int -> [a]
repli xs n = foldl (\acc e -> acc ++ repli' e n) [] xs
    where
      repli' _ 0 = []
      repli' x n = x : repli' x (n-1)
or, a version that does not use list concatenation:

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = foldr (const (x:)) (repli xs n) [1..n]
-}
