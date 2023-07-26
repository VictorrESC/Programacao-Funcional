crop :: [a] -> [a]
crop x = drop 1 (take (length x - 1) x)
