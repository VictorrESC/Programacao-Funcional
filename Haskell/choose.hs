choose :: Int -> String
choose 0 = "zero"
choose n = if even n then "Par" else "Impar"

fat :: Integral a => a -> a
fat 0 = 1
fat 1 = 1
fat n = n * fat (n - 1)