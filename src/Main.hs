module Main where

main = do
	putStrLn ""
	putStrLn $ show $ takeN 3 $ numFrom 200
	putStrLn ""


numFrom n = n : (numFrom $ n + 1)

takeN 0 _        = []
takeN _ []       = []
takeN n (x:xs)   = x : take (n-1) xs