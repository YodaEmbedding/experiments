slice s n = take (length s) $ drop n $ cycle s
cyclicPerms s = map (slice s) [1..(length s)]

main :: IO ()
main = do
  print $ cyclicPerms "abcde"
