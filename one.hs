import All

testing st fcall = do
  putStrLn $ "Testing " ++ st ++ " : " ++ show fcall

main = do
  testing "pala" (pala [4,3,2] == 4)
  testing "buntut" (buntut [3,4,5,5] == [4,5,5])
  testing "akhir" (akhir [3,4,2] == 2)
  
  
  
