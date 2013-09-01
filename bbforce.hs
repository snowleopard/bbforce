import Data.Bits
import Control.Monad

type Nibble = (Bool, Bool, Bool, Bool)
type NibbleFun = Nibble -> Bool

isMonotone :: NibbleFun -> Bool
isMonotone f = and $ do    
                a <- [True, False]
                b <- [True, False]
                c <- [True, False]
                return $ f (False, a, b, c) <= f (True, a, b, c) 
                      && f (a, False, b, c) <= f (a, True, b, c)
                      && f (a, b, False, c) <= f (a, b, True, c)
                      && f (a, b, c, False) <= f (a, b, c, True)

fromInt :: Int -> NibbleFun
fromInt n (a, b, c, d) = testBit n (fromEnum a * 8 + fromEnum b * 4 + fromEnum c * 2 + fromEnum d)

allNFs = [0..65535]

monotoneNFs = filter (isMonotone . fromInt) allNFs

solve :: [(Int, Int, Int, Int, Int)]
solve = do
          fid <- monotoneNFs
          gid <- monotoneNFs

          let ensure condition = guard $ and $ do
              a <- [True, False]
              b <- [True, False]
              c <- [True, False]
              let f = not $ fromInt fid (a, b, c, True)
              let g = not $ fromInt gid (a, b, c, f)
              return $ condition a b c f g

          aid <- monotoneNFs
          ensure (\a b c f g -> a /= fromInt aid (b, c, f, g))

          bid <- monotoneNFs
          ensure (\a b c f g -> b /= fromInt bid (a, c, f, g))

          cid <- monotoneNFs
          ensure (\a b c f g -> c /= fromInt cid (a, b, f, g))

          return (fid, gid, aid, bid, cid)                              

main :: IO ()
main = do
  putStrLn $ "Count of all nibble functions = " ++ show (length allNFs)
  putStrLn $ "Count of monotone nibble functions = " ++ show (length monotoneNFs)
  putStrLn $ "Solutions: \n" ++ show solve
  putStrLn $ "Count of solutions = " ++ show (length solve)
