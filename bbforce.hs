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
              x <- [True, False]
              y <- [True, False]
              z <- [True, False]
              let f = not $ fromInt fid (x, y, z, True)
              let g = not $ fromInt gid (x, y, z, f)
              return $ condition x y z f g

          xid <- monotoneNFs
          ensure (\x y z f g -> x /= fromInt xid (y, z, f, g))

          yid <- monotoneNFs
          ensure (\x y z f g -> y /= fromInt yid (x, z, f, g))

          zid <- monotoneNFs
          ensure (\x y z f g -> z /= fromInt zid (x, y, f, g))

          return (fid, gid, xid, yid, zid)                              

main :: IO ()
main = do
  putStrLn $ "Count of all nibble functions = " ++ show (length allNFs)
  putStrLn $ "Count of monotone nibble functions = " ++ show (length monotoneNFs)
  putStrLn $ "Solutions: \n" ++ show solve
  putStrLn $ "Count of solutions = " ++ show (length solve)
