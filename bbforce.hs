import Data.Bits
import Control.Monad

type Nibble = (Bool, Bool, Bool, Bool)
type NibbleFun = Nibble -> Bool

isMonotone :: NibbleFun -> Bool
isMonotone f = and $ do    
                x <- [True, False]
                y <- [True, False]
                z <- [True, False]
                return $ f (False, x, y, z) <= f (True, x, y, z) 
                      && f (x, False, y, z) <= f (x, True, y, z)
                      && f (x, y, False, z) <= f (x, y, True, z)
                      && f (x, y, z, False) <= f (x, y, z, True)

fromInt :: Int -> NibbleFun
fromInt n (a, b, c, d) = testBit n (fromEnum a * 8 + fromEnum b * 4 + fromEnum c * 2 + fromEnum d)

allNFs = [0..65535]

monotoneNFs = filter (isMonotone . fromInt) allNFs

solve :: [(Int, Int, Int, Int, Int)]
solve = do
          fid <- monotoneNFs
          let f = not . fromInt fid
          
          gid <- monotoneNFs
          let g = not . fromInt gid

          xid <- monotoneNFs
          guard $ and $ do
            x <- [True, False]
            y <- [True, False]
            z <- [True, False]
            let f_val = f (x, y, z, False)
            let g_val = g (x, y, z, f_val)
            let x_val = fromInt xid (y, z, f_val, g_val)
            return $ x /= x_val

          yid <- monotoneNFs
          guard $ and $ do
            x <- [True, False]
            y <- [True, False]
            z <- [True, False]
            let f_val = f (x, y, z, False)
            let g_val = g (x, y, z, f_val)
            let y_val = fromInt yid (x, z, f_val, g_val)
            return $ y /= y_val

          zid <- monotoneNFs
          guard $ and $ do
            x <- [True, False]
            y <- [True, False]
            z <- [True, False]
            let f_val = f (x, y, z, False)
            let g_val = g (x, y, z, f_val)
            let z_val = fromInt zid (x, y, f_val, g_val)
            return $ z /= z_val

          return (fid, gid, xid, yid, zid)                              

main :: IO ()
main = do
  putStrLn $ "Count of all nibble functions = " ++ show (length allNFs)
  putStrLn $ "Count of monotone nibble functions = " ++ show (length monotoneNFs)
  putStrLn $ "Solutions: \n" ++ show solve
  putStrLn $ "Count of solutions = " ++ show (length solve)
