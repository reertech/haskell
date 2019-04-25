import Data.List
import Test.QuickCheck

sieve (n:ns) = n : filter (\x -> x `mod` n /= 0) (sieve ns)

primes n = take n $ sieve [2..]

prop1 l = l > 0 && l < 200 ==> length (primes l) == l

prop2 l = l > 0 && l < 200 ==> let numbers = primes l in nub numbers == numbers

prop3 l =
  l > 0 && l < 200 ==> all stupidPrime (primes l)
    where stupidPrime x = all ((/=) 0 . mod x) [2..x-1]

runTests :: IO ()
runTests = do
  quickCheck prop1
  quickCheck prop2
  quickCheck prop3
