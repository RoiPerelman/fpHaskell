-- could only write this line
-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
-- the following is the full way
import Test.Hspec

import qualified TypeClassSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "TypeClass test" TypeClassSpec.spec