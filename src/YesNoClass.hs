module YesNoClass where

-- monad example:
--import Control.Monad

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False