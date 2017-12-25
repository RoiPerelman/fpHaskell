module WriterClass where

-- writer monad example
newtype Writer w a = Writer { runWriter :: (a, w) }

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f m = Writer $ f (runWriter m)

instance Functor (Writer w) where
   fmap f m = Writer $ let (a, w) = runWriter m in (f a, w)

instance (Monoid w) => Applicative (Writer w) where
   pure = return
   (<*>) mf m = Writer $ let
       (a, w1) = runWriter m
       (f, w2) = runWriter mf
       in (f a, w1 `mappend` w2)

instance (Monoid w) => Monad (Writer w) where
   return a = Writer (a, mempty)
   m >>= k  = Writer $ let
       (a, w)  = runWriter m
       (b, w') = runWriter (k a)
           in (b, w `mappend` w')

roiEven :: Int -> Writer String Bool
roiEven a = Writer (even a, "Multiply ")
roiNegate :: Bool -> Writer String Bool
roiNegate a = Writer (not a, "Abs")

roiCompose = runWriter $ roiEven 5 >>= roiNegate
roiCompose2 = runWriter $ do
  b1 <- roiEven 5
  roiNegate b1
