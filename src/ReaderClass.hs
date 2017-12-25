{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module ReaderClass where

-- reader monad example
newtype Reader r a = Reader
  { runReader :: r -> a
  }

instance Functor (Reader e) where
  fmap f mr = Reader (\r -> f (runReader mr r))

instance Applicative (Reader e) where
  pure x = Reader (\_ -> x)
  rf <*> rx = Reader (\r -> (runReader rf r) (runReader rx r))

instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ \_ -> a
  (>>=) :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= k = Reader $ \r -> runReader (k (runReader m r :: a) :: Reader r b) r

exampleReader :: Show a => Reader a [Char]
exampleReader = Reader id >>= \e -> return $ "hey, " ++ show e

exampleReader2 :: Show a => Reader a [Char]
exampleReader2 = do
  e <- Reader id
  return $ "hey, " ++ show e

data Cat = Cat
  { catName :: String
  , catFood :: String
  } deriving (Show)

catNameReader = Reader catName

catFoodReader = Reader catFood

nameAndFood :: Reader Cat String
nameAndFood = do
  myCatName <- catNameReader
  myCatFood <- catFoodReader
  return $ show myCatName ++ show myCatFood

nameAndFood2 =
  catNameReader >>= (\myCatName -> catFoodReader >>= (\myCatFood -> return $ show myCatName ++ show myCatFood))

---- Reader Transformer!
--newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
--
--liftReaderT :: m a -> ReaderT r m a
--liftReaderT m = ReaderT (const m)
--
---- | Transform the computation inside a @ReaderT@.
----
---- * @'runReaderT' ('mapReaderT' f m) = f . 'runReaderT' m@
--mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
--mapReaderT f m = ReaderT $ f . runReaderT m
--{-# INLINE mapReaderT #-}
--
---- | Execute a computation in a modified environment
---- (a more general version of 'local').
----
---- * @'runReaderT' ('withReaderT' f m) = 'runReaderT' m . f@
--withReaderT
--    :: (r' -> r)        -- ^ The function to modify the environment.
--    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
--    -> ReaderT r' m a
--withReaderT f m = ReaderT $ runReaderT m . f
--
--instance (Functor m) => Functor (ReaderT r m) where
--    fmap f  = mapReaderT (fmap f)
--
--instance (Applicative m) => Applicative (ReaderT r m) where
--    pure    = liftReaderT . pure
--    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
--
--instance (Monad m) => Monad (ReaderT r m) where
--    return   = liftReaderT . return
--    (>>=) :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
--    m >>= k  = ReaderT $ \r -> do
--        a <- runReaderT m r
--        runReaderT (k a) r
--    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r
