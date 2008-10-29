{-# LANGUAGE Rank2Types #-}
--import Control.Enumerator.Base where

type IterateeM acc a m = acc -> a -> m (IterResult acc)
newtype EnumeratorM m a =
    Enum { enum :: forall acc. IterateeM acc a m -> acc -> m acc }

mkEnum = Enum

data IterResult a 
    = IOk a
    | IStop a

instance Functor (EnumeratorM m) where
    -- fmap :: (a -> b) -> EnumeratorM m a -> EnumeratorM m b
    --       = forall r. IterateeM r a m -> r -> m r
    --         -> forall r. IterateeM r b m -> r -> m r
    fmap f (Enum enuma) = mkEnum $ \iter r ->
                            let iter' r' a = iter r' (f a) in
                            enuma iter' r

-- | Concatenate two enumerators yields a new enumerator that generates the
--  output from the first enumerator, then the output from the second
--  enumerator.
concatE :: Monad m => EnumeratorM m b -> EnumeratorM m b -> EnumeratorM m b
concatE enum1 enum2 = mkEnum $ e
  where
    e iter initSeed = enum enum1 iter' (True, initSeed) >>= k
      where
        iter' (True, seed) dat = do
          r <- iter seed dat
          case r of
            IOk y        -> return $ IOk (True, y)
            IStop y      -> return $ IStop (False, y)
        iter' (False, _) _ = error "impossible"
        k (False, seed) = return seed
        k (True, seed) = enum enum2 iter seed

stringEnum :: Monad m => String -> Int -> EnumeratorM m String
stringEnum str chunksize = mkEnum e 
  where
    e iter a = go a str
      where
        go a [] = return a
        go a s = do
          let (d,s') = splitAt chunksize s
          r <- iter a d
          case r of
            IOk a'   -> go a' s'
            IStop a' -> return a'

printIter :: Show a => IterateeM () a IO
printIter _ a = print a >> return (IOk ())

countIter :: Monad m => IterateeM Int a m
countIter n _ = return (IOk $! n + 1)

--lEnum :: Monad m => EnumeratorM m String -> EnumeratorM m Int
lEnum enumstr = mkEnum e
  where
    e iter a = enum enumstr iter' a
      where
        iter' a str = iter a (length str)
  

forE :: EnumeratorM m dat -> a -> IterateeM a dat m -> m a
forE e a i = enum e i a

t001 = forE (concatE (stringEnum "foobarbaz" 5)
                     (stringEnum "axonos" 2)) () $
            printIter
t002 = (forE (concatE (stringEnum "foobarbaz" 1)
                      (stringEnum "axonos" 2)) 0 $
            countIter)
       >>= print

t003 = forE ((\x -> (x,x)) `fmap` 
                   concatE (stringEnum "foobarbaz" 5)
                           (stringEnum "axonos" 2)) () $
            printIter

thenI :: IterateeM acc1 a m -> (acc1 -> a -> IterateeM acc2 a m) 
      -> IterateeM acc2 a m
thenI _ _ = undefined