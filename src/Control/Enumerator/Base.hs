{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-monomorphism-restriction #-}
import Prelude hiding ( Enum )
--import Control.Enumerator.Base where
{-
type IterateeM acc a m = acc -> a -> m (IterResult acc)
newtype EnumeratorM m a =
    EnumM { enum :: forall acc. IterateeM acc a m -> acc -> m acc }

mkEnum = EnumM

data IterResult a 
    = IOk a
    | IStop a

instance Functor (EnumeratorM m) where
    -- fmap :: (a -> b) -> EnumeratorM m a -> EnumeratorM m b
    --       = forall r. IterateeM r a m -> r -> m r
    --         -> forall r. IterateeM r b m -> r -> m r
    fmap f (EnumM enuma) = mkEnum $ \iter r ->
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
-}
------------------------------------------------------------------------

-- Iterator returns a continuation.  This can be a more convenient way to
-- change state.
--
-- Drawback: iterator needs to be a newtype (else type would be infinite).
--
-- XXX: Can we change the state of the accumulator?
newtype Iter a m r = Iter { runIter :: r -> a -> m (Either r (r, Iter a m r)) }
newtype Enum m a = Enum (forall r. Iter a m r -> r -> m r)

prIter :: Show a => Iter a IO ()
prIter = it
  where it = Iter it'
        it' _ a = do print a; return $ Right ((),it)


strEnum :: Monad m => String -> Int -> Enum m String
strEnum str chunksize = Enum e
  where
    e iter r = go iter r str
    go _ r [] = return r
    go (Iter it) r s = do
        let (a, s') = splitAt chunksize s
        ir <- it r a
        case ir of
          Left r' -> return r'
          Right (r', iter') -> go iter' r' s'

iToggle :: Show a => Iter a IO ()
iToggle = it1
  where
    it1 = Iter it1'
    it1' _ a = do print a; return $ Right ((), it2)
    it2 = Iter it2'
    it2' _ a = do putStrLn (reverse (show a)); return $ Right ((), it1)

thenI :: Monad m => Iter a m r -> Iter a m r -> Iter a m r
thenI i1 i2 = it
  where
    it = Iter it'
    it' r a = do
      ir <- runIter i1 r a
      case ir of
        Left r' -> return $ Right (r', i2)
        Right (r', i1') -> return $ Right (r', i1' `thenI` i2)

runE :: Enum m a -> r -> Iter a m r -> m r
runE (Enum e) a i = e i a

onceI :: Monad m => (a -> m ()) -> Iter a m ()
onceI act = it
  where
    it = Iter it'
    it' r a = act a >> return (Left ())

twiceI :: Monad m => (a -> m ()) -> Iter a m ()
twiceI act = it1
  where
    it1 = Iter it1'
    it1' r a = act a >> return (Right ((), it2))
    it2 = Iter i21'
    i21' r a = act a >> return (Left ())

t001 = runE (strEnum "foobarbaz" 3) () prIter
t002 = runE (strEnum "foobarbaz" 3) () iToggle
t003 = runE (strEnum "foobarbazmoo" 3) ()
            (twiceI print `thenI` onceI (print . length))

-- Iterators that return a result and possibly the unconsumed input.
-- INVARIANT: Right ((_, s),_) ==> s == ""
--            (i.e. "gimme more" implies no unconsumed input)
--
type Chunky m r = Iter String m (r, String)
{-
thenC :: Monad m => Chunky m r -> Chunky m r -> Chunky m r
thenC i1 i2 = it
  where
    it = Iter it'
    it' (r, rest) str = do
      ir1 <- runIter i1 (r, rest)
      case ir1 of
        Right (r', i1') -> do
          ir1' <- runIter i1' r' str
          case ir1' of
            Right (r'', i1'') ->
        Left (r', rest') ->
            ir2 <- runIter i2 (r', rest')
-}
