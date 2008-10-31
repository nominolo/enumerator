{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-monomorphism-restriction #-}

import Debug.Trace

type IterateeM a m r = r -> a -> m (Either r r)
newtype EnumeratorM m a =
    EnumM { enum :: forall r. IterateeM a m r -> r -> m r }

mkEnum :: (forall r. IterateeM a m r -> r -> m r) -> EnumeratorM m a
mkEnum = EnumM

instance Functor (EnumeratorM m) where
    fmap f (EnumM enuma) = mkEnum $ \iter r ->
                            let iter' r' a = iter r' (f a) in
                            enuma iter' r

------------------------------------------------------------------------

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
            Right a' -> go a' s'
            Left a'  -> return a'

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


------------------------------------------------------------------------
-- Parser stuff
-- 
-- TODO: move into separate file
-- TODO: implement look-ahead (at least a single symbol)

newtype P s a = MkP { runP :: forall b. (a -> P' s b) -> P' s b }
data P' s a 
  = SymbolBind (s -> P' s a)
  | Fail'
  | ReturnPlus a (P' s a)

infixr 8 +++

instance Monad (P s) where
  return  = succeed
  p >>= f = pbind p f
  fail _  = pfail

symbol :: P s s
symbol     = MkP $ \k -> SymbolBind k

pfail  :: P s a
pfail      = MkP $ \k -> Fail'

(+++) :: P s a -> P s a -> P s a
p +++ q    = MkP $ \k -> runP p k `choice` runP q k

succeed :: a -> P s a
succeed x  = MkP $ \k -> k x

pbind :: P s a -> (a -> P s b) -> P s b
pbind p f  = MkP $ \k -> runP p (\x -> runP (f x) k)

choice (SymbolBind f)    (SymbolBind g)    = SymbolBind (\c -> f c `choice` g c)
choice Fail'             q                 = q
choice p                 Fail'             = p
choice (ReturnPlus x p)  q                 = ReturnPlus x (p `choice` q)
choice p                 (ReturnPlus x q)  = ReturnPlus x (p `choice` q)

-----

parse p    = parse' (runP p (\x -> ReturnPlus x Fail'))

parse' (SymbolBind f)    (c : s)  = parse' (f c) s
parse' (ReturnPlus x p)  s        = (x, s) : parse' p s
parse' _                 _        = []

-----

satisfy :: (s -> Bool) -> P s s
satisfy p = do s <- symbol
               if p s then return s
                      else pfail

char :: Char -> P Char Char
char c = satisfy (==c)

literal :: String -> P Char String
literal s = l' s
  where l' [] = return s
        l' (c:cs) = char c >> l' cs

space :: P Char ()
space = char ' ' >> return ()

p001 = literal "foobar"

------------------------------------------------------------------------

parseFullE' :: (Monad m, Show a, Show s) => 
               P s a -> EnumeratorM m [s]
            -> m (Maybe [a])
parseFullE' p e = do
  let p' = runP p (\x -> ReturnPlus x Fail')
  let iter = pFullIter
  (_, r) <- enum e iter (p', Nothing)
  return r

pFullIter :: (Monad m, Show a, Show s) =>
             IterateeM [s] m (P' s a, Maybe [a])
pFullIter (p0, a0) s0 = go p0 a0 s0
  where
    go (SymbolBind f) a (c : s) = 
        trace (show ("go_bind", a, c, s)) $
        go (f c) Nothing s
    go p@(SymbolBind _) a [] =
        trace (show ("go_eoc", a)) $
        return (Right (p, a))
    go (ReturnPlus x p) a s =
        trace (show ("go_return", x, s)) $ 
        -- ok, we have *some* output, dependening on whether we hit eof or not
        go p (addResult x a) s
    go p@Fail' a [] = 
        trace (show ("go_fail_eoc", a)) $ 
        return (Right (p, a))
    go p@Fail' a s  = 
        trace (show ("go_fail", a, s)) $
        return (Left (p, Nothing))
 
    addResult x Nothing   = Just [x]
    addResult x (Just xs) = Just (x:xs)

t010 = do p <- parseFullE' p001 (stringEnum "foobar" 2)
          print p

p002 = (symbol >> return 1) +++ (char 'a' >> return 2)

t011 = do p <- parseFullE' p002 (stringEnum "a" 2)
          print p

------------------------------------------------------------------------
{-
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
-}
