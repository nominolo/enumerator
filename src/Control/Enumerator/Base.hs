{-IUEUOEAU # LANGUAGE RankNTypes, GADTs #-}
{-# OPTIONS_GHC -fno-monomorphism-restriction -fglasgow-exts #-}
import Prelude hiding ( Enum )

import Debug.Trace

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
{-
newtype Iter' a -- chunk type
              m -- monad
              r -- *input* accumulator type
  = Iter' { runIter' :: r -> a -> m (Either  ....
-}
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
{-
-- Iterators that return a result and possibly the unconsumed input.
-- INVARIANT: Right ((_, s),_) ==> s == ""
--            (i.e. "gimme more" implies no unconsumed input)
--
type Chunky m r = String -> Iter String m (r, String)

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
    run1 
-}

-- @s@: stream type (not symbol type)
data Syn s a where
  Symbol :: Syn s s
  Fail   :: Syn s a
  (:+++) :: Syn s a -> Syn s a -> Syn s a
  Return :: a -> Syn s a
  (:>>=) :: Syn s a -> (a -> Syn s b) -> Syn s b

infixr 8 :+++
infixr 9 :>>=

type Parser s a = [s] -> [(a, [s])]

mkParser :: Syn s a -> Parser s a
mkParser Symbol        (c : s)  = [ (c, s) ]
mkParser Symbol        []       = []
mkParser Fail          s        = []
mkParser (p :+++ q)  s        = mkParser p s ++ mkParser q s
mkParser (Return x)    s        = [ (x, s) ]
mkParser (p :>>= k)    s        = [ (y, s'')  | (x, s')  <- mkParser p s
                                              , (y, s'') <- mkParser (k x) s' ]


laws :: Syn s a -> Syn s a
laws (Return x :>>= k)     = k x
--laws (p :>>= Return)     = p
laws ((p :>>= k') :>>= k)  = p :>>= (\x -> k' x :>>= k)
laws (Fail :>>= k)         = Fail
laws ((p :+++ q) :>>= k)   = (p :>>= k) :+++ (q :>>= k)
laws (Fail :+++ q)         = q
laws (p :+++ Fail)         = p
laws ((p :+++ q) :+++ r)   = p :+++ (q :+++ r)
--laws (p :+++ q)            = q :+++ p
laws ((Symbol :>>= k) :+++ (Symbol :>>= k'))
                           = Symbol :>>= (\c -> k c :+++ k' c)

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

parseI_first :: Monad m => P s a -> Iter [s] m (Maybe a)
parseI_first p = parseI_first' (runP p (\x -> ReturnPlus x Fail'))

parseI_first' :: Monad m => P' s a -> Iter [s] m (Maybe a)
parseI_first' p = Iter (go p)
  where 
    go _ (Just x) _ = -- already found something, we're done
        return (Left (Just x))
    go (SymbolBind f)   a (c : s) =
        go (f c) a s
    go p@(SymbolBind _) a []      =
        -- end of chunk, parser remains in same state, so that if there's more
        -- we parse it the same way we tried to parse this one.  If we're not
        -- called again, a is the result (Nothing)
        return (Right (a, parseI_first' p))
    go (ReturnPlus x _) _ _       =
        return (Left (Just x))
    go Fail' _ _ =
        return (Left Nothing)

-- | Only succeeds if it finds a parse for the full input.
parseFullE :: (Monad m, Show a, Show s) => P s a -> Enum m [s] -> m (Maybe a)
parseFullE p (Enum enum) = do 
  r <- enum (parseI_tillEof p) NoRslt
  case r of
    RsltIfEof a -> return (Just a)
    _           -> return Nothing

parseI_tillEof :: 
    ( Monad m
    , Show a, Show s
    )=> P s a -> Iter [s] m (EofSt a)
parseI_tillEof p = parseI_tillEof' (runP p (\x -> ReturnPlus x Fail'))

data EofSt a = NoRslt | RsltIfEof a deriving Show

parseI_tillEof' :: 
    ( Monad m
    , Show a, Show s
    ) => P' s a -> Iter [s] m (EofSt a)
parseI_tillEof' p = Iter (go p)
  where
    -- no input left in current chunk
    -- some input left
    go (SymbolBind f)   a (c : s)  = trace (show ("go_bind", a, c, s)) $
        -- if we had some parse result, we don't have anymore, since
        -- we only want the output right before EOF
        go (f c) NoRslt s
    go p@(SymbolBind _) a [] = 
        trace (show ("go_eoc", a)) $
        return (Right (a, parseI_tillEof' p))
    go (ReturnPlus x p) a s = trace (show ("go_return", x, s)) $ 
        -- ok, we have *some* output, dependening on whether we hit eof or not
        go p (RsltIfEof x) s
    go p@Fail' a [] = trace (show ("go_fail_eoc", a)) $ 
                      return (Right (a, parseI_tillEof' p))
    go Fail' a s  = trace (show ("go_fail", a, s)) $ return (Left NoRslt)
   

tp001 = do p <- runE (strEnum "foobarbaz" 2) Nothing (parseI_first p001)
           print p

tp002 = do p <- parseFullE p001 (strEnum "foobar" 2)
           print p

tp003 = do p <- parseFullE p001 (strEnum "foobarz" 2)
           print p
