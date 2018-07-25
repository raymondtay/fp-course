{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.State where

import Course.Core
import qualified Prelude as P
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad
import qualified Data.Set as S

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State f) s = snd (f s)

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State f) = \s -> (fst . f)(s)

-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
-- put = State . const . (,) ()
put s = State (\_ -> ((), s))
-- It's a bit of a mystery how i constructed this instance and the best way is
-- to dissect how this was derived. Best to start with the simpler (imo)
-- expression of `put s = State (\_ -> ((), s))` where State consumes the input
-- but spews no output; this does the trick with the exception that it ignores
-- the input and you can best see this contrast by examining the previous
-- definition of State.
--
-- The latter definition i.e. `put = State . const . (, ) ()` is probably more
-- "complicated" to read and understand in the sense that there is a lot going
-- on in there; but what it translates down to is really this
--
-- State ( const . (,) $ () )  where `const . (,)` gives us a function of the
-- type `a -> b1 -> b2 -> (a, b2)` and when it consumes `()` it becomes
-- `b1 -> b2 -> ((), b2)` and when you compose it with State it gives us the
-- desired result.
--

-- | Implement the `Functor` instance for `State s`.
--
-- >>> runState ((+1) <$> State (\s -> (9, s * 2))) 3
-- (10,6)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  f <$> (State g) = State (\s -> let (newV, newS) = (g s) in (f(newV), newS))

-- | Implement the `Applicative` instance for `State s`.
--
-- >>> runState (pure 2) 0
-- (2,0)
--
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a, s))
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b 
  (State f) <*> (State g) = State(\s ->
    let (h, s' ) = f s
        (v, s'') = g s'
    in (h v, s'')
    )
  --State f) <*> (State g) = State (\s -> let (newV, newS) = (g s) in ( fst (f s) $ newV, newS))
  --The interesting part of this implementation is that according to the tests,
  --i have to first process 'f' before 'g' (in a previous implementation which
  --i have removed since processes 'g' before 'f' and the tests failed.)

-- | Implement the `Bind` instance for `State s`.
--
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
-- ((),16)
instance Monad (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  f =<< (State g) = State(\s ->
    let (a', s') = g s
        (b, s'') = runState (f a') $ s'
    in (b, s''))
-- The above is more involved but it should not be hard to see what i am
-- attempting to do. The harder part is to recognize the expression `runState (f a')`
-- because the result of `f a'` is a State and i have to invoke runState on
-- this result to obtain the function embedded within; lastly this embedded
-- function consumes s' and that's it.
--


-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM f (h:.t) = (f h) >>= (\x -> if x then pure (Full h) else findM f t)


-- Dissection of this is quite instructive, i find, and what happens here is
-- that when f is applied to h we have `f Bool` and we know its a Monad and i
-- can use the bind-operator s.t. i can lift the boolean and decipher that.
-- If the decipher is True then we lift the `Full h` into the applicative via
-- `pure` because a Monad is also an Applicative.

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat = error "todo next"

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> \xs -> firstRepeat (distinct xs) == Empty
--
-- prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct =
  error "todo: Course.State#distinct"

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `firstRepeat` with `produce`.
--
-- /Tip:/ Use `join` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  error "todo: Course.State#isHappy"
