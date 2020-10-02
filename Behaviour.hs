{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Behaviour (
    Time,
    Behaviour,
    Event,
    lift0, timeB, mapB, lift1, lift2,
    diff, int,
    pair, triple,
    predicate, switch, snapshot,
    handlerV, handlerN,
    lt, gt, eq, neq, gteq, lteq,
    (&&&), (|||),
    g,
    run,
    sim, plot
) where

import Data.VectorSpace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Plots
import qualified Diagrams.Prelude as DP
import Diagrams.Backend.Rasterific.CmdLine
import Data.Typeable

--the types
type Time = Double

data Behaviour a where
    Lift0 :: a -> Behaviour a
    MapB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
    Switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
    Diff :: (VectorSpace a Time) => Maybe a -> Behaviour a -> Behaviour a
    Int :: (VectorSpace a Time) => a -> Behaviour a -> Behaviour a

type Event a = Behaviour (Maybe a)

--the behaviour constructor
lift0 :: a -> Behaviour a
lift0 = Lift0

--mapping a function over a behaviour
mapB :: Behaviour (a -> b) -> Behaviour a -> Behaviour b
mapB = MapB

--integration and differentiation
diff :: (VectorSpace a Time) => Behaviour a -> Behaviour a
diff = Diff Nothing

int :: (VectorSpace a Time) => Behaviour a -> Behaviour a
int = Int zeroVector

--derived functions
timeB :: Behaviour Time
timeB = int (lift0 1.0)

lift1 :: (a -> b) -> Behaviour a -> Behaviour b
lift1 f b = mapB (lift0 f) b 

lift2 :: (a -> b -> c) -> Behaviour a -> Behaviour b -> Behaviour c
lift2 f b1 b2 = mapB (lift0 f) b1 `mapB` b2

pair :: Behaviour a -> Behaviour b -> Behaviour (a, b)
pair = lift2 (\x y -> (x, y))

triple :: Behaviour a -> Behaviour b -> Behaviour c -> Behaviour (a, b, c)
triple a b c = let lift3 f x y z = (lift0 f) `mapB` x `mapB` y `mapB` z
               in lift3 (\x y z -> (x, y, z)) a b c

lt :: (Ord a) => Behaviour a -> Behaviour a -> Behaviour Bool
lt = lift2 (<)

gt :: (Ord a) => Behaviour a -> Behaviour a -> Behaviour Bool
gt = lift2 (>)

eq :: (Eq a) => Behaviour a -> Behaviour a -> Behaviour Bool
eq = lift2 (==)

neq :: (Eq a) => Behaviour a -> Behaviour a -> Behaviour Bool
neq = lift2 (/=)

gteq :: (Ord a) => Behaviour a -> Behaviour a -> Behaviour Bool
gteq = lift2 (>=)

lteq :: (Ord a) => Behaviour a -> Behaviour a -> Behaviour Bool
lteq = lift2 (<=)

(&&&) :: Behaviour Bool -> Behaviour Bool -> Behaviour Bool
(&&&) = lift2 (&&)

(|||) :: Behaviour Bool -> Behaviour Bool -> Behaviour Bool
(|||) = lift2 (||)

g :: (Floating a) => Behaviour a
g = lift0 9.81

instance Functor Behaviour where
    fmap = lift1

instance Applicative Behaviour where
    pure = lift0
    (<*>) = mapB

instance Num a => Num (Behaviour a) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    negate = lift1 negate
    fromInteger = \x -> lift0 (fromInteger x)
    abs = lift1 abs
    signum = lift1 signum

instance Fractional a => Fractional (Behaviour a) where
    (/) = lift2 (/)
    recip = lift1 recip
    fromRational = \x -> lift0 (fromRational x)

instance Floating a => Floating (Behaviour a) where
    pi = lift0 pi
    exp = lift1 exp
    log = lift1 log
    sqrt = lift1 sqrt
    (**) = lift2 (**)
    logBase = lift2 logBase
    sin = lift1 sin
    cos = lift1 cos
    tan = lift1 tan
    asin = lift1 asin
    acos = lift1 acos
    atan = lift1 atan
    sinh = lift1 sinh
    cosh = lift1 cosh
    tanh = lift1 tanh
    asinh = lift1 asinh
    acosh = lift1 acosh
    atanh = lift1 atanh

--interaction and handling
predicate :: Behaviour Bool -> Event ()
predicate b = f `lift1` b 
                where f True = Just ()
                      f _ = Nothing

switch  :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch = Switch

snapshot :: Behaviour a -> Event b -> Event a
snapshot b e = lift2 f b e
                    where f a (Just b) = Just a
                          f _ _ = Nothing

handlerV :: Event a -> (a -> b) -> Event b
handlerV e f = (fmap f) `lift1` e

handlerN :: Event a -> b -> Event b
handlerN e b = handlerV e (const b)

--interpreter: value of a behaviour at a given point in time. internal
sample :: Behaviour a -> Time -> (a, Behaviour a)
sample (Lift0 b) = const (b, Lift0 b)
sample (MapB bf b) = \t -> let (f, bf') = sample bf t
                               (a, b') = sample b t
                           in (f a, MapB bf' b')
sample (Switch b e) = \t -> case sample e t of (Just b', e') -> sample b' t
                                               (Nothing, e') -> let (v, b') = sample b t
                                                                in (v, switch b' e')
sample (Diff a b) = \t -> case a of Just x -> let (v, b') = sample b t
                                                  d = (v ^-^ x) ^/ t
                                              in (d, Diff (Just v) b')
                                    Nothing -> let (v, b') = sample b t
                                                   (v0, b0) = sample b 0.0
                                                   d = (v ^-^ v0) ^/ t
                                               in (d, Diff (Just v) b')
sample (Int a b) = \t -> let (v, nb) = sample b t
                             rect = t *^ v
                         in (a ^+^ rect, Int (a ^+^ rect) nb)

--interpreter: run function
run :: Behaviour a -> Time -> Int -> [a]
run b t n = if n < 0
            then error "Number of samples must not be negative"
            else
                let run' :: Behaviour a -> Time -> Int -> ([a] -> [a]) -> [a]
                    run' b t 0 c = c []
                    run' b t n c = run' b' t (n-1) (\res -> c (v:res))
                                    where (v, b') = sample b t
                in run' b t n id

--visualisation functions
----simulation
form :: Display
form = InWindow "" (1000, 800) (100, 100)

bgColour :: Color
bgColour = makeColorI 255 255 255 1

modelToPoint :: ((Double, Double), Behaviour (Double, Double)) -> Picture
modelToPoint model = let ((a1, a2), b) = model
                     in translate (realToFrac a1) (realToFrac a2) (circleSolid 2.0)

stateToState :: ViewPort -> Float -> ((Double, Double), Behaviour (Double, Double)) -> ((Double, Double), Behaviour (Double, Double))
stateToState vp step model = let ((a1, a2), b) = model
                                 model' = sample b (realToFrac step)
                             in model'
                        
sim ::  Behaviour(Double, Double) -> IO ()                         
sim b = simulate form bgColour 10 (sample b 0.1) modelToPoint stateToState

----plots
axis :: Behaviour (Double, Double) -> Time -> Int -> Axis B DP.V2 Double
axis b t n = r2Axis DP.&~ do
             linePlot' (run b t n)
             yLabel DP..= "label sizing"
             axisLabelStyle DP.%= DP.fc DP.white
             yAxis . axisLabelStyle . DP._fontSize DP..= DP.local 12

plot :: Behaviour (Double, Double) -> Time -> Int -> IO ()
plot b t n = r2AxisMain (axis b t n)
