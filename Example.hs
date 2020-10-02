module Example where
import Behaviour

--Example 1 
nDecay :: Double -> (Double, Double)
nDecay t = let n0 = 460000
               tHalf = 400
               lambda = (log 2) / tHalf
           in (t, n0 * exp (negate (lambda*t)))

nDecayB :: Behaviour (Double, Double)
nDecayB = let n0 = 460000
              tHalf = 400
              lambda = (log 2) / tHalf
              decayB = n0 * exp (negate ( lambda * timeB))
          in pair timeB decayB

--discrete time example: cooling and heating => tn = (1-k)^n(t0 - R) + R where r is the surrounding temp and is constant 
cooling :: Double -> (Double, Double)
cooling n = let t0 = 30.0
                r = 25.0
                k = 0.1
                tn = ((1 - k)**n) * (t0 - r) + r
            in (n, tn)

coolingB :: Behaviour (Double, Double)
coolingB = let t0 = 30.0
               r = 25.0
               k = 0.1
               tn = ((1.0 - k)**timeB) * (t0 - r) + r
           in pair timeB tn

--Example 2
projectile :: Double -> Double -> Double -> Double -> Double -> (Double, Double) 
projectile x0 y0 v0 theta t = let a = -9.81
                                  x t = let v0x = v0 * cos theta
                                        in x0 + v0x * t
                                  y t = let v0y = v0 * sin theta
                                        in y0 + v0y*t + 0.5*a*t**2 
                              in (x t, y t)

projectileB :: Double -> Double -> Double -> Double -> Behaviour(Double, Double)
projectileB x0 y0 v0 theta = let a = negate g
                                 xB = let v0x = v0 * cos theta
                                      in (lift0 x0) + int (lift0 v0x)
                                 yB = let v0y = v0 * sin theta
                                      in (lift0 y0) + int ((lift0 v0y) + (int a))
                             in pair xB yB

--Example 3
bounce :: Double -> Double -> Double -> Double
bounce v0 y0 t = let a = -9.81
                     v t = v0 + a*t
                     y t = y0 + v0*t + 0.5*a*t**2
                 in if y t <= 0.0 && (v t < 0.0)
                    then bounce (negate 0.8 * (v t)) (y t) t
                    else y t

bounceB :: Double -> Double -> Behaviour (Double, Double)
bounceB v0 y0 = let a = negate g
                    v = (lift0 v0) + int a
                    y = (lift0 y0) + int v
                    x = (lift0 0.0)
                    collision = snapshot (pair y v) (predicate ((y `lteq` lift0 0.0) &&& (v `lt` lift0 0.0)))
                in switch (pair x y) (collision `handlerV` (\(y, v) -> bounceB (negate 0.8*v) y))

-- run coolingB 1.0 10000
-- [cooling n | n <- [1, 2..10000]]
-- run (projectileB 0.0 0.0 70.0 (pi/3.0)) 0.5 10000
-- [projectile 0.0 0.0 70.0 (pi/3.0) t | t <- [0.5, 1.0..5000]]