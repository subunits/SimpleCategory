{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.State
import Control.Category (Category(..))
import Prelude hiding ((.), id)
import Data.Foldable (for_)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL8

------------------------------------------------------------
-- Quaternion Type
------------------------------------------------------------

data Quaternion = Q
  { q0 :: !Double
  , q1 :: !Double
  , q2 :: !Double
  , q3 :: !Double
  } deriving (Show, Eq, Generic)

instance ToJSON Quaternion
instance FromJSON Quaternion

qConj :: Quaternion -> Quaternion
qConj (Q a b c d) = Q a (-b) (-c) (-d)

qNorm2 :: Quaternion -> Double
qNorm2 (Q a b c d) = a*a + b*b + c*c + d*d

qScale :: Double -> Quaternion -> Quaternion
qScale s (Q a b c d) = Q (s*a) (s*b) (s*c) (s*d)

qAdd :: Quaternion -> Quaternion -> Quaternion
qAdd (Q a b c d) (Q e f g h) = Q (a+e) (b+f) (c+g) (d+h)

qSub :: Quaternion -> Quaternion -> Quaternion
qSub (Q a b c d) (Q e f g h) = Q (a-e) (b-f) (c-g) (d-h)

qMul :: Quaternion -> Quaternion -> Quaternion
qMul (Q a b c d) (Q e f g h) =
  Q (a*e - b*f - c*g - d*h)
    (a*f + b*e + c*h - d*g)
    (a*g - b*h + c*e + d*f)
    (a*h + b*g - c*f + d*e)

qNormalize :: Quaternion -> Quaternion
qNormalize q@(Q a b c d) =
  let n = sqrt (qNorm2 q)
  in if n == 0 then Q 1 0 0 0 else qScale (1 / n) q

------------------------------------------------------------
-- SLERP
------------------------------------------------------------

slerp :: Quaternion -> Quaternion -> Double -> Quaternion
slerp qa qb t
  | t <= 0    = qa
  | t >= 1    = qb
  | otherwise =
      let dot0 = q0 qa * q0 qb + q1 qa * q1 qb + q2 qa * q2 qb + q3 qa * q3 qb
          dot  = if dot0 < 0 then -dot0 else dot0
          theta = acos (min 1 dot)
          sinTheta = sin theta
      in if sinTheta < 1e-6
           then qNormalize $ qAdd qa (qScale t (qSub qb qa))
           else
             let a = sin ((1 - t) * theta) / sinTheta
                 b = sin (t * theta) / sinTheta
             in qNormalize $ qAdd (qScale a qa) (qScale b qb)

------------------------------------------------------------
-- Hyperkähler Triple
------------------------------------------------------------

data HKStruct = HK
  { iOp :: Quaternion -> Quaternion
  , jOp :: Quaternion -> Quaternion
  , kOp :: Quaternion -> Quaternion
  }

iUnit, jUnit, kUnit :: Quaternion
iUnit = Q 0 1 0 0
jUnit = Q 0 0 1 0
kUnit = Q 0 0 0 1

leftMul :: Quaternion -> Quaternion -> Quaternion
leftMul = qMul

defaultHK :: HKStruct
defaultHK = HK
  { iOp = leftMul iUnit
  , jOp = leftMul jUnit
  , kOp = leftMul kUnit
  }

------------------------------------------------------------
-- Omega Potential
------------------------------------------------------------

type Omega = Quaternion -> Quaternion

omegaSample :: Double -> Omega
omegaSample s (Q a b c d) =
  let th = s * a
      bx = b * cos th - c * sin th
      cy = b * sin th + c * cos th
  in Q a bx cy d

------------------------------------------------------------
-- HK Attention
------------------------------------------------------------

data AttentionHead = AttHead
  { ahOmega  :: Omega
  , ahWeight :: Double
  }

applyHead :: AttentionHead -> Quaternion -> Quaternion
applyHead (AttHead om w) q = qScale w (om q)

applyMultiHead :: [AttentionHead] -> Quaternion -> Quaternion
applyMultiHead hs q =
  qNormalize (foldl qAdd (Q 0 0 0 0) (map (\h -> applyHead h q) hs))

mkHeads :: Int -> Double -> [AttentionHead]
mkHeads n s =
  [ AttHead (omegaSample (s * (1 + fromIntegral i/10)))
            (1 / fromIntegral n)
  | i <- [0..n-1]
  ]

------------------------------------------------------------
-- Quaternionic RNN
------------------------------------------------------------

data QRNN = QRNN
  { rnnState :: Quaternion
  , rnnStep  :: Quaternion -> Quaternion -> Quaternion
  }

qrnnStepSample :: Quaternion -> Quaternion -> Quaternion
qrnnStepSample p i =
  qNormalize $
    qAdd (qScale 0.6 p)
    (qAdd (qScale 0.4 i) (qScale 0.05 (qMul p i)))

mkQRNN :: Quaternion -> QRNN
mkQRNN q = QRNN q qrnnStepSample

qrnnUpdate :: QRNN -> Quaternion -> QRNN
qrnnUpdate st inp = st { rnnState = rnnStep st (rnnState st) inp }

------------------------------------------------------------
-- Evolution
------------------------------------------------------------

data Evolution = Evolution
  { evoHK       :: HKStruct
  , evoOmega    :: Omega
  , evoAttHeads :: [AttentionHead]
  , evoQRNN     :: QRNN
  }

evolveStep :: Double -> Evolution -> Quaternion -> (Evolution, Quaternion)
evolveStep t evo q =
  let att = applyMultiHead (evoAttHeads evo) q
      om  = evoOmega evo (qAdd q att)
      r'  = qrnnUpdate (evoQRNN evo) om
      qNext = qNormalize (rnnState r')
      qSmooth = slerp q qNext t
  in (evo { evoQRNN = r' }, qSmooth)

evolveRun :: Double -> Int -> Evolution -> Quaternion -> (Evolution, [Quaternion])
evolveRun t n evo q0 = go n evo q0 []
  where
    go 0 e q acc = (e, reverse acc)
    go k e q acc = let (e', q') = evolveStep t e q
                   in go (k-1) e' q' (q':acc)

------------------------------------------------------------
-- JSON Frame Wrapper
------------------------------------------------------------

data Frame = Frame
  { frameIndex :: Int
  , quaternion :: Quaternion
  } deriving (Show, Generic)

instance ToJSON Frame

------------------------------------------------------------
-- API Input (JSON)
------------------------------------------------------------

data APIInput = APIInput
  { slerpFactor :: Double
  , frames      :: Int
  , initQuat    :: Maybe Quaternion
  } deriving (Show, Generic)

instance FromJSON APIInput

------------------------------------------------------------
-- Main (Playground-ready with hardcoded JSON)
------------------------------------------------------------

main :: IO ()
main = do
  -- Hardcoded JSON string for playground testing
  let inputJson = "{\"slerpFactor\":0.2,\"frames\":120,\"initQuat\":{\"q0\":1,\"q1\":0.1,\"q2\":0.2,\"q3\":0.3}}"
  case decode (BL8.pack inputJson) :: Maybe APIInput of
    Nothing -> putStrLn "{\"error\":\"Invalid JSON input\"}"
    Just (APIInput t n maybeQ0) -> do
      let q0 = qNormalize $ maybe (Q 1 0.1 0.2 0.3) id maybeQ0
          evo0 = Evolution defaultHK (omegaSample 0.05) (mkHeads 4 0.02) (mkQRNN q0)
          (_, trace) = evolveRun t n evo0 q0
          arr = zipWith Frame [0..] trace
      BL8.putStrLn (encode arr)
