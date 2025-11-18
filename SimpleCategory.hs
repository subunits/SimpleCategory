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
import System.Random (mkStdGen, randomR, StdGen)

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

qNormalize :: Bool -> Quaternion -> Quaternion
qNormalize normalize q@(Q a b c d) =
  if not normalize then q
  else let n = sqrt (qNorm2 q)
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
           then qNormalize True $ qAdd qa (qScale t (qSub qb qa))
           else
             let a = sin ((1 - t) * theta) / sinTheta
                 b = sin (t * theta) / sinTheta
             in qNormalize True $ qAdd (qScale a qa) (qScale b qb)

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
  qNormalize True (foldl qAdd (Q 0 0 0 0) (map (\h -> applyHead h q) hs))

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
  , rnnWeights :: (Double, Double, Double) -- (prev, input, interaction)
  , rnnStep  :: (Double, Double, Double) -> Quaternion -> Quaternion -> Quaternion
  }

qrnnStepSample :: (Double, Double, Double) -> Quaternion -> Quaternion -> Quaternion
qrnnStepSample (wPrev, wInput, wInteract) p i =
  qNormalize True $
    qAdd (qScale wPrev p)
         (qAdd (qScale wInput i) (qScale wInteract (qMul p i)))

mkQRNN :: Quaternion -> (Double, Double, Double) -> QRNN
mkQRNN q weights = QRNN q weights (qrnnStepSample)

qrnnUpdate :: QRNN -> Quaternion -> QRNN
qrnnUpdate st inp = st { rnnState = rnnStep st (rnnWeights st) (rnnState st) inp }

------------------------------------------------------------
-- Evolution
------------------------------------------------------------

data Evolution = Evolution
  { evoHK       :: HKStruct
  , evoOmega    :: Omega
  , evoAttHeads :: [AttentionHead]
  , evoQRNN     :: QRNN
  , evoNormalize :: Bool
  }

evolveStep :: Double -> Evolution -> Quaternion -> (Evolution, Quaternion)
evolveStep t evo q =
  let att = applyMultiHead (evoAttHeads evo) q
      om  = evoOmega evo (qAdd q att)
      r'  = qrnnUpdate (evoQRNN evo) om
      qNext = qNormalize (evoNormalize evo) (rnnState r')
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
-- API Input
------------------------------------------------------------

data APIInput = APIInput
  { slerpFactor :: Double
  , frames      :: Int
  , initQuat    :: Maybe Quaternion
  , numHeads    :: Maybe Int
  , omegaScale  :: Maybe Double
  , rnnWeightsAPI :: Maybe (Double, Double, Double) -- renamed to avoid conflict
  , normalize   :: Maybe Bool
  , seed        :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON APIInput

------------------------------------------------------------
-- Main
------------------------------------------------------------

main :: IO ()
main = do
  let inputJson = "{\"slerpFactor\":0.2,\"frames\":120,\"initQuat\":{\"q0\":1,\"q1\":0.1,\"q2\":0.2,\"q3\":0.3},\"numHeads\":4,\"omegaScale\":0.05,\"rnnWeightsAPI\":[0.6,0.4,0.05],\"normalize\":true,\"seed\":42}"
  case decode (BL8.pack inputJson) :: Maybe APIInput of
    Nothing -> putStrLn "{\"error\":\"Invalid JSON input\"}"
    Just input -> do
      let q0 = qNormalize (maybe True id (normalize input))
               $ maybe (Q 1 0.1 0.2 0.3) id (initQuat input)
          numH = maybe 4 id (numHeads input)
          omegaS = maybe 0.05 id (omegaScale input)
          rnnW = maybe (0.6,0.4,0.05) id (rnnWeightsAPI input)
          normalizeFlag = maybe True id (normalize input)
          evo0 = Evolution defaultHK (omegaSample omegaS) (mkHeads numH omegaS) (mkQRNN q0 rnnW) normalizeFlag
          (_, trace) = evolveRun (slerpFactor input) (frames input) evo0 q0
          arr = zipWith Frame [0..] trace
      BL8.putStrLn (encode arr)
