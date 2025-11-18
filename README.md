# Hyperkähler Stack 8.0 – Full API and Playground Documentation

This Haskell module implements Hyperkähler Stack 8.0, a quaternion-based evolution framework with attention, quaternionic RNN, omega potential, and SLERP interpolation. The API outputs a JSON array of quaternion frames suitable for downstream applications, visualization, or machine learning pipelines.

---

## Features

- Quaternion arithmetic: `qAdd`, `qSub`, `qMul`, `qScale`, `qNormalize`, `qConj`
- Hyperkähler triple operations: `iOp`, `jOp`, `kOp`
- Omega potential: `omegaSample`
- Multi-head quaternionic attention
- Quaternionic RNN with configurable weights
- SLERP interpolation for smooth transitions
- JSON API output: array of frames
- Unicode and ASCII-safe output

---

## Module Structure

1. **Quaternion Type**
   - `data Quaternion = Q { q0, q1, q2, q3 :: Double }`
   - Basic operations: addition, subtraction, multiplication, scaling, conjugate, normalization
   - JSON serialization via `ToJSON` and `FromJSON` instances

2. **Hyperkähler Triple**
   - `data HKStruct = HK { iOp, jOp, kOp :: Quaternion -> Quaternion }`
   - Default operators use left multiplication by quaternion units (`iUnit`, `jUnit`, `kUnit`)
   - Supports quaternionic rotations in 3D manifold

3. **Omega Potential**
   - `omegaSample :: Double -> Quaternion -> Quaternion`
   - Provides phase-based rotation along quaternion components
   - Used in attention heads and evolution

4. **Quaternionic Attention**
   - `data AttentionHead = AttHead { ahOmega :: Omega, ahWeight :: Double }`
   - Multi-head attention: normalized sum of weighted quaternion transformations
   - Configurable number of heads (`numHeads`) and omega scaling

5. **Quaternionic RNN**
   - `data QRNN = QRNN { rnnState :: Quaternion, rnnStep :: Quaternion -> Quaternion -> Quaternion }`
   - Evolution step combines previous state, input, and interaction: `qrnnStepSample`
   - QRNN weights configurable: `(prevWeight, inputWeight, interactionWeight)` via `rnnWeightsAPI`
   - Normalization can be toggled

6. **SLERP Interpolation**
   - Smooth interpolation between consecutive quaternions
   - Factor controlled via `slerpFactor` (0 = no movement, 1 = full step)
   - Handles near-identical quaternions to avoid numerical instability

7. **Evolution Loop**
   - `evolveStep` applies attention, omega potential, QRNN, normalization, and optional SLERP
   - `evolveRun` iterates for N frames
   - Produces an array of quaternions representing the system's evolution

---

## API Input Parameters

JSON object fields:

| Field            | Type                   | Default                         | Description |
|------------------|-----------------------|---------------------------------|-------------|
| `slerpFactor`    | Double                | 0.2                             | SLERP interpolation factor per step |
| `frames`         | Int                   | 120                             | Number of evolution steps |
| `initQuat`       | Quaternion            | {q0:1,q1:0.1,q2:0.2,q3:0.3}    | Initial quaternion |
| `numHeads`       | Int                   | 4                               | Number of attention heads |
| `omegaScale`     | Double                | 0.05                            | Omega potential scaling factor |
| `rnnWeightsAPI`  | [Double,Double,Double]| [0.6,0.4,0.05]                  | QRNN weights (prev, input, interaction) |
| `normalize`      | Bool                  | true                            | Normalize quaternion after each step |
| `seed`           | Int                   | random                          | Random seed for reproducibility |

---

## Quaternion Format

```json
{
  "q0": 1.0,
  "q1": 0.1,
  "q2": 0.2,
  "q3": 0.3
}
## API Output Format

The API returns a **JSON array of frames**, where each frame represents the quaternion state after an evolution step. Example:

```json
[
  {
    "frameIndex": 0,
    "quaternion": { "q0": 1.0, "q1": 0.1, "q2": 0.2, "q3": 0.3 }
  },
  {
    "frameIndex": 1,
    "quaternion": { "q0": 0.998, "q1": 0.102, "q2": 0.198, "q3": 0.301 }
  },
  ...
]
