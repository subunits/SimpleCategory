# Hyperkähler Stack – Kernel & Logarithm Summary

## 1. Stack Iteration (Global / Cross-Layer)
- Propagates quaternionic RNN states, HK-Attention outputs, and Ω-Potentials across layers.
- Maintains global manifold consistency.
- Each layer receives updated states from the previous layer (residual connections possible).

## 2. Kernel Iteration (Local / Intra-Layer)
- Operates within a single layer/module.
- Refines local neighborhoods of quaternions using:
  - SLERP (Spherical Linear Interpolation)
  - Ω-Potential mixing
  - Attention weighting
  - Quaternionic logarithms
- Workflow:
  1. Map quaternions to tangent space: `v_i = log(q_i)`
  2. Apply kernel combination: `v_i_new = Σ K(i,j) * v_j`
  3. Map back to manifold: `q_i_new = exp(v_i_new)`
- Ensures smooth, manifold-consistent local transformations.

## 3. User Interaction / Querying
- Queries can pull:
  - Full layer states
  - Local quaternion neighborhoods
  - Ω-Potentials and attention outputs
- Users do not manually write kernels in standard operation; they observe or query outputs.
- When actively writing kernels, quaternionic logarithms define the transformations applied locally.

## 4. Key Concepts
- Stack iteration = global propagation across layers.
- Kernel iteration = local refinement within a layer.
- Quaternionic logarithms allow linear operations in tangent space, preserving manifold structure.
- Fully version-agnostic, scalable to Hyperkähler Stack 1000.0 and beyond.

---

### Legend
- `q_i` → Quaternion state at position i  
- `v_i` → Tangent-space vector (logarithm of `q_i`)  
- `K(i,j)` → Kernel weight between quaternion neighbors  
- SLERP → Smooth interpolation of quaternion states  
- Ω-Potential → Hyperkähler potential for geometric consistency  
- `exp/log` → Exponential / logarithm map between manifold and tangent space
