# Hyperkähler Quaternion Evolution API (HKStack 8.0)

## Overview

This API implements a **quaternionic evolution engine** leveraging:

- **Hyperkähler triples** (i, j, k operators)
- **Omega potentials** for dynamic quaternion transformations
- **Multi-head attention**
- **Quaternionic RNN** for state evolution
- **SLERP interpolation** for smooth transitions

It produces **JSON arrays of frames**, each containing a normalized quaternion.

---

## API Input

```json
{
  "slerpFactor": 0.2,
  "frames": 120,
  "initQuat": { "q0": 1, "q1": 0.1, "q2": 0.2, "q3": 0.3 }
}
