{-# LANGUAGE TemplateHaskell #-}

module Crypto
(
    module Crypto.BLS12381,
    module Crypto.Curve,
    module Crypto.DFT,
    module Crypto.Extension,
    module Crypto.MerkleTree,
    module Crypto.MiMC,
    module Crypto.Polynomial,
    module Crypto.R1CS,
    module Crypto.ZKSNARK,
    module Crypto.Zp
)
where

import           Crypto.BLS12381
import           Crypto.Curve
import           Crypto.DFT
import           Crypto.Extension
import           Crypto.MerkleTree
import           Crypto.MiMC
import           Crypto.Polynomial
import           Crypto.R1CS
import           Crypto.ZKSNARK
import           Crypto.Zp
import qualified PlutusTx

PlutusTx.makeIsDataIndexed ''ReducedReferenceString [('ReducedReferenceString, 0)]
PlutusTx.makeIsDataIndexed ''PublicSignals [('PublicSignals, 0)]
PlutusTx.makeIsDataIndexed ''Proof [('Proof, 0)]
PlutusTx.makeIsDataIndexed ''CurvePoint [('CP, 0), ('O, 1)]
PlutusTx.makeIsDataIndexed ''Extension [('E, 0)]
PlutusTx.makeIsDataIndexed ''Polynomial [('P, 0)]
PlutusTx.makeIsDataIndexed ''E2 [('E2, 0)]
