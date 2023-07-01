{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ExUnits where

import Crypto
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude
import Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Plutus.V2.Ledger.Api as Plutus
import qualified Plutus.V2.Ledger.EvaluationContext as Plutus
import Codec.Serialise (serialise)
import Test
import Prelude (IO, print)
import           MixerProofs.Groth16

type ZkpDatum = ReducedReferenceString

type ZkpRedeemer = (PublicSignals, Proof)

data ZkpValidator
instance Scripts.ValidatorTypes ZkpValidator where
    type DatumType ZkpValidator = ZkpDatum
    type RedeemerType ZkpValidator = ZkpRedeemer

zkpValidator :: Validator
zkpValidator = Scripts.validatorScript zkpTyped

zkpTyped :: Scripts.TypedValidator ZkpValidator
zkpTyped =
    mkTypedValidator @ZkpValidator
         $$(PlutusTx.compile [||mkZkpValidator||])
        $$(PlutusTx.compile [||Scripts.mkUntypedValidator||])

{-# INLINEABLE mkZkpValidator #-}
mkZkpValidator
    :: ZkpDatum
    -> ZkpRedeemer
    -> ScriptContext
    -> Bool
mkZkpValidator crs (ps, proof) _ = verify crs ps proof

validatorTest
    :: Validator
    -> ReducedReferenceString
    -> PublicSignals
    -> Proof
    -> (Plutus.LogOutput,
                       Either Plutus.EvaluationError Plutus.ExBudget)
validatorTest validator crs ps proof =
    let scriptSBS = contractSBS validator
        arguments =
            [Plutus.toData crs, Plutus.toData (ps, proof), Plutus.toData ()]
    in  Plutus.evaluateScriptCounting
                (Plutus.ProtocolVersion 7 0)
                Plutus.Verbose
                Plutus.evalCtxForTesting
                scriptSBS
                arguments

contractSBS :: Validator -> SBS.ShortByteString
contractSBS validator' = SBS.toShort . LBS.toStrict $ serialise contractScript
  where
    contractScript :: Plutus.Script
    contractScript = Plutus.unValidatorScript validator'

runTest :: IO ()
runTest = do
    secret <- generateProofSecret
    let (_, pubIns, proof) = generateSimulatedWithdrawProof secret testPKH testDepositSecret testShieldedAccountSecret testMixerState
    let ps = toWithdrawPublicSignals pubIns
    print $ validatorTest zkpValidator withdrawCRS ps proof
