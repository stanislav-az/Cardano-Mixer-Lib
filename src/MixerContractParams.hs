{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}


module MixerContractParams where

import           Data.Aeson                          (FromJSON(..), ToJSON(..))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Plutus.V1.Ledger.Value
import           Prelude

import           Crypto

---------------------------------------- Types for API calls to the PAB -------------------------------------

-- Parameters for the "deposit" endpoint
data DepositParams = DepositParams
    {
        dpAddress        :: !Text,
        dpValue          :: !Value,
        dpLeaf           :: !Fr
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- Parameters for the "withdraw" endpoint
data WithdrawParams = WithdrawParams
    {
        wpAddress       :: !Text,
        wpValue         :: !Value,
        wpDepositNum    :: !(Integer, Integer),
        wpPublicInputs  :: !PublicInputs,
        wpProof         :: !Proof
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
