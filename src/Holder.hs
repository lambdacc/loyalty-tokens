{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Holder(endpoints, HolderSchema) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Issuer      as I hiding (endpoints)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Wallet.Emulator.Wallet (Wallet, walletPubKey)


type HolderSchema  = Endpoint "inspectVal" ()

inspectVal :: forall w s e. AsContractError e => () -> Contract w s e ()
inspectVal () = do
    logInfo @String $ "Inspecting own utxos."
    pk  <- ownPubKey
    os  <- map snd . Map.toList <$> utxosAt (pubKeyAddress pk)
    let totalVal = mconcat [view ciTxOutValue o | o <- os]
    logInfo @String
            $ "Logging total Value : " <> show totalVal
    logInfo @String $ "inspectVal complete"

inspectVal' :: Promise () HolderSchema Text ()
inspectVal' = endpoint @"inspectVal" inspectVal

endpoints :: AsContractError e => Contract () HolderSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [inspectVal'] >>  endpoints

mkSchemaDefinitions ''HolderSchema
mkKnownCurrencies []
