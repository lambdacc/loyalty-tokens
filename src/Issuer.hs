{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Issuer (issuerCS, endpoints, unpackPkh, IssuerSchema) where

import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS (ByteString, length, pack, unpack)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map hiding (empty)
import Data.Text (Text)
import qualified Data.Time.Clock as DTC
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract (ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.Builtins.Class
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet (Wallet, WalletId, getWalletId, walletPubKey)
import Prelude (IO, Semigroup (..), Show (..), String)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

data IssueParam = IssueParam
  { level :: String
  , destW :: Wallet
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type IssuerSchema =
  Endpoint "mint" IssueParam
    .\/ Endpoint "logCS" ()

mint :: forall w s e. AsContractError e => IssueParam -> Contract w s e ()
mint param = do
  pkh <- pubKeyHash <$> ownPubKey
  utxos <- utxosAt $ pubKeyHashAddress pkh
  let destWallet = destW param
      destPkh = pubKeyHash . walletPubKey $ destWallet
      val = buildValue (pkh, level param, destPkh)
      lookups =
        Constraints.mintingPolicy (policy pkh)
          <> Constraints.unspentOutputs utxos
      tx =
        Constraints.mustMintValue val
          <> Constraints.mustPayToPubKey destPkh val
  logInfo @String $ printf "Minting NFT %s" (show val)
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ printf "Issued NFT %s" (show val)

buildValue :: (PubKeyHash, String, PubKeyHash) -> Value
buildValue (pkh, "GENERAL", destPkh) =
  Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh, "LOUNGE")) 2
buildValue (pkh, "SPECIAL", destPkh) =
  Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh, "SUITE")) 2
    <> Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh, "SPA")) 2

buildTokenName :: (PubKeyHash, String) -> String
buildTokenName (p, r) = show (getPubKeyHash p) ++ "#" ++ r

unpackPkh :: PubKeyHash -> String
unpackPkh = BSC.unpack . fromBuiltin . getPubKeyHash

logCS :: forall w s e. AsContractError e => () -> Contract w s e ()
logCS () = do
  pkh <- pubKeyHash <$> ownPubKey
  let cs = issuerCS pkh
  logInfo @String $
    "Logging own nft token name : " <> show cs
  logInfo @String $ "logCS complete"

mint' :: Promise () IssuerSchema Text ()
mint' = endpoint @"mint" mint

logCS' :: Promise () IssuerSchema Text ()
logCS' = endpoint @"logCS" logCS

endpoints :: AsContractError e => Contract () IssuerSchema Text e
endpoints = do
  logInfo @String "Waiting for request."
  selectList [mint', logCS'] >> endpoints

mkSchemaDefinitions ''IssuerSchema
mkKnownCurrencies []
