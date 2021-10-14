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

module Issuer(issuerCS, endpoints, unpackPkh, IssuerSchema) where

import qualified Data.ByteString as BS (ByteString, pack, unpack, length)
import qualified Data.ByteString.Char8 as BSC
import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           Data.Time
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Builtins
import           PlutusTx.Builtins.Class
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet (Wallet, WalletId, walletPubKey, getWalletId)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

data IssueParam = IssueParam
    { level   :: String
    , destW   :: Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type IssuerSchema = Endpoint "mint" IssueParam
                .\/ Endpoint "logCS" ()

mint :: forall w s e. AsContractError e => IssueParam -> Contract w s e ()
mint param = do
    pkh <- pubKeyHash <$> ownPubKey
    utxos <- utxosAt $ pubKeyHashAddress pkh
    let destWallet = destW param
        destPkh = (pubKeyHash . walletPubKey) $ destWallet
        {-val = testVal (pkh, level param, destPkh)-}
    val <- buildValue (pkh, level param, destPkh)
    let lookups = Constraints.mintingPolicy (policy pkh) <>
              Constraints.unspentOutputs utxos
        tx  = Constraints.mustMintValue val <>
              Constraints.mustPayToPubKey destPkh val
    logInfo @String $ printf "Minting NFT %s" (show val)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Issued NFT %s" (show val)

testVal :: (PubKeyHash, String , PubKeyHash) -> Value
testVal (pkh, "GENERAL", destPkh) =
  Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh,"LOUNGE")) 2

buildValue :: (PubKeyHash, String , PubKeyHash) -> IO Value
buildValue (pkh, "GENERAL", destPkh) =
  do
    exp <- getExpiry "GENERAL"
    return (Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh,"LOUNGE")) 2)
buildValue (pkh, "SPECIAL", destPkh) =
  do
    exp <- getExpiry "SPECIAL"
    return (Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh,"SUITE")) 2 <>
           Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString $ buildTokenName (destPkh,"SPA")) 2)

getExpiry :: String -> IO String  {-It isn't possible to return String here, correct?-}
getExpiry "GENERAL" = getCurrentTime >>= (\x -> return (addUTCTime (30 :: NominalDiffTime) x)) >>= (\y -> return (show y))

buildTokenName :: (PubKeyHash, String) -> String
buildTokenName (p,r) = show (getPubKeyHash p) ++ "#" ++ r

--TODO Clean up after review
unpackPkh :: PubKeyHash -> String
unpackPkh = BSC.unpack . fromBuiltin . getPubKeyHash

logCS :: forall w s e. AsContractError e => () -> Contract w s e ()
logCS () = do
    pkh <- pubKeyHash <$> ownPubKey
    let cs = issuerCS pkh
    logInfo @String
            $ "Logging own currency symbol : " <> show cs
    logInfo @String $ "logCS complete"

mint' :: Promise () IssuerSchema Text ()
mint' = endpoint @"mint" mint

logCS' :: Promise () IssuerSchema Text ()
logCS' = endpoint @"logCS" logCS

endpoints :: AsContractError e => Contract () IssuerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [mint',logCS'] >>  endpoints

mkSchemaDefinitions ''IssuerSchema
mkKnownCurrencies []
