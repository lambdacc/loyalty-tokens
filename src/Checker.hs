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

module Checker(endpoints, CheckerSchema) where

import qualified Data.ByteString.Char8 as BSC
import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.List.Split        (splitOn)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              as DT (Text,pack,unpack)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Issuer                 as I hiding (endpoints)
import           Plutus.Contract
import qualified PlutusTx               as PlutusTx
import qualified PlutusTx.AssocMap      as PMap
import qualified PlutusTx.Builtins      as PB
import           PlutusTx.Builtins.Class
import           PlutusTx.Prelude       as PlutusTx hiding (Semigroup(..), unless)
import           Plutus.V1.Ledger.Value as Value
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..), last )
import           Wallet.Emulator.Wallet (Wallet, walletAddress, walletPubKey)
import           Data.Text.Encoding       as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding                as TE
import qualified Data.ByteString.Base16            as B16
import qualified Data.Text                         as Text

data AuthParam = AuthParam
    { issuerWallet :: Wallet
    , holderWallet :: Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type CheckerSchema = Endpoint "authorise" AuthParam

authorise :: forall w s e. AsContractError e => AuthParam -> Contract w s e ()
authorise param =
  do
    pkh <- pubKeyHash <$> ownPubKey
    let hw  = holderWallet param
        iw = issuerWallet param
        cs = I.issuerCS $ (pubKeyHash . walletPubKey) iw
        hwPkh = (pubKeyHash . walletPubKey) hw
    os  <- map snd . Map.toList <$> utxosAt (walletAddress hw)
    let osMatchingCS = mconcat [view ciTxOutValue o | o <- os, csMatcher cs (view ciTxOutValue o)]
        flatValue = flattenValue osMatchingCS
        filteredFlatValue = [(a,b,c) | (a,b,c) <- flatValue, cs == a, pkhMatcher hwPkh b]
    if null filteredFlatValue
      then logInfo @String $ "No loyalty tokens found in wallet"
      else
        do
          logInfo @String $ "Found reward" ++ (show $ collectRewards filteredFlatValue)


pkhMatcher :: PubKeyHash -> TokenName -> Bool
pkhMatcher pkh tn =
        let tnString = show tn
            pkhPart = show $ head $ splitOn "#" (tnString)
            pkh' = show pkh
            in case pkhPart of
              pkh' -> True
              _ -> False

collectRewards :: [(CurrencySymbol,TokenName,Integer)] -> [String]
collectRewards l = map (\(_,tn,qty) -> showReward tn qty) l

showReward :: TokenName -> Integer -> String
showReward tn qty =
          let tnString = show tn
              rewardPart = show $ last $ splitOn "#" (tnString)
              in rewardPart ++ show qty


{- Why does this not work ???
pkhMatcher :: PubKeyHash -> TokenName -> Bool
pkhMatcher pkh tn =
      let tnString = show tn
          pkhPart = show $ head $ splitOn "#" (tnString)
          pkh' = show pkh
          in
            pkhPart == pkh'-}

csMatcher :: CurrencySymbol -> Value -> Bool
csMatcher cs val = cs `elem` (symbols val)

logEachToken :: Value -> Contract w s e ()
logEachToken val = logInfo @String $ "Reward: " ++ (show val)

{- This was overkill
unpackTokenName :: TokenName -> String
unpackTokenName = DT.unpack . PlutusTx.fromBuiltin . PB.decodeUtf8 . unTokenName
-}

authorise' :: Promise () CheckerSchema Text ()
authorise' = endpoint @"authorise" authorise

endpoints :: AsContractError e => Contract () CheckerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [authorise'] >>  endpoints

mkSchemaDefinitions ''CheckerSchema
mkKnownCurrencies []
