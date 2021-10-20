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

module Checker (endpoints, CheckerSchema) where

import Control.Lens (view)
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as DF
import Data.List.Split (splitOn)
import qualified Data.Map as Map hiding (empty)
import Data.Text as DT (Text, pack, unpack)
import qualified Data.Text as Text
import Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified GHC.List as GL (filter,elem)
import Issuer as I hiding (endpoints)
import Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import qualified PlutusTx.Prelude
import qualified PlutusTx.AssocMap as PMap
import qualified PlutusTx.Builtins as PB
import PlutusTx.Builtins.Class
import PlutusTx.Prelude as PlutusTx hiding (Semigroup (..), unless)
import Wallet.Emulator.Wallet (Wallet, walletAddress, walletPubKey)
import Prelude (IO, Semigroup (..), Show (..), String, last,(==), read, (>=))
{-What are the best practices when it comes to imports?-}

data AuthParam = AuthParam
  { issuerWallet :: Wallet
  , holderWallet :: Wallet
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type CheckerSchema = Endpoint "authorise" AuthParam

authorise :: forall w s e. AsContractError e => AuthParam -> Contract w s e ()
authorise param =
  do
    pkh <- pubKeyHash <$> ownPubKey
    let hw = holderWallet param
        iw = issuerWallet param
        cs = I.issuerCS $ (pubKeyHash . walletPubKey) iw
        hwPkh = (pubKeyHash . walletPubKey) hw
    os <- map snd . Map.toList <$> utxosAt (walletAddress hw)
    let osMatchingCS = mconcat [view ciTxOutValue o | o <- os, csMatcher cs (view ciTxOutValue o)]
        flatValue = [(a, b, c) | (a, b, c) <- flattenValue osMatchingCS, cs Prelude.== a]
        {-filteredFlatValue = [(a, b, c) | (a, b, c) <- flatValue, tokenAttributesMatch hwPkh b]-}
    filteredFlatValue <- filterM (\(a,b,c) -> tokenAttributesMatch hwPkh b) flatValue
    if null filteredFlatValue
      then logInfo @String $ "No loyalty tokens found in wallet"
      else do
        logInfo @String $ "Found reward" ++ show (collectRewards filteredFlatValue)

collectRewards :: [(CurrencySymbol, TokenName, Integer)] -> [String]
collectRewards = map (\(_, tn, qty) -> showReward tn qty)

showReward :: TokenName -> Integer -> String
showReward tn qty =
  let tnStringSplits = splitOn "#" $ show tn
      rewardPart = head $ tail tnStringSplits
   in rewardPart ++ "-for-" ++ show qty

csMatcher :: CurrencySymbol -> Value -> Bool
csMatcher cs val = cs `elem` symbols val

tokenAttributesMatch :: forall w s e. AsContractError e => PubKeyHash -> TokenName -> Contract w s e Bool
tokenAttributesMatch pkh tn =
    do
      let tnStringSplits = splitOn "#" $ show tn
          tokenPkhPart = head tnStringSplits
          {-expiryPart = (GL.filter (GL.notElem "123456789") (last tnStringSplits)) :: String -- Did not compile --hack to remove escape chars that creep in "1596159112999\""-}
          expiryPart = [x | x <-  (last tnStringSplits) , x `GL.elem` "1234567890"] --hack to remove escape chars that creep in "1596159112999\""
          pkhMatched = isMatchingPkh tokenPkhPart pkh
      isValid <- isNotExpired expiryPart
      return $ pkhMatched && isValid

isMatchingPkh :: String -> PubKeyHash -> Bool
isMatchingPkh pkhString pkh =
          case show pkh of
            pkhString   -> True
            _           -> False
          {- Neither of these work at runtime!!-
          pkhPart Prelude.== pkh'
          pkhPart PlutusTx.Prelude.== pkh'-}


isNotExpired :: forall w s e. AsContractError e => String -> Contract w s e Bool
isNotExpired exp =
        currentTime >>= (\t -> return ((read exp :: Integer) Prelude.>= getPOSIXTime t))


authorise' :: Promise () CheckerSchema Text ()
authorise' = endpoint @"authorise" authorise

endpoints :: AsContractError e => Contract () CheckerSchema Text e
endpoints = do
  logInfo @String "Waiting for request."
  selectList [authorise'] >> endpoints

mkSchemaDefinitions ''CheckerSchema
mkKnownCurrencies []