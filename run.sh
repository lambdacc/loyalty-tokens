#!/bin/bash
printf "\nThis demo showcases how to use loyalty tokens using the idea of NFT based implementation of OAuth 2.0. This demo showcases how to use loyalty tokens using the idea of NFT based implementation of OAuth 2.0. An issuer supplies loyalty tokens of different categories to its patron. The rewards depend on the level of the token. The nature of the reward and its quantity and are encoded into the token issued for verification at a later point.\n"
sleep 1

printf "\n"
export IssuerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWA=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWB=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWC=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export CheckerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1

export IssuerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$IssuerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWA_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWB_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWC_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export CheckerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "CheckerContract", "caWallet":{"getWalletId": '$CheckerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
printf "\nThere are four wallets belonging to Issuer brand, 2 token holders and 1 Resort\n"
printf "\nThe issuer issued auth NFT to holders after encoding the list of privileges and expiry time in it.\n"
printf "\nThe NFT is linked to the identity of the issuer and holder as its minted using both their pub key hashes\n"

printf "\nThere are 2 levels of loyalty tokens minted GENERAL and SPECIAL\n"
printf "\nEach type comes with its set of privileges and expiration time\n"
printf "\nHolder A is 'GENERAL' member while Holder B is a 'SPECIAL' member\n"

sleep 1
printf "\n1. Log the currency symbol for reference.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '[]' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/logCS
sleep 1

sleep 1
printf "\n2. Mint NFTs and send to A and B wallets. Wallet C does not receive any.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"level":"GENERAL", "destW":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"level":"SPECIAL", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find all NFTs for the issuer inside the wallets A B C.\n"
printf "\n3. The output depends on the time elapsed since the tokens were minted\n"

printf "\nPress any key after you wait for a time < 10  or > 10 seconds...\n"
printf "\n"

if [ -t 0 ]; then
  SAVED_STTY="`stty --save`"
  stty -echo -icanon -icrnl time 0 min 0
fi

count=0
keypress=''
while [ "x$keypress" = "x" ]; do
  sleep 1
  let count+=1
  echo -ne $count'\r'
  keypress="`cat -v`"
done

if [ -t 0 ]; then stty "$SAVED_STTY"; fi

echo "You pressed '$keypress' after $count seconds"
printf "\nContinuing ... \n"


curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1



sleep 1
printf "\n Now, we iterate through minting and authorising once more \n"
printf "\n2. Mint NFTs and send to A and B wallets. Wallet C does not receive any.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"level":"GENERAL", "destW":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"level":"SPECIAL", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find all NFTs for the issuer inside the wallets A B C.\n"
printf "\n3. The output depends on the time elapsed since the tokens were minted\n"

printf "\nPress any key after you wait for a time < 10  or > 10 seconds...\n"
printf "\n"

if [ -t 0 ]; then
  SAVED_STTY="`stty --save`"
  stty -echo -icanon -icrnl time 0 min 0
fi

count=0
keypress=''
while [ "x$keypress" = "x" ]; do
  sleep 1
  let count+=1
  echo -ne $count'\r'
  keypress="`cat -v`"
done

if [ -t 0 ]; then stty "$SAVED_STTY"; fi

echo "You pressed '$keypress' after $count seconds"
printf "\nContinuing ... \n"


curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWC'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/authorise
printf "\n"
sleep 1

printf "\nThank you for your time.\n"
printf "\n"