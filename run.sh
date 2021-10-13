#!/bin/bash
printf "\This demo showcases how to use loyalty tokens using the idea of NFT based implementation of OAuth 2.0. This demo shows an authentication NFT minted by an issuer, delivered to a client wallet and then a protected resource granting and denying access to wallets based on whether they have the correct NFT.\n"
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

sleep 2
printf "\n1. Log the currency symbol for reference.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '[]' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/logCS
sleep 1

sleep 1
printf "\n2. Mint NFTs and send to A and B wallets\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"level":"GENERAL", "destW":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 2
curl -H "Content-Type: application/json" -X POST -d '{"level":"SPECIAL", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find all NFTs for the issuer inside the wallets A B C.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
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


