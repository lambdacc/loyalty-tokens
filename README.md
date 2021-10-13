# Loyalty tokens using NFT
This demo showcases how to use loyalty tokens using the idea of NFT based implementation of OAuth 2.0. This demo shows an authentication NFT minted by an issuer, delivered to a client wallet and then a protected resource granting and denying access to wallets based on whether they have the correct NFT.


This project gives uses the [Plutus Platform starter project](https://github.com/input-output-hk/plutus-starter) as the template.

### Setting up
Please refer to [Setting up](https://github.com/input-output-hk/plutus-starter#setting-up) section of the plutus starter project.

### The Plutus Application Backend (PAB)

With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with two contracts, `AuthNFTIssuer` and  `ProtectedResource` contracts under `/src` folder.

Here's an example of running and interacting with this contract via the API. For this it will help if you have `jq` installed.

1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````

This will then start up the server on port 9080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).


### How to run the demo

- Perform the setup for **Plutus** as described in the plutus starter project.
- Clone this repo to your local environment.
- Start a nix shell from the plutus repo from the first step.  
- Build the PAB executable:
```
cabal build plutus-starter-pab
```
Ignore the unused import warnings. Did not get time to clean that up.

- Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````
- Now, execute `run.sh` for the demo.
  - You might need to do `chmod +x run.sh` first in case execute permissions are missing.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

#### Endpoints
`AuthNFTIssuer.hs` has the following endpoints:
- `mint` : It mints NFT for a given Wallet id as input and sends it to that wallet.
- `inspect` : It logs the total value in the wallet. This is used for demoing that the minted NFT is delivered to the client wallet and not kept in the issuer wallet.
- `logWalletNftTokenName` : This logs the token name that the issuer will use for a given wallet id. 

`ProtectedResource.hs` has the following endpoints:
- `checkAccess` : This checks for the correct auth NFT at the given wallet id. If the wallet holds the NFT, access can be granted.

### What you see in the demo
The `run.sh` script execute the following.

- It create 4 wallets. One is the auth NFT issuer, two are client wallets and the last one is the protected resource.
- It then activates the requisite contacts in the wallets. The NFT issuer and 2 client wallets get instance of  `AuthNFTIssuerContract` where the fourth wallet gets an instance of `ProtectedResourceContract`.
- First client Wallet One requests for NFT. The issuer authorises wallet One. So, it mints an NFT using its own pkh and the Wallet One's pkh. We omit the logic for who to authorise at this point. The client wallets pkh is derived from the wallet id that is passed in the request payload.

- The `inspect` endpoints logs the value in issuer and client wallet. it should show that the NFT was delivered to the client.
- Second wallet, Wallet Two does not obtain the auth NFT.
- Now Protected Resource has to decide who to authorise, Wallet One or Two
- The protected smart contract checks both wallets. It finds the NFT in Wallet One and not in Wallet Two. In the logs, you can see that the protected smarts logs `ACCESS GRANTED` and `ACCESS DENIED` accordingly.


#### Future work
The token name used in this demo was built using the public key hash of the client wallet (NFT requestor). The token name can be encoded with more information like the expiry time of the auth NFT, the list of privileges obtained by holding this NFT etc.  

The contract code can be cleaned up a bit. Did not get time to tidy up some unused imports.

### Tips
#### To use entr to watch your source file during development
```ls src/<file name>.hs | entr -r cabal build plutus-starter-pab --dependencies-only --disable-documentation```

#### Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc. please do so over on the main [plutus repository](https://github.com/input-output-hk/plutus).


Thanks!
