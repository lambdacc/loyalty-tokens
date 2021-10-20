# Loyalty tokens using NFT
This demo showcases how to use loyalty tokens using the idea of NFT based implementation of OAuth 2.0. An issuer supplies loyalty tokens of different categories to its patron. The rewards depend on the level of the token. The rewards are encoded into the token issued for verification at a later point.


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



#### Future work
Encode expiry time for rewards into the token.  

The contract code can be cleaned up a bit. Did not get time to tidy up some unused imports.

### Tips
#### To use entr to watch your source file during development
```ls src/<file name>.hs | entr -r cabal build plutus-starter-pab --dependencies-only --disable-documentation```

#### Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc. please do so over on the main [plutus repository](https://github.com/input-output-hk/plutus).


Thanks!
