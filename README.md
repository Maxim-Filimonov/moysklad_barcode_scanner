# MoySklad Barcode Scanner
This application helps you to find missing barcodes in your inventory and scan them via barcode scanner within one screen.

## Setup
The project is using Elm language. Make sure that you have (Elm installed)[https://guide.elm-lang.org/install.html]

### Install dependencies
```
elm install
```

### Start Dev Server
```
./dev
```

### Deploying
If you want to deploy you first need to (create heroku app)[https://gist.github.com/hone/24b06869b4c1eca701f9]

After setting up an app just run deploy script
```
./deploy
```


## Current limitations
- Product data is based on your account sales report - for large inventories it helps to focus on products which are getting sold instead of the whole inventory.
- Hardcoded server for no-cors(see `getProxyUrl` function). To work around CORS limitations there is a simple nodejs app which will proxy moysklad api. (Repo)[https://github.com/Maxim-Filimonov/cors_anywhere]. Feel free to fork the report and change the url
- One file to rule them all - intentionally kept it this way - following (life of a file)[https://www.youtube.com/watch?v=XpDsk374LDE]
