npm install -g json-server $$ elm-package install
or npm install json-server --save (local install) run using ./node_modules/json-server sth stch
run server: json-server --watch db.json
run client: elm-live src/Main.elm --open
or: elm-reactor
