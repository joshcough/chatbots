find src/ | grep 'hs$' | xargs stack exec brittany -- --config-file .brittany.yaml --write-mode inplace
find test/ | grep 'hs$' | xargs stack exec brittany -- --config-file .brittany.yaml --write-mode inplace
find app/ | grep 'hs$' | xargs stack exec brittany -- --config-file .brittany.yaml --write-mode inplace


