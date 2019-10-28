find src/ | grep 'hs$' | xargs stack exec stylish-haskell -- --inplace
find test/ | grep 'hs$' | xargs stack exec stylish-haskell -- --inplace
find app/ | grep 'hs$' | xargs stack exec stylish-haskell -- --inplace
