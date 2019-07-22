module Main where

import Prelude

import Effect (Effect)
import Elmish (boot)
import HTMLHelloWorld (main')

main :: Effect Unit
main = main'
