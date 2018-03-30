{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Development.Shake
import Turtle

import AdditionsTo.Turtle

usageText :: Shell ()
usageText = err "Test help message."

main :: IO ()
main = shakeArgs shakeOptions $ do

    want ["help"]

    phony "help" $ actsh usageText
