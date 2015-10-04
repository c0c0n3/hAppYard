{-# LANGUAGE UnicodeSyntax #-}
--
-- Utility functions to run the program.
--
module Program where

import Prelude.Unicode

import System.Environment
import System.Exit


runAction ∷ IO Bool → IO ()
runAction action = do
        isSuccess ← action
        exitWith $ exitStatus isSuccess
    where
    exitStatus True  = ExitSuccess
    exitStatus False = ExitFailure (0-1)
