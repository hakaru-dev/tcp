{-# LANGUAGE DataKinds, NegativeLiterals #-}
module LDA2.Model where

import           Data.Number.LogFloat (LogFloat)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Runtime.CmdLine

import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ numTopics2 ->
  lam $ \ w3 ->
  lam $ \ d4 ->
  lam $ \ z5 ->
  lam $ \ nUp6 ->
  lam $ \ n_wz7 ->
  lam $ \ n_dz8 ->
  lam $ \ n_z9 ->
  categorical (array numTopics2 $
                     \ zNew10 ->
                     unsafeProb ((fromInt (n_wz7 ! (w3 ! nUp6 * numTopics2 + zNew10)) +
                                  fromInt (nat2int (case_ (z5 ! nUp6 == zNew10)
                                                          [branch ptrue (nat_ 1),
                                                           branch pfalse (nat_ 0)]) *
                                           int_ -1) +
                                  fromProb word_prior1) *
                                 (fromInt (n_dz8 ! (d4 ! nUp6 * numTopics2 + zNew10)) +
                                  fromInt (nat2int (case_ (z5 ! nUp6 == zNew10)
                                                          [branch ptrue (nat_ 1),
                                                           branch pfalse (nat_ 0)]) *
                                           int_ -1) +
                                  fromProb topic_prior0) *
                                 recip (fromInt (n_z9 ! zNew10) +
                                        fromInt (nat2int (case_ (z5 ! nUp6 == zNew10)
                                                                [branch ptrue (nat_ 1),
                                                                 branch pfalse (nat_ 0)]) *
                                                 int_ -1) +
                                        fromProb (nat2prob (size w3) * word_prior1))))
