{-# LANGUAGE DataKinds, NegativeLiterals #-}
module NaiveBayes where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  case_ (docUpdate5 < size z2)
        [branch ptrue
                ((array (size topic_prior0) $
                        \ zNew丏6 ->
                        product (nat_ 0)
                                (size topic_prior0)
                                (\ i7 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ i丣8 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_fanout (r_index (\ () ->
                                                                                     size z2)
                                                                                    (\ (i丙11,()) ->
                                                                                     doc4
                                                                                     ! i丙11)
                                                                                    (r_index (\ (docUpdate12,()) ->
                                                                                              size word_prior1)
                                                                                             (\ (i丙11,(docUpdate12,())) ->
                                                                                              w3
                                                                                              ! i丙11)
                                                                                             (r_add (\ (i丙11,(i丣13,(docUpdate12,()))) ->
                                                                                                     nat_ 1))))
                                                                           r_nop))) $ \ summary10 ->
                                                   case_ (i7 == zNew丏6)
                                                         [branch ptrue
                                                                 (case_ summary10
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y14 z15 -> y14)]
                                                                  ! docUpdate5
                                                                  ! i丣8),
                                                          branch pfalse (nat_ 0)])
                                                  (\ j9 ->
                                                   nat2prob (let_ (bucket (nat_ 0)
                                                                          (size w3)
                                                                          ((r_split (\ (i丙17,()) ->
                                                                                     doc4 ! i丙17
                                                                                     == docUpdate5)
                                                                                    r_nop
                                                                                    (r_index (\ () ->
                                                                                              size topic_prior0)
                                                                                             (\ (i丙17,()) ->
                                                                                              z2
                                                                                              ! (doc4
                                                                                                 ! i丙17))
                                                                                             (r_index (\ (i18,()) ->
                                                                                                       size word_prior1)
                                                                                                      (\ (i丙17,(i18,())) ->
                                                                                                       w3
                                                                                                       ! i丙17)
                                                                                                      (r_add (\ (i丙17,(i丣19,(i18,()))) ->
                                                                                                              nat_ 1))))))) $ \ summary16 ->
                                                             case_ summary16
                                                                   [branch (ppair PVar PVar)
                                                                           (\ y20 z21 -> z21)]
                                                             ! i7
                                                             ! i丣8) +
                                                   nat2prob j9 +
                                                   word_prior1 ! i丣8))) *
                        ((let_ (bucket (nat_ 0)
                                       (size z2)
                                       ((r_split (\ (i丙23,()) -> i丙23 == docUpdate5)
                                                 r_nop
                                                 (r_index (\ () -> size topic_prior0)
                                                          (\ (i丙23,()) -> z2 ! i丙23)
                                                          (r_add (\ (i丙23,(zNew丏24,())) ->
                                                                  nat_ 1)))))) $ \ summary22 ->
                          nat2prob (case_ summary22
                                          [branch (ppair PVar PVar) (\ y25 z26 -> z26)]
                                    ! zNew丏6)) +
                         topic_prior0 ! zNew丏6) *
                        recip (nat2prob (summate (nat_ 0)
                                                 (size z2)
                                                 (\ i丙27 ->
                                                  case_ (i丙27 == docUpdate5)
                                                        [branch ptrue (nat_ 0),
                                                         branch pfalse
                                                                (case_ (z2 ! i丙27 < nat_ 0)
                                                                       [branch ptrue (nat_ 0),
                                                                        branch pfalse
                                                                               (nat_ 1)])])) +
                               summate (nat_ 0)
                                       (size topic_prior0)
                                       (\ i丙28 -> topic_prior0 ! i丙28)) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ i29 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_fanout (r_index (\ () -> size z2)
                                                                                  (\ (i丙32,()) ->
                                                                                   doc4
                                                                                   ! i丙32)
                                                                                  (r_split (\ (i丙32,(docUpdate33,())) ->
                                                                                            w3
                                                                                            ! i丙32
                                                                                            < nat_ 0)
                                                                                           r_nop
                                                                                           (r_add (\ (i丙32,(docUpdate33,())) ->
                                                                                                   nat_ 1))))
                                                                         r_nop))) $ \ summary31 ->
                                                 case_ (i29 == zNew丏6)
                                                       [branch ptrue
                                                               (case_ (case_ summary31
                                                                             [branch (ppair PVar
                                                                                            PVar)
                                                                                     (\ y34 z35 ->
                                                                                      y34)]
                                                                       ! docUpdate5)
                                                                      [branch (ppair PVar PVar)
                                                                              (\ y36 z37 -> z37)]),
                                                        branch pfalse (nat_ 0)])
                                                (\ i丣30 ->
                                                 nat2prob (let_ (bucket (nat_ 0)
                                                                        (size w3)
                                                                        ((r_split (\ (i丙39,()) ->
                                                                                   w3 ! i丙39
                                                                                   < nat_ 0)
                                                                                  r_nop
                                                                                  (r_split (\ (i丙39,()) ->
                                                                                            doc4
                                                                                            ! i丙39
                                                                                            == docUpdate5)
                                                                                           r_nop
                                                                                           (r_index (\ () ->
                                                                                                     size topic_prior0)
                                                                                                    (\ (i丙39,()) ->
                                                                                                     z2
                                                                                                     ! (doc4
                                                                                                        ! i丙39))
                                                                                                    (r_add (\ (i丙39,(i40,())) ->
                                                                                                            nat_ 1))))))) $ \ summary38 ->
                                                           case_ (case_ summary38
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y41 z42 -> z42)])
                                                                 [branch (ppair PVar PVar)
                                                                         (\ y43 z44 -> z44)]
                                                           ! i29) +
                                                 nat2prob i丣30 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ i丙45 -> word_prior1 ! i丙45)))))),
         branch pfalse ((arrayLit []))]
