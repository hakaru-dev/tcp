{-# LANGUAGE DataKinds, NegativeLiterals #-}
module LDA.Model where

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
  lam $ \ numDocs5 ->
  lam $ \ wordUpdate6 ->
  case_ (wordUpdate6 < size w3)
        [branch ptrue
                ((pose (product (nat_ 0)
                                (size topic_prior0)
                                (\ d7 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ i丛8 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_index (\ () ->
                                                                           size topic_prior0)
                                                                          (\ (d両11,()) -> z2 ! d両11)
                                                                          (r_index (\ (d12,()) ->
                                                                                    size word_prior1)
                                                                                   (\ (d両11,(d12,())) ->
                                                                                    w3
                                                                                    ! d両11)
                                                                                   (r_add (\ (d両11,(i丛13,(d12,()))) ->
                                                                                           nat_ 1)))))) $ \ summary10 ->
                                                   summary10 ! d7
                                                   ! i丛8)
                                                  (\ j9 -> nat2prob j9 + word_prior1 ! i丛8))) *
                        product (nat_ 0)
                                (size w3)
                                (\ d14 ->
                                 product (nat_ 0)
                                         (size topic_prior0)
                                         (\ i丯15 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_split (\ (d両18,()) ->
                                                                           d両18
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    size topic_prior0)
                                                                                   (\ (d両18,()) ->
                                                                                    z2
                                                                                    ! d両18)
                                                                                   (r_index (\ (i丯19,()) ->
                                                                                             size w3)
                                                                                            (\ (d両18,(i丯19,())) ->
                                                                                             doc4
                                                                                             ! d両18)
                                                                                            (r_add (\ (d両18,(d20,(i丯19,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary17 ->
                                                   case_ summary17
                                                         [branch (ppair PVar PVar)
                                                                 (\ y21 z22 -> z22)]
                                                   ! i丯15
                                                   ! d14)
                                                  (\ j16 -> nat2prob j16 + topic_prior0 ! i丯15))) *
                        recip (product (nat_ 0)
                                       (size w3)
                                       (\ d23 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (d両26,()) ->
                                                                         z2 ! d両26
                                                                         < nat_ 0)
                                                                        r_nop
                                                                        (r_split (\ (d両26,()) ->
                                                                                  d両26
                                                                                  == wordUpdate6)
                                                                                 r_nop
                                                                                 (r_index (\ () ->
                                                                                           size w3)
                                                                                          (\ (d両26,()) ->
                                                                                           doc4
                                                                                           ! d両26)
                                                                                          (r_add (\ (d両26,(d27,())) ->
                                                                                                  nat_ 1))))))) $ \ summary25 ->
                                                 case_ (case_ summary25
                                                              [branch (ppair PVar PVar)
                                                                      (\ y28 z29 -> z29)])
                                                       [branch (ppair PVar PVar) (\ y30 z31 -> z31)]
                                                 ! d23)
                                                (\ i丯24 ->
                                                 nat2prob i丯24 +
                                                 summate (nat_ 0)
                                                         (size topic_prior0)
                                                         (\ d両32 -> topic_prior0 ! d両32)))) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ d33 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_index (\ () -> size topic_prior0)
                                                                        (\ (d両36,()) -> z2 ! d両36)
                                                                        (r_split (\ (d両36,(d37,())) ->
                                                                                  w3 ! d両36
                                                                                  < nat_ 0)
                                                                                 r_nop
                                                                                 (r_add (\ (d両36,(d37,())) ->
                                                                                         nat_ 1)))))) $ \ summary35 ->
                                                 case_ (summary35 ! d33)
                                                       [branch (ppair PVar PVar)
                                                               (\ y38 z39 -> z39)])
                                                (\ i丛34 ->
                                                 nat2prob i丛34 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ d両40 -> word_prior1 ! d両40))))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNew丑41 ->
                                           product (nat_ 0)
                                                   (size w3)
                                                   (\ d42 ->
                                                    case_ (d42 == doc4 ! wordUpdate6 &&
                                                           not (doc4 ! wordUpdate6 < nat_ 0) &&
                                                           not (nat2int (size w3) + int_ -1
                                                                < nat2int (doc4 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (d両44,()) ->
                                                                                                     d両44
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              size topic_prior0)
                                                                                                             (\ (d両44,()) ->
                                                                                                              z2
                                                                                                              ! d両44)
                                                                                                             (r_index (\ (zNew丑45,()) ->
                                                                                                                       size w3)
                                                                                                                      (\ (d両44,(zNew丑45,())) ->
                                                                                                                       doc4
                                                                                                                       ! d両44)
                                                                                                                      (r_add (\ (d両44,(d46,(zNew丑45,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary43 ->
                                                                             nat_ 0) +
                                                                   topic_prior0 ! zNew丑41),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d42
                                                                                         == doc4
                                                                                            ! wordUpdate6))
                                                                                   [branch ptrue
                                                                                           (nat_ 1),
                                                                                    branch pfalse
                                                                                           (case_ (doc4
                                                                                                   ! wordUpdate6
                                                                                                   < nat_ 0)
                                                                                                  [branch ptrue
                                                                                                          (nat_ 1),
                                                                                                   branch pfalse
                                                                                                          (nat_ 1)])]))]) *
                                           recip (product (nat_ 0)
                                                          (size w3)
                                                          (\ d47 ->
                                                           case_ (d47 == doc4 ! wordUpdate6 &&
                                                                  not (doc4 ! wordUpdate6
                                                                       < nat_ 0) &&
                                                                  not (nat2int (size w3) + int_ -1
                                                                       < nat2int (doc4
                                                                                  ! wordUpdate6)))
                                                                 [branch ptrue
                                                                         (nat2prob (let_ (bucket (nat_ 0)
                                                                                                 (size w3)
                                                                                                 ((r_split (\ (d両49,()) ->
                                                                                                            z2
                                                                                                            ! d両49
                                                                                                            < nat_ 0)
                                                                                                           r_nop
                                                                                                           (r_split (\ (d両49,()) ->
                                                                                                                     d両49
                                                                                                                     == wordUpdate6)
                                                                                                                    r_nop
                                                                                                                    (r_index (\ () ->
                                                                                                                              size w3)
                                                                                                                             (\ (d両49,()) ->
                                                                                                                              doc4
                                                                                                                              ! d両49)
                                                                                                                             (r_add (\ (d両49,(d50,())) ->
                                                                                                                                     nat_ 1))))))) $ \ summary48 ->
                                                                                    nat_ 0) +
                                                                          summate (nat_ 0)
                                                                                  (size topic_prior0)
                                                                                  (\ d両51 ->
                                                                                   topic_prior0
                                                                                   ! d両51)),
                                                                  branch pfalse
                                                                         (nat2prob (case_ (not (d47
                                                                                                == doc4
                                                                                                   ! wordUpdate6))
                                                                                          [branch ptrue
                                                                                                  (nat_ 1),
                                                                                           branch pfalse
                                                                                                  (case_ (doc4
                                                                                                          ! wordUpdate6
                                                                                                          < nat_ 0)
                                                                                                         [branch ptrue
                                                                                                                 (nat_ 1),
                                                                                                          branch pfalse
                                                                                                                 (nat_ 1)])]))])))))),
         branch pfalse (reject)]
