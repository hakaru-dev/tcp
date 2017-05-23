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
  lam $ \ numDocs2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ z5 ->
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
                                                                ((r_split (\ (d両11,()) ->
                                                                           d両11
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    size word_prior1)
                                                                                   (\ (d両11,()) ->
                                                                                    w3
                                                                                    ! d両11)
                                                                                   (r_index (\ (i丛12,()) ->
                                                                                             size topic_prior0)
                                                                                            (\ (d両11,(i丛12,())) ->
                                                                                             z5
                                                                                             ! d両11)
                                                                                            (r_add (\ (d両11,(d13,(i丛12,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary10 ->
                                                   case_ summary10
                                                         [branch (ppair PVar PVar)
                                                                 (\ y14 z15 -> z15)]
                                                   ! i丛8
                                                   ! d7)
                                                  (\ j9 -> nat2prob j9 + word_prior1 ! i丛8))) *
                        product (nat_ 0)
                                numDocs2
                                (\ d16 ->
                                 product (nat_ 0)
                                         (size topic_prior0)
                                         (\ i丯17 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_split (\ (d両20,()) ->
                                                                           d両20
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    numDocs2)
                                                                                   (\ (d両20,()) ->
                                                                                    doc4
                                                                                    ! d両20)
                                                                                   (r_index (\ (d21,()) ->
                                                                                             size topic_prior0)
                                                                                            (\ (d両20,(d21,())) ->
                                                                                             z5
                                                                                             ! d両20)
                                                                                            (r_add (\ (d両20,(i丯22,(d21,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary19 ->
                                                   case_ summary19
                                                         [branch (ppair PVar PVar)
                                                                 (\ y23 z24 -> z24)]
                                                   ! d16
                                                   ! i丯17)
                                                  (\ j18 -> nat2prob j18 + topic_prior0 ! i丯17))) *
                        recip (product (nat_ 0)
                                       numDocs2
                                       (\ d25 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (d両28,()) ->
                                                                         z5 ! d両28
                                                                         < nat_ 0)
                                                                        r_nop
                                                                        (r_split (\ (d両28,()) ->
                                                                                  d両28
                                                                                  == wordUpdate6)
                                                                                 r_nop
                                                                                 (r_index (\ () ->
                                                                                           numDocs2)
                                                                                          (\ (d両28,()) ->
                                                                                           doc4
                                                                                           ! d両28)
                                                                                          (r_add (\ (d両28,(d29,())) ->
                                                                                                  nat_ 1))))))) $ \ summary27 ->
                                                 case_ (case_ summary27
                                                              [branch (ppair PVar PVar)
                                                                      (\ y30 z31 -> z31)])
                                                       [branch (ppair PVar PVar) (\ y32 z33 -> z33)]
                                                 ! d25)
                                                (\ i丯26 ->
                                                 nat2prob i丯26 +
                                                 summate (nat_ 0)
                                                         (size topic_prior0)
                                                         (\ d両34 -> topic_prior0 ! d両34)))) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ d35 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (d両38,()) ->
                                                                         w3 ! d両38
                                                                         < nat_ 0)
                                                                        r_nop
                                                                        (r_split (\ (d両38,()) ->
                                                                                  d両38
                                                                                  == wordUpdate6)
                                                                                 r_nop
                                                                                 (r_index (\ () ->
                                                                                           size topic_prior0)
                                                                                          (\ (d両38,()) ->
                                                                                           z5
                                                                                           ! d両38)
                                                                                          (r_add (\ (d両38,(d39,())) ->
                                                                                                  nat_ 1))))))) $ \ summary37 ->
                                                 case_ (case_ summary37
                                                              [branch (ppair PVar PVar)
                                                                      (\ y40 z41 -> z41)])
                                                       [branch (ppair PVar PVar) (\ y42 z43 -> z43)]
                                                 ! d35)
                                                (\ i丛36 ->
                                                 nat2prob i丛36 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ d両44 -> word_prior1 ! d両44))))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNew丑45 ->
                                           product (nat_ 0)
                                                   (size word_prior1)
                                                   (\ d46 ->
                                                    case_ (d46 == w3 ! wordUpdate6 &&
                                                           not (w3 ! wordUpdate6 < nat_ 0) &&
                                                           not (nat2int (size word_prior1) + int_ -1
                                                                < nat2int (w3 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (d両48,()) ->
                                                                                                     d両48
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              size word_prior1)
                                                                                                             (\ (d両48,()) ->
                                                                                                              w3
                                                                                                              ! d両48)
                                                                                                             (r_index (\ (d49,()) ->
                                                                                                                       size topic_prior0)
                                                                                                                      (\ (d両48,(d49,())) ->
                                                                                                                       z5
                                                                                                                       ! d両48)
                                                                                                                      (r_add (\ (d両48,(zNew丑50,(d49,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary47 ->
                                                                             nat_ 0) +
                                                                   word_prior1 ! d46),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d46
                                                                                         == w3
                                                                                            ! wordUpdate6))
                                                                                   [branch ptrue
                                                                                           (nat_ 1),
                                                                                    branch pfalse
                                                                                           (case_ (w3
                                                                                                   ! wordUpdate6
                                                                                                   < nat_ 0)
                                                                                                  [branch ptrue
                                                                                                          (nat_ 1),
                                                                                                   branch pfalse
                                                                                                          (nat_ 1)])]))]) *
                                           product (nat_ 0)
                                                   numDocs2
                                                   (\ d51 ->
                                                    case_ (d51 == doc4 ! wordUpdate6 &&
                                                           not (doc4 ! wordUpdate6 < nat_ 0) &&
                                                           not (nat2int numDocs2 + int_ -1
                                                                < nat2int (doc4 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (d両53,()) ->
                                                                                                     d両53
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              numDocs2)
                                                                                                             (\ (d両53,()) ->
                                                                                                              doc4
                                                                                                              ! d両53)
                                                                                                             (r_index (\ (d54,()) ->
                                                                                                                       size topic_prior0)
                                                                                                                      (\ (d両53,(d54,())) ->
                                                                                                                       z5
                                                                                                                       ! d両53)
                                                                                                                      (r_add (\ (d両53,(zNew丑55,(d54,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary52 ->
                                                                             nat_ 0) +
                                                                   topic_prior0 ! zNew丑45),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d51
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
                                                          numDocs2
                                                          (\ d56 ->
                                                           case_ (d56 == doc4 ! wordUpdate6 &&
                                                                  not (doc4 ! wordUpdate6
                                                                       < nat_ 0) &&
                                                                  not (nat2int numDocs2 + int_ -1
                                                                       < nat2int (doc4
                                                                                  ! wordUpdate6)))
                                                                 [branch ptrue
                                                                         (nat2prob (let_ (bucket (nat_ 0)
                                                                                                 (size w3)
                                                                                                 ((r_split (\ (d両58,()) ->
                                                                                                            z5
                                                                                                            ! d両58
                                                                                                            < nat_ 0)
                                                                                                           r_nop
                                                                                                           (r_split (\ (d両58,()) ->
                                                                                                                     d両58
                                                                                                                     == wordUpdate6)
                                                                                                                    r_nop
                                                                                                                    (r_index (\ () ->
                                                                                                                              numDocs2)
                                                                                                                             (\ (d両58,()) ->
                                                                                                                              doc4
                                                                                                                              ! d両58)
                                                                                                                             (r_add (\ (d両58,(d59,())) ->
                                                                                                                                     nat_ 1))))))) $ \ summary57 ->
                                                                                    nat_ 0) +
                                                                          summate (nat_ 0)
                                                                                  (size topic_prior0)
                                                                                  (\ d両60 ->
                                                                                   topic_prior0
                                                                                   ! d両60)),
                                                                  branch pfalse
                                                                         (nat2prob (case_ (not (d56
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
                                                                                                                 (nat_ 1)])]))])) *
                                           recip (case_ (not (w3 ! wordUpdate6 < nat_ 0))
                                                        [branch ptrue
                                                                (nat2prob (let_ (bucket (nat_ 0)
                                                                                        (size w3)
                                                                                        ((r_split (\ (d両62,()) ->
                                                                                                   w3
                                                                                                   ! d両62
                                                                                                   < nat_ 0)
                                                                                                  r_nop
                                                                                                  (r_split (\ (d両62,()) ->
                                                                                                            d両62
                                                                                                            == wordUpdate6)
                                                                                                           r_nop
                                                                                                           (r_index (\ () ->
                                                                                                                     size topic_prior0)
                                                                                                                    (\ (d両62,()) ->
                                                                                                                     z5
                                                                                                                     ! d両62)
                                                                                                                    (r_add (\ (d両62,(zNew丑63,())) ->
                                                                                                                            nat_ 1))))))) $ \ summary61 ->
                                                                           case_ (case_ summary61
                                                                                        [branch (ppair PVar
                                                                                                       PVar)
                                                                                                (\ y64
                                                                                                   z65 ->
                                                                                                 z65)])
                                                                                 [branch (ppair PVar
                                                                                                PVar)
                                                                                         (\ y66
                                                                                            z67 ->
                                                                                          z67)]
                                                                           ! zNew丑45) +
                                                                 summate (nat_ 0)
                                                                         (size word_prior1)
                                                                         (\ d両68 ->
                                                                          word_prior1
                                                                          ! d両68)),
                                                         branch pfalse (prob_ 1)]))))),
         branch pfalse (reject)]
