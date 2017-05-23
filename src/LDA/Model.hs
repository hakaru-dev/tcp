{-# LANGUAGE DataKinds, NegativeLiterals #-}
module LDA.Model where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

import Numeric.SpecFunctions

betaFunc :: LogFloat -> LogFloat -> LogFloat
betaFunc a b = logToLogFloat $ logBeta (fromLogFloat a) (fromLogFloat b)


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
                                (unsafeNat (nat2int (size word_prior1) + int_ -1))
                                (\ d7 ->
                                 betaFunc (summate (d7 + nat_ 1)
                                                   (size word_prior1)
                                                   (\ d丛8 -> word_prior1 ! d丛8))
                                          (word_prior1 ! d7))
                        ** (nat2real (size topic_prior0) * real_ (-1)) *
                        product (nat_ 0)
                                numDocs2
                                (\ d9 ->
                                 product (nat_ 0)
                                         (size topic_prior0)
                                         (\ i丩10 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_split (\ (d丛13,()) ->
                                                                           d丛13
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    numDocs2)
                                                                                   (\ (d丛13,()) ->
                                                                                    nat_ 1 +
                                                                                    doc4 ! d丛13)
                                                                                   (r_index (\ (d14,()) ->
                                                                                             size topic_prior0)
                                                                                            (\ (d丛13,(d14,())) ->
                                                                                             nat_ 1 +
                                                                                             z5
                                                                                             ! d丛13)
                                                                                            (r_add (\ (d丛13,(i丩15,(d14,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary12 ->
                                                   case_ summary12
                                                         [branch (ppair PVar PVar)
                                                                 (\ y16 z17 -> z17)]
                                                   ! d9
                                                   ! i丩10)
                                                  (\ j11 -> nat2prob j11 + topic_prior0 ! i丩10))) *
                        recip (product (nat_ 0)
                                       numDocs2
                                       (\ d18 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (d丛21,()) ->
                                                                         nat2int (z5 ! d丛21)
                                                                         < int_ -1)
                                                                        r_nop
                                                                        (r_split (\ (d丛21,()) ->
                                                                                  d丛21
                                                                                  == wordUpdate6)
                                                                                 r_nop
                                                                                 (r_index (\ () ->
                                                                                           numDocs2)
                                                                                          (\ (d丛21,()) ->
                                                                                           nat_ 1 +
                                                                                           doc4
                                                                                           ! d丛21)
                                                                                          (r_add (\ (d丛21,(d22,())) ->
                                                                                                  nat_ 1))))))) $ \ summary20 ->
                                                 case_ (case_ summary20
                                                              [branch (ppair PVar PVar)
                                                                      (\ y23 z24 -> z24)])
                                                       [branch (ppair PVar PVar) (\ y25 z26 -> z26)]
                                                 ! d18)
                                                (\ i丩19 ->
                                                 nat2prob i丩19 +
                                                 summate (nat_ 0)
                                                         (size topic_prior0)
                                                         (\ d丛27 -> topic_prior0 ! d丛27))))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNew丑28 ->
                                           product (nat_ 0)
                                                   (size topic_prior0)
                                                   (\ d29 ->
                                                    product (nat_ 0)
                                                            (unsafeNat (nat2int (size word_prior1) +
                                                                        int_ -1))
                                                            (\ i30 ->
                                                             betaFunc (nat2prob (let_ (bucket (nat_ 0)
                                                                                              (size w3)
                                                                                              ((r_index (\ () ->
                                                                                                         unsafeNat (nat2int (size word_prior1) +
                                                                                                                    int_ -1))
                                                                                                        (\ (d丛32,()) ->
                                                                                                         nat_ 1 +
                                                                                                         w3
                                                                                                         ! d丛32)
                                                                                                        (r_split (\ (d丛32,(i33,())) ->
                                                                                                                  d丛32
                                                                                                                  == wordUpdate6)
                                                                                                                 (r_fanout (r_add (\ (d丛32,(i33,())) ->
                                                                                                                                   nat_ 1))
                                                                                                                           r_nop)
                                                                                                                 (r_index (\ (i33,()) ->
                                                                                                                           size topic_prior0)
                                                                                                                          (\ (d丛32,(i33,())) ->
                                                                                                                           nat_ 1 +
                                                                                                                           z5
                                                                                                                           ! d丛32)
                                                                                                                          (r_add (\ (d丛32,(d34,(i33,()))) ->
                                                                                                                                  nat_ 1))))))) $ \ summary31 ->
                                                                                 case_ (d29
                                                                                        == zNew丑28)
                                                                                       [branch ptrue
                                                                                               (case_ (case_ (summary31
                                                                                                              ! i30)
                                                                                                             [branch (ppair PVar
                                                                                                                            PVar)
                                                                                                                     (\ y35
                                                                                                                        z36 ->
                                                                                                                      y35)])
                                                                                                      [branch (ppair PVar
                                                                                                                     PVar)
                                                                                                              (\ y37
                                                                                                                 z38 ->
                                                                                                               y37)]),
                                                                                        branch pfalse
                                                                                               (nat_ 0)] +
                                                                                 case_ (summary31
                                                                                        ! i30)
                                                                                       [branch (ppair PVar
                                                                                                      PVar)
                                                                                               (\ y39
                                                                                                  z40 ->
                                                                                                z40)]
                                                                                 ! d29) +
                                                                       word_prior1 ! i30)
                                                                      ((let_ (bucket (nat_ 0)
                                                                                     (size w3)
                                                                                     ((r_split (\ (d丛42,()) ->
                                                                                                d丛42
                                                                                                == wordUpdate6)
                                                                                               (r_fanout (r_split (\ (d丛42,()) ->
                                                                                                                   w3
                                                                                                                   ! d丛42
                                                                                                                   < i30)
                                                                                                                  r_nop
                                                                                                                  (r_add (\ (d丛42,()) ->
                                                                                                                          nat_ 1)))
                                                                                                         r_nop)
                                                                                               (r_index (\ () ->
                                                                                                         size topic_prior0)
                                                                                                        (\ (d丛42,()) ->
                                                                                                         nat_ 1 +
                                                                                                         z5
                                                                                                         ! d丛42)
                                                                                                        (r_split (\ (d丛42,(d43,())) ->
                                                                                                                  w3
                                                                                                                  ! d丛42
                                                                                                                  < i30)
                                                                                                                 r_nop
                                                                                                                 (r_add (\ (d丛42,(d43,())) ->
                                                                                                                         nat_ 1))))))) $ \ summary41 ->
                                                                        case_ (d29 == zNew丑28)
                                                                              [branch ptrue
                                                                                      (case_ (case_ (case_ summary41
                                                                                                           [branch (ppair PVar
                                                                                                                          PVar)
                                                                                                                   (\ y44
                                                                                                                      z45 ->
                                                                                                                    y44)])
                                                                                                    [branch (ppair PVar
                                                                                                                   PVar)
                                                                                                            (\ y46
                                                                                                               z47 ->
                                                                                                             y46)])
                                                                                             [branch (ppair PVar
                                                                                                            PVar)
                                                                                                     (\ y48
                                                                                                        z49 ->
                                                                                                      nat2prob z49)]),
                                                                               branch pfalse
                                                                                      (prob_ 0)] +
                                                                        case_ (case_ summary41
                                                                                     [branch (ppair PVar
                                                                                                    PVar)
                                                                                             (\ y50
                                                                                                z51 ->
                                                                                              z51)]
                                                                               ! d29)
                                                                              [branch (ppair PVar
                                                                                             PVar)
                                                                                      (\ y52 z53 ->
                                                                                       nat2prob z53)]) +
                                                                       summate (i30 + nat_ 1)
                                                                               (size word_prior1)
                                                                               (\ d丛54 ->
                                                                                word_prior1
                                                                                ! d丛54)))) *
                                           product (nat_ 0)
                                                   numDocs2
                                                   (\ d55 ->
                                                    case_ (d55 == nat_ 1 + doc4 ! wordUpdate6 &&
                                                           not (nat2int (doc4 ! wordUpdate6)
                                                                < int_ -1) &&
                                                           not (nat2int numDocs2 + int_ -2
                                                                < nat2int (doc4 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (d丛57,()) ->
                                                                                                     d丛57
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              numDocs2)
                                                                                                             (\ (d丛57,()) ->
                                                                                                              nat_ 1 +
                                                                                                              doc4
                                                                                                              ! d丛57)
                                                                                                             (r_index (\ (d58,()) ->
                                                                                                                       size topic_prior0)
                                                                                                                      (\ (d丛57,(d58,())) ->
                                                                                                                       nat_ 1 +
                                                                                                                       z5
                                                                                                                       ! d丛57)
                                                                                                                      (r_add (\ (d丛57,(zNew丑59,(d58,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary56 ->
                                                                             nat_ 0) +
                                                                   topic_prior0 ! zNew丑28),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d55
                                                                                         == nat_ 1 +
                                                                                            doc4
                                                                                            ! wordUpdate6))
                                                                                   [branch ptrue
                                                                                           (nat_ 1),
                                                                                    branch pfalse
                                                                                           (case_ (nat2int (doc4
                                                                                                            ! wordUpdate6)
                                                                                                   < int_ -1)
                                                                                                  [branch ptrue
                                                                                                          (nat_ 1),
                                                                                                   branch pfalse
                                                                                                          (nat_ 1)])]))]) *
                                           recip (product (nat_ 0)
                                                          numDocs2
                                                          (\ d60 ->
                                                           case_ (d60
                                                                  == nat_ 1 + doc4 ! wordUpdate6 &&
                                                                  not (nat2int (doc4 ! wordUpdate6)
                                                                       < int_ -1) &&
                                                                  not (nat2int numDocs2 + int_ -2
                                                                       < nat2int (doc4
                                                                                  ! wordUpdate6)))
                                                                 [branch ptrue
                                                                         (nat2prob (let_ (bucket (nat_ 0)
                                                                                                 (size w3)
                                                                                                 ((r_split (\ (d丛62,()) ->
                                                                                                            nat2int (z5
                                                                                                                     ! d丛62)
                                                                                                            < int_ -1)
                                                                                                           r_nop
                                                                                                           (r_split (\ (d丛62,()) ->
                                                                                                                     d丛62
                                                                                                                     == wordUpdate6)
                                                                                                                    r_nop
                                                                                                                    (r_index (\ () ->
                                                                                                                              numDocs2)
                                                                                                                             (\ (d丛62,()) ->
                                                                                                                              nat_ 1 +
                                                                                                                              doc4
                                                                                                                              ! d丛62)
                                                                                                                             (r_add (\ (d丛62,(d63,())) ->
                                                                                                                                     nat_ 1))))))) $ \ summary61 ->
                                                                                    nat_ 0) +
                                                                          summate (nat_ 0)
                                                                                  (size topic_prior0)
                                                                                  (\ d丛64 ->
                                                                                   topic_prior0
                                                                                   ! d丛64)),
                                                                  branch pfalse
                                                                         (nat2prob (case_ (not (d60
                                                                                                == nat_ 1 +
                                                                                                   doc4
                                                                                                   ! wordUpdate6))
                                                                                          [branch ptrue
                                                                                                  (nat_ 1),
                                                                                           branch pfalse
                                                                                                  (case_ (nat2int (doc4
                                                                                                                   ! wordUpdate6)
                                                                                                          < int_ -1)
                                                                                                         [branch ptrue
                                                                                                                 (nat_ 1),
                                                                                                          branch pfalse
                                                                                                                 (nat_ 1)])]))])))))),
         branch pfalse (reject)]
