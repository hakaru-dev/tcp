{-# LANGUAGE DataKinds, NegativeLiterals #-}
module LDA.Model where

import           Data.Number.LogFloat (LogFloat)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Runtime.LogFloatCmdLine

import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ numDocs2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ z5 ->
  lam $ \ wordUpdate6 ->
  case_ (wordUpdate6 < size w3 &&
         doc4 ! wordUpdate6 < numDocs2 &&
         w3 ! wordUpdate6 < size word_prior1)
        [branch ptrue
                ((pose (product (nat_ 0)
                                (size topic_prior0)
                                (\ d7 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ it8 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_split (\ (dB11,()) ->
                                                                           dB11
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    size word_prior1)
                                                                                   (\ (dB11,()) ->
                                                                                    w3
                                                                                    ! dB11)
                                                                                   (r_index (\ (it12,()) ->
                                                                                             size topic_prior0)
                                                                                            (\ (dB11,(it12,())) ->
                                                                                             z5
                                                                                             ! dB11)
                                                                                            (r_add (\ (dB11,(d13,(it12,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary10 ->
                                                   case_ summary10
                                                         [branch (ppair PVar PVar)
                                                                 (\ y14 z15 -> z15)]
                                                   ! it8
                                                   ! d7)
                                                  (\ j9 -> nat2prob j9 + word_prior1 ! it8))) *
                        product (nat_ 0)
                                numDocs2
                                (\ d16 ->
                                 product (nat_ 0)
                                         (size topic_prior0)
                                         (\ iN17 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_split (\ (dB20,()) ->
                                                                           dB20
                                                                           == wordUpdate6)
                                                                          r_nop
                                                                          (r_index (\ () ->
                                                                                    numDocs2)
                                                                                   (\ (dB20,()) ->
                                                                                    doc4
                                                                                    ! dB20)
                                                                                   (r_index (\ (d21,()) ->
                                                                                             size topic_prior0)
                                                                                            (\ (dB20,(d21,())) ->
                                                                                             z5
                                                                                             ! dB20)
                                                                                            (r_add (\ (dB20,(iN22,(d21,()))) ->
                                                                                                    nat_ 1))))))) $ \ summary19 ->
                                                   case_ summary19
                                                         [branch (ppair PVar PVar)
                                                                 (\ y23 z24 -> z24)]
                                                   ! d16
                                                   ! iN17)
                                                  (\ j18 -> nat2prob j18 + topic_prior0 ! iN17))) *
                        recip (product (nat_ 0)
                                       numDocs2
                                       (\ d25 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (dB28,()) ->
                                                                         dB28
                                                                         == wordUpdate6)
                                                                        r_nop
                                                                        (r_index (\ () -> numDocs2)
                                                                                 (\ (dB28,()) ->
                                                                                  doc4
                                                                                  ! dB28)
                                                                                 (r_add (\ (dB28,(d29,())) ->
                                                                                         nat_ 1)))))) $ \ summary27 ->
                                                 case_ summary27
                                                       [branch (ppair PVar PVar) (\ y30 z31 -> z31)]
                                                 ! d25)
                                                (\ iN26 ->
                                                 nat2prob iN26 +
                                                 summate (nat_ 0)
                                                         (size topic_prior0)
                                                         (\ dB32 -> topic_prior0 ! dB32)))) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ d33 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_split (\ (dB36,()) ->
                                                                         dB36
                                                                         == wordUpdate6)
                                                                        r_nop
                                                                        (r_index (\ () ->
                                                                                  size topic_prior0)
                                                                                 (\ (dB36,()) ->
                                                                                  z5
                                                                                  ! dB36)
                                                                                 (r_add (\ (dB36,(d37,())) ->
                                                                                         nat_ 1)))))) $ \ summary35 ->
                                                 case_ summary35
                                                       [branch (ppair PVar PVar) (\ y38 z39 -> z39)]
                                                 ! d33)
                                                (\ it34 ->
                                                 nat2prob it34 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ dB40 -> word_prior1 ! dB40))))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNewh41 ->
                                           ((let_ (bucket (nat_ 0)
                                                          (size w3)
                                                          ((r_split (\ (dB43,()) ->
                                                                     dB43
                                                                     == wordUpdate6)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (dB43,()) ->
                                                                              z5
                                                                              ! dB43)
                                                                             (r_split (\ (dB43,(zNewh44,())) ->
                                                                                       w3
                                                                                       ! wordUpdate6
                                                                                       == w3 ! dB43)
                                                                                      (r_add (\ (dB43,(zNewh44,())) ->
                                                                                              nat_ 1))
                                                                                      r_nop))))) $ \ summary42 ->
                                             case_ (case_ summary42
                                                          [branch (ppair PVar PVar)
                                                                  (\ y45 z46 -> z46)]
                                                    ! zNewh41)
                                                   [branch (ppair PVar PVar)
                                                           (\ y47 z48 -> nat2prob y47)]) +
                                            word_prior1 ! (w3 ! wordUpdate6)) *
                                           ((let_ (bucket (nat_ 0)
                                                          (size w3)
                                                          ((r_split (\ (dB50,()) ->
                                                                     dB50
                                                                     == wordUpdate6)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (dB50,()) ->
                                                                              z5
                                                                              ! dB50)
                                                                             (r_split (\ (dB50,(zNewh51,())) ->
                                                                                       doc4
                                                                                       ! wordUpdate6
                                                                                       == doc4
                                                                                          ! dB50)
                                                                                      (r_add (\ (dB50,(zNewh51,())) ->
                                                                                              nat_ 1))
                                                                                      r_nop))))) $ \ summary49 ->
                                             case_ (case_ summary49
                                                          [branch (ppair PVar PVar)
                                                                  (\ y52 z53 -> z53)]
                                                    ! zNewh41)
                                                   [branch (ppair PVar PVar)
                                                           (\ y54 z55 -> nat2prob y54)]) +
                                            topic_prior0 ! zNewh41) *
                                           recip (nat2prob (summate (nat_ 0)
                                                                    (size w3)
                                                                    (\ dB56 ->
                                                                     case_ (dB56 == wordUpdate6)
                                                                           [branch ptrue (nat_ 0),
                                                                            branch pfalse
                                                                                   (case_ (doc4
                                                                                           ! wordUpdate6
                                                                                           == doc4
                                                                                              ! dB56)
                                                                                          [branch ptrue
                                                                                                  (nat_ 1),
                                                                                           branch pfalse
                                                                                                  (nat_ 0)])])) +
                                                  summate (nat_ 0)
                                                          (size topic_prior0)
                                                          (\ dB57 -> topic_prior0 ! dB57)) *
                                           recip (nat2prob (let_ (bucket (nat_ 0)
                                                                         (size w3)
                                                                         ((r_split (\ (dB59,()) ->
                                                                                    dB59
                                                                                    == wordUpdate6)
                                                                                   r_nop
                                                                                   (r_index (\ () ->
                                                                                             size topic_prior0)
                                                                                            (\ (dB59,()) ->
                                                                                             z5
                                                                                             ! dB59)
                                                                                            (r_add (\ (dB59,(zNewh60,())) ->
                                                                                                    nat_ 1)))))) $ \ summary58 ->
                                                            case_ summary58
                                                                  [branch (ppair PVar PVar)
                                                                          (\ y61 z62 -> z62)]
                                                            ! zNewh41) +
                                                  summate (nat_ 0)
                                                          (size word_prior1)
                                                          (\ dB63 -> word_prior1 ! dB63)))))),
         branch pfalse
                (case_ (not (wordUpdate6 < size w3))
                       [branch ptrue (reject),
                        branch pfalse
                               (case_ (not (doc4 ! wordUpdate6 < numDocs2))
                                      [branch ptrue (reject), branch pfalse (reject)])])]
