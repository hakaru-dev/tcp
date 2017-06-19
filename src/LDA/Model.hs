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
  case_ (wordUpdate6 < size w3)
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
                                           product (nat_ 0)
                                                   (size word_prior1)
                                                   (\ d42 ->
                                                    case_ (d42 == w3 ! wordUpdate6 &&
                                                           not (nat2int (size word_prior1) + int_ -1
                                                                < nat2int (w3 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (dB44,()) ->
                                                                                                     dB44
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              size word_prior1)
                                                                                                             (\ (dB44,()) ->
                                                                                                              w3
                                                                                                              ! dB44)
                                                                                                             (r_index (\ (d45,()) ->
                                                                                                                       size topic_prior0)
                                                                                                                      (\ (dB44,(d45,())) ->
                                                                                                                       z5
                                                                                                                       ! dB44)
                                                                                                                      (r_add (\ (dB44,(zNewh46,(d45,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary43 ->
                                                                             case_ summary43
                                                                                   [branch (ppair PVar
                                                                                                  PVar)
                                                                                           (\ y47
                                                                                              z48 ->
                                                                                            z48)]
                                                                             ! d42
                                                                             ! zNewh41) +
                                                                   word_prior1 ! d42),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d42
                                                                                         == w3
                                                                                            ! wordUpdate6))
                                                                                   [branch ptrue
                                                                                           (nat_ 1),
                                                                                    branch pfalse
                                                                                           (nat_ 1)]))]) *
                                           product (nat_ 0)
                                                   numDocs2
                                                   (\ d49 ->
                                                    case_ (d49 == doc4 ! wordUpdate6 &&
                                                           not (nat2int numDocs2 + int_ -1
                                                                < nat2int (doc4 ! wordUpdate6)))
                                                          [branch ptrue
                                                                  (nat2prob (let_ (bucket (nat_ 0)
                                                                                          (size w3)
                                                                                          ((r_split (\ (dB51,()) ->
                                                                                                     dB51
                                                                                                     == wordUpdate6)
                                                                                                    r_nop
                                                                                                    (r_index (\ () ->
                                                                                                              numDocs2)
                                                                                                             (\ (dB51,()) ->
                                                                                                              doc4
                                                                                                              ! dB51)
                                                                                                             (r_index (\ (d52,()) ->
                                                                                                                       size topic_prior0)
                                                                                                                      (\ (dB51,(d52,())) ->
                                                                                                                       z5
                                                                                                                       ! dB51)
                                                                                                                      (r_add (\ (dB51,(zNewh53,(d52,()))) ->
                                                                                                                              nat_ 1))))))) $ \ summary50 ->
                                                                             case_ summary50
                                                                                   [branch (ppair PVar
                                                                                                  PVar)
                                                                                           (\ y54
                                                                                              z55 ->
                                                                                            z55)]
                                                                             ! d49
                                                                             ! zNewh41) +
                                                                   topic_prior0 ! zNewh41),
                                                           branch pfalse
                                                                  (nat2prob (case_ (not (d49
                                                                                         == doc4
                                                                                            ! wordUpdate6))
                                                                                   [branch ptrue
                                                                                           (nat_ 1),
                                                                                    branch pfalse
                                                                                           (nat_ 1)]))]) *
                                           recip (product (nat_ 0)
                                                          numDocs2
                                                          (\ d56 ->
                                                           case_ (d56 == doc4 ! wordUpdate6 &&
                                                                  not (nat2int numDocs2 + int_ -1
                                                                       < nat2int (doc4
                                                                                  ! wordUpdate6)))
                                                                 [branch ptrue
                                                                         (nat2prob (let_ (bucket (nat_ 0)
                                                                                                 (size w3)
                                                                                                 ((r_split (\ (dB58,()) ->
                                                                                                            dB58
                                                                                                            == wordUpdate6)
                                                                                                           r_nop
                                                                                                           (r_index (\ () ->
                                                                                                                     numDocs2)
                                                                                                                    (\ (dB58,()) ->
                                                                                                                     doc4
                                                                                                                     ! dB58)
                                                                                                                    (r_add (\ (dB58,(d59,())) ->
                                                                                                                            nat_ 1)))))) $ \ summary57 ->
                                                                                    case_ summary57
                                                                                          [branch (ppair PVar
                                                                                                         PVar)
                                                                                                  (\ y60
                                                                                                     z61 ->
                                                                                                   z61)]
                                                                                    ! d56) +
                                                                          summate (nat_ 0)
                                                                                  (size topic_prior0)
                                                                                  (\ dB62 ->
                                                                                   topic_prior0
                                                                                   ! dB62)),
                                                                  branch pfalse
                                                                         (nat2prob (case_ (not (d56
                                                                                                == doc4
                                                                                                   ! wordUpdate6))
                                                                                          [branch ptrue
                                                                                                  (nat_ 1),
                                                                                           branch pfalse
                                                                                                  (nat_ 1)]))])) *
                                           recip (nat2prob (let_ (bucket (nat_ 0)
                                                                         (size w3)
                                                                         ((r_split (\ (dB64,()) ->
                                                                                    dB64
                                                                                    == wordUpdate6)
                                                                                   r_nop
                                                                                   (r_index (\ () ->
                                                                                             size topic_prior0)
                                                                                            (\ (dB64,()) ->
                                                                                             z5
                                                                                             ! dB64)
                                                                                            (r_add (\ (dB64,(zNewh65,())) ->
                                                                                                    nat_ 1)))))) $ \ summary63 ->
                                                            case_ summary63
                                                                  [branch (ppair PVar PVar)
                                                                          (\ y66 z67 -> z67)]
                                                            ! zNewh41) +
                                                  summate (nat_ 0)
                                                          (size word_prior1)
                                                          (\ dB68 -> word_prior1 ! dB68)))))),
         branch pfalse (reject)]
