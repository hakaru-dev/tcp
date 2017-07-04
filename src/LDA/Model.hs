{-# LANGUAGE DataKinds, NegativeLiterals #-}
module LDA.Model where

import           Data.Number.LogFloat (LogFloat)
import           Prelude hiding (product, exp, log, (**))
import           Language.Hakaru.Runtime.LogFloatPrelude

import           Language.Hakaru.Runtime.CmdLine
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

prog ::
  ((MayBoxVec Prob Prob) ->
   ((MayBoxVec Prob Prob) ->
    (Int ->
     ((MayBoxVec Int Int) ->
      ((MayBoxVec Int Int) ->
       ((MayBoxVec Int Int) -> (Int -> (Measure Int))))))))
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
                                                         (\ dB40 -> word_prior1 ! dB40)))) *
                        recip (nat2prob (summate (nat_ 0)
                                                 (size w3)
                                                 (\ dB41 ->
                                                  case_ (dB41 == wordUpdate6)
                                                        [branch ptrue (nat_ 0),
                                                         branch pfalse
                                                                (case_ (not (z5 ! dB41 < nat_ 0) &&
                                                                        doc4 ! wordUpdate6
                                                                        == doc4 ! dB41)
                                                                       [branch ptrue (nat_ 1),
                                                                        branch pfalse
                                                                               (nat_ 0)])])) +
                               summate (nat_ 0)
                                       (size topic_prior0)
                                       (\ dB42 -> topic_prior0 ! dB42))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNewh43 ->
                                           ((let_ (bucket (nat_ 0)
                                                          (size w3)
                                                          ((r_split (\ (dB45,()) ->
                                                                     dB45
                                                                     == wordUpdate6)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (dB45,()) ->
                                                                              z5
                                                                              ! dB45)
                                                                             (r_split (\ (dB45,(zNewh46,())) ->
                                                                                       w3
                                                                                       ! wordUpdate6
                                                                                       == w3 ! dB45)
                                                                                      (r_add (\ (dB45,(zNewh46,())) ->
                                                                                              nat_ 1))
                                                                                      r_nop))))) $ \ summary44 ->
                                             case_ (case_ summary44
                                                          [branch (ppair PVar PVar)
                                                                  (\ y47 z48 -> z48)]
                                                    ! zNewh43)
                                                   [branch (ppair PVar PVar)
                                                           (\ y49 z50 -> nat2prob y49)]) +
                                            word_prior1 ! (w3 ! wordUpdate6)) *
                                           ((let_ (bucket (nat_ 0)
                                                          (size w3)
                                                          ((r_split (\ (dB52,()) ->
                                                                     dB52
                                                                     == wordUpdate6)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (dB52,()) ->
                                                                              z5
                                                                              ! dB52)
                                                                             (r_split (\ (dB52,(zNewh53,())) ->
                                                                                       doc4
                                                                                       ! wordUpdate6
                                                                                       == doc4
                                                                                          ! dB52)
                                                                                      (r_add (\ (dB52,(zNewh53,())) ->
                                                                                              nat_ 1))
                                                                                      r_nop))))) $ \ summary51 ->
                                             case_ (case_ summary51
                                                          [branch (ppair PVar PVar)
                                                                  (\ y54 z55 -> z55)]
                                                    ! zNewh43)
                                                   [branch (ppair PVar PVar)
                                                           (\ y56 z57 -> nat2prob y56)]) +
                                            topic_prior0 ! zNewh43) *
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
                                                            ! zNewh43) +
                                                  summate (nat_ 0)
                                                          (size word_prior1)
                                                          (\ dB63 -> word_prior1 ! dB63)))))),
         branch pfalse
                (case_ (not (wordUpdate6 < size w3))
                       [branch ptrue (reject),
                        branch pfalse
                               (case_ (not (doc4 ! wordUpdate6 < numDocs2))
                                      [branch ptrue (reject), branch pfalse (reject)])])]
