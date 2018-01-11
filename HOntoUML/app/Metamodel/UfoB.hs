{-# LANGUAGE CPP, OverloadedStrings, MultiParamTypeClasses, GADTs, FlexibleInstances #-}

module Metamodel.UfoB where

import Data.Monoid ((<>))
import qualified Data.List as DL
import Data.List (nub, (\\), find)

import Metamodel.UfoA
import Metamodel.UfoAInst

--import Debug.Trace (traceShow)

#ifndef __HASTE__
import           Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
filter' = TL.filter
#endif

data OUDisposition = OUDisposition
  { oudmText :: Maybe Text
  , oudEvents :: [OUEventB]
  } deriving (Eq)

type ECombinator = [OUElementInst] -> [OUElementInst] -> [OUElementInst]

type EOperation = [OUElementInst] -> [OUElementInst]

data OUSituation = OUSituation
  { ousName :: Text
  , ousEOperations :: [EOperation]
  , ousDispositions :: [OUDisposition]
  , ousStory :: Text
  }

mkSituation :: Text -> [EOperation] -> [OUDisposition] -> Text -> OUSituation
mkSituation n scs ds s = OUSituation
  { ousName = n
  , ousEOperations = scs
  , ousDispositions = ds
  , ousStory = s
  }

emptySituation :: OUSituation
emptySituation = mkSituation "" [] [] ""

mkMSituation ::  Text -> [EOperation] -> [OUDisposition] -> Text -> OUSituation
mkMSituation n = mkSituation ("<<merging>>\n" <> n)

instance Eq OUSituation where
  s1 == s2 = ousName s1 == ousName s2

ouSituationLabel :: OUSituation -> Text
ouSituationLabel = ousName

instance OUIdentified OUSituation where
  ouId = toIdentifier . ousName

--data BElement = BESituation OUSituation
--              | GSplit OUId
--              | GJoin OUId

--instance OUIdentified BElement where
--  ouId (BESituation s) = ouId s
--  ouId (GSplit gId) = "Gateway_" <> gId
--  ouId (GJoin gId) = "Gateway_" <> gId

data OUEventB = OUEventB
  { ouebName :: Text
  , ouebSituation :: OUSituation
  }

instance Eq OUEventB where
  e1 == e2 = ouebName e1 == ouebName e2

instance OUIdentified OUEventB where
  ouId (OUEventB lbl situation) = "e_" <> toIdentifier lbl <> ouId situation

ouEventBLabel :: OUEventB -> Text
ouEventBLabel (OUEventB lbl _) = lbl

mEventB :: Text -> OUSituation -> OUEventB
mEventB n = OUEventB ("<<merging>>\n" <> n)

newtype OUModelB = OUModelB
  { oumbSituations :: [OUSituation]
  }

oumbElementsInsts :: OUModelB -> [OUElementInst]
oumbElementsInsts ouModelB = nub $ oumsElements (oumbSituations ouModelB)

-- Combinators --

addElements :: ECombinator
addElements = mappend

removeElements :: ECombinator
removeElements = (\\)

switchPhase :: OUModelInst -> ECombinator
switchPhase ouModelInst [newPhase] eis1 = (eis1 `removeElements` getSiblingPhases newPhase) `addElements` [newPhase]
  where
  getSiblingPhases :: OUElementInst -> [OUElementInst]
  --getSiblingPhases phaseElemInst = traceShow (map oueiLabel getAllPhases) getAllPhases \\ [phaseElemInst]
  getSiblingPhases phaseElemInst = getAllPhases \\ [phaseElemInst]
    where
    getAllPhases :: [OUElementInst]
    getAllPhases = case getmSupi $ oumiGeneralizations ouModelInst of
      Nothing -> error "No phase sup found"
      Just supi -> getPhasesFromGIs $ filterGIsBySup supi (oumiGeneralizations ouModelInst)
      where
      getmSupi :: [OUGeneralizationInst] -> Maybe OUElementInst
      getmSupi gis = ougiSup <$> find (\(OUGeneralizationInst _ _ sub) -> sub == phaseElemInst) gis
      filterGIsBySup :: OUElementInst -> [OUGeneralizationInst] -> [OUGeneralizationInst]
      filterGIsBySup supi = filter (\(OUGeneralizationInst _ supi1 _) -> supi == supi1)
      getPhasesFromGIs = map ougiSub
switchPhase _ _ _ = error "Just one phase can be in switchPhase"

-- Simulation --

type ModelState = [OUSituation]

initModelState :: ModelState
initModelState = [emptySituation]

isInitial :: ModelState -> Bool
isInitial ms = length ms == 1

addSituation :: ModelState -> OUSituation -> ModelState
addSituation ms s = ms <> [s]

lastSituation :: ModelState -> OUSituation
lastSituation [] = emptySituation
lastSituation ms = DL.last ms

oumsElements :: ModelState -> [OUElementInst]
oumsElements = foldl foldSituation []
  where
  foldSituation :: [OUElementInst] -> OUSituation -> [OUElementInst]
  foldSituation insts s = foldl foldEOperation insts (ousEOperations s)
    where
    foldEOperation :: [OUElementInst] -> EOperation -> [OUElementInst]
    foldEOperation insts1 eOperation = eOperation insts1

oumsGeneralizations :: OUModelInst -> ModelState -> [OUGeneralizationInst]
oumsGeneralizations ouModelInst ms = filter (\(OUGeneralizationInst _ sup sub) -> (sup `elem` oumsElements ms) && (sub `elem` oumsElements ms)) (oumiGeneralizations ouModelInst)

oumsAssocs :: OUModelInst -> ModelState -> [OUAssocInst]
oumsAssocs ouModelInst ms = filter isSituationAssoc (oumiAssocs ouModelInst)
  where
  isSituationAssoc :: OUAssocInst -> Bool
  isSituationAssoc (OUAssocInst _ ei1 ei2) = ei1 `elem` oumsElements ms && ei2 `elem` oumsElements ms

oumsAssocsPH :: OUModelInst -> ModelState -> [OUAssocPHInst]
oumsAssocsPH ouModelInst ms = filter isSituationAssocPH (oumiAssocsPH ouModelInst)
  where
  isSituationAssocPH :: OUAssocPHInst -> Bool
  isSituationAssocPH (OUAssocPHInst _ ei1 ei2) = ei1 `elem` oumsElements ms && ei2 `elem` oumsElements ms

oumsElementsNew :: ModelState -> ModelState -> [OUElementInst]
oumsElementsNew ms1 ms2 = oumsElements ms2 \\ oumsElements ms1

oumsAssocsNew :: OUModelInst -> ModelState -> ModelState -> [OUAssocInst]
oumsAssocsNew ouModelInst ms1 ms2 = oumsAssocs ouModelInst ms2 \\ oumsAssocs ouModelInst ms1

oumsAssocsPHNew :: OUModelInst -> ModelState -> ModelState -> [OUAssocPHInst]
oumsAssocsPHNew ouModelInst ms1 ms2 = oumsAssocsPH ouModelInst ms2 \\ oumsAssocsPH ouModelInst ms1

-- Checks --
checkConsistency :: OUModelInst -> [Text]
checkConsistency ouModelInst =
  checkGeneralizations
  <> checkAssocs
  <> checkAssocsPH
  where
    checkDuplicities :: (Eq a, Show a) => Text -> [a] -> [Text]
    checkDuplicities name lst = ["!" <> name <> ": duplicity found: " <> pack (show $ map (pack . show) (lst \\ nub lst)) | nub lst /= lst]
    checkGeneralizations = concatMap checkGeneralization (oumiGeneralizations ouModelInst) <> checkDuplicities "generalizations" (oumiGeneralizations ouModelInst)
      where
      checkGeneralization :: OUGeneralizationInst -> [Text]
      checkGeneralization (OUGeneralizationInst (OUGeneralization _ eSup eSubs _) supi@(OUElementInst eiSup _) subi@(OUElementInst eiSub _)) =
        supMatch <> subMatch
        where
        supMatch :: [Text]
        supMatch = ["!Superclass mismatch: " <> oueiLabel supi | eSup /= eiSup]
        subMatch :: [Text]
        subMatch = ["!Subclass mismatch: " <> oueiLabel subi | eiSub `notElem` eSubs]
    checkAssocs = concatMap checkAssoc (oumiAssocs ouModelInst) <> checkDuplicities "associations" (oumiAssocs ouModelInst)
      where
      checkAssoc :: OUAssocInst -> [Text]
      checkAssoc (OUAssocInst (OUAssoc _ _ aId (e1, _) (e2, _)) i1@(OUElementInst ei1 _) i2@(OUElementInst ei2 _)) =
        e1Match <> e2Match
        where
        e1Match :: [Text]
        e1Match = ["!" <> aId <> " assoc start mismatch: " <> oueiLabel i1 | e1 /= ei1]
        e2Match :: [Text]
        e2Match = ["!" <> aId <> " assoc end mismatch: " <> oueiLabel i2 | e2 /= ei2]
    checkAssocsPH = concatMap checkAssocPH (oumiAssocsPH ouModelInst) <> checkDuplicities "associationsPH" (oumiAssocsPH ouModelInst)
      where
      checkAssocPH :: OUAssocPHInst -> [Text]
      checkAssocPH (OUAssocPHInst (OUAssocPH _ aId (e1, _) (e2, _) _) i1@(OUElementInst ei1 _) i2@(OUElementInst ei2 _)) =
        e1Match <> e2Match
        where
        e1Match :: [Text]
        e1Match = ["!" <> aId <> " assocPH start mismatch: " <> oueiLabel i1 | e1 /= ei1]
        e2Match :: [Text]
        e2Match = ["!" <> aId <> " assocPH mismatch: " <> oueiLabel i2 | e2 /= ei2]

checkMissingInsts :: OUModel -> OUModelB -> [Text]
checkMissingInsts ouModel ouModelB = checkMissingElemInsts
  where
    checkMissingElemInsts = concatMap checkElemInst (oumElements ouModel)
      where
      checkElemInst :: OUElement -> [Text]
      checkElemInst e = ["Warning: No situation contains instance of " <> ouId e | e `notElem` map (\(OUElementInst e1 _) -> e1) (oumbElementsInsts ouModelB)]
