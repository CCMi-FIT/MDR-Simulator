{-# LANGUAGE CPP, OverloadedStrings, MultiParamTypeClasses, GADTs, FlexibleInstances #-}

module Metamodel.UfoAInst where

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (find, nub)

import Metamodel.UfoA

#ifndef __HASTE__
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
filter' = TL.filter
#endif

--import Debug.Trace (traceShow)

data OUElementInst = OUElementInst OUElement (Maybe Text) deriving (Show, Eq)

instance OUIdentified OUElementInst where
  ouId = toIdentifier . oueiLabel

oueiLabel :: OUElementInst -> Text
oueiLabel (OUElementInst e mLbl) = fromMaybe "" mLbl <> ": " <> oueName e

data OUGeneralizationInst = OUGeneralizationInst
  { ougiGeneralization :: OUGeneralization
  , ougiSup :: OUElementInst
  , ougiSub :: OUElementInst
  } deriving (Show, Eq)

instance OUIdentified OUGeneralizationInst where
  ouId (OUGeneralizationInst g supi subi) = "gi_" <> ouId g <> ouId supi <> ouId subi

data OUAssocInst = OUAssocInst OUAssoc OUElementInst OUElementInst deriving (Show, Eq)

instance OUIdentified OUAssocInst where
  ouId (OUAssocInst _ i1 i2) = "Assoc_" <> ouId i1 <> ouId i2

data OUAssocPHInst = OUAssocPHInst OUAssocPH OUElementInst OUElementInst deriving (Show, Eq)

instance OUIdentified OUAssocPHInst where
  ouId (OUAssocPHInst _ i1 i2) = "AssocPH_" <> ouId i1 <> ouId i2

data OUModelInst = OUModelInst
  { oumiGeneralizationClusters :: [[OUGeneralizationInst]]
  , oumiAssocs :: [OUAssocInst]
  , oumiAssocsPH :: [OUAssocPHInst]
  }

oumiGeneralizations :: OUModelInst -> [OUGeneralizationInst]
oumiGeneralizations = concat . oumiGeneralizationClusters

oumiElements :: OUModelInst -> [OUElementInst]
oumiElements ouModelInst =
  nub $
    concatMap (\(OUGeneralizationInst _ sup sub) -> [sup, sub]) (oumiGeneralizations ouModelInst)
    <> concatMap (\(OUAssocInst _ i1 i2) -> [i1, i2]) (oumiAssocs ouModelInst)
    <> concatMap (\(OUAssocPHInst _ i1 i2) -> [i1, i2]) (oumiAssocsPH ouModelInst)

instance Monoid OUModelInst where
  mempty = OUModelInst
    { oumiGeneralizationClusters = []
    , oumiAssocs = []
    , oumiAssocsPH = []
    }
  mappend mi1 mi2 = OUModelInst
    { oumiGeneralizationClusters = oumiGeneralizationClusters mi1 <> oumiGeneralizationClusters mi2
    , oumiAssocs = oumiAssocs mi1 <> oumiAssocs mi2
    , oumiAssocsPH = oumiAssocsPH mi1 <> oumiAssocsPH mi2
    }

-- Queries --

isSuperClass :: OUModel -> OUElement -> Bool
isSuperClass ouModel ouElement = not $ null $ find (\(OUGeneralization _ sup _ _) -> sup == ouElement) (oumGeneralizations ouModel)

isSuperClassInst :: OUModel -> OUElementInst -> Bool
isSuperClassInst ouModel (OUElementInst e _) = isSuperClass ouModel e

hasSuperClass :: OUModel -> OUElement -> Bool
hasSuperClass ouModel ouElement = not $ null $ find (\(OUGeneralization _ _ subs _) -> ouElement `elem` subs) (oumGeneralizations ouModel)

hasSuperClassInst :: OUModel -> OUElementInst -> Bool
hasSuperClassInst ouModel (OUElementInst ouElement _) = hasSuperClass ouModel ouElement

subClasses :: OUModel -> OUElement -> [OUElement]
subClasses ouModel ouElement = concatMap (\(OUGeneralization _ sup subs _) -> if sup == ouElement then subs else []) (oumGeneralizations ouModel)

getLeafSubClass :: OUModel -> OUElement -> OUElement
getLeafSubClass ouModel e
  | null $ subClasses ouModel e = e
  | otherwise = getLeafSubClass ouModel (head $ subClasses ouModel e)

getLeafSubClassInst :: OUModel -> OUModelInst -> OUElementInst -> OUElementInst
getLeafSubClassInst ouModel ouModelInst (OUElementInst e _) = head $ getInstancesOfClass ouModelInst $ getLeafSubClass ouModel e

getInstanceByName :: OUModelInst -> Text -> Maybe OUElementInst
getInstanceByName ouModelInst lbl = find (\(OUElementInst _ mLbl) -> fromMaybe "" mLbl == lbl) (oumiElements ouModelInst)

getInstancesOfClass :: OUModelInst -> OUElement -> [OUElementInst]
getInstancesOfClass ouModelInst ouElement = filter (\(OUElementInst e _) -> e == ouElement) (oumiElements ouModelInst)

--subClassesInsts :: OUModelInst -> OUElementInst -> [OUElementInst]
--subClassesInsts ouModelInst i = concatMap (\(OUGeneralizationInst _ sup subs) -> if sup == i then subs else []) (oumiGeneralizations ouModelInst)

