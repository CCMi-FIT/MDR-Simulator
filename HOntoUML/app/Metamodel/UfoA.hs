{-# LANGUAGE CPP, OverloadedStrings, MultiParamTypeClasses, GADTs, FlexibleInstances #-}

module Metamodel.UfoA where

import Data.Monoid ((<>))
import Data.List (nub)

--import Debug.Trace (traceShow)

#ifdef __HASTE__
type Text = String
pack :: a -> a
pack = id
unpack :: a -> a
unpack = id
filter' = filter
#else
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
filter' = TL.filter
#endif

type OUEStereotype = Text
type OUEName = Text
type OUELabel = Text
type OUId = Text
type URL = Text

class OUIdentified e where
  ouId :: e -> OUId

toIdentifier :: Text -> OUId
toIdentifier t = filter' (\ch -> (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_' || ch == '-') t

data OUElementType = Kind | Subkind | Role | Phase | Relator | Mode | Quality | Quantity | Collective | Event deriving (Show, Eq)

ouElementTypePrint :: OUElementType -> Text
ouElementTypePrint Kind = "<<kind>>"
ouElementTypePrint Subkind = "<<subkind>>"
ouElementTypePrint Role = "<<role>>"
ouElementTypePrint Phase = "<<phase>>"
ouElementTypePrint Relator = "<<relator>>"
ouElementTypePrint Mode = "<<mode>>"
ouElementTypePrint Quality = "<<quality>>"
ouElementTypePrint Quantity = "<<quantity>>"
ouElementTypePrint Collective = "<<collective>>"
ouElementTypePrint Event = "<<event>>"

data OUElement = OUElement
  { oueStereotype :: OUElementType
  , oueName :: OUEName
  } deriving (Show, Eq)

instance OUIdentified OUElement where
  ouId e = toIdentifier $ oueName e

ouePrint :: OUElement -> Text
ouePrint e = ouElementTypePrint (oueStereotype e) <> " " <> oueName e

oueLabel :: OUElement -> Text
oueLabel e = ouElementTypePrint (oueStereotype e) <> "\n" <> oueName e

-- Elements --

mkKind :: OUEName -> OUElement
mkKind = OUElement Kind

mkSubKind :: OUEName -> OUElement
mkSubKind = OUElement Subkind

mkRole :: OUEName -> OUElement
mkRole = OUElement Role

mkPhase :: OUEName -> OUElement
mkPhase = OUElement Phase

mkRelator :: OUEName -> OUElement
mkRelator = OUElement Relator

mkMode :: OUEName -> OUElement
mkMode = OUElement Mode

mkQuality :: OUEName -> OUElement
mkQuality = OUElement Quality

mkQuantity :: OUEName -> OUElement
mkQuantity = OUElement Quantity

mkCollective :: OUEName -> OUElement
mkCollective = OUElement Collective

mkEvent :: OUEName -> OUElement
mkEvent = OUElement Event

-- Generalization --

data OUGeneralizationType = PlainGT | Disjoint | Complete | DisjointComplete deriving (Show, Eq)

ougtLabel :: OUGeneralizationType -> Text
ougtLabel PlainGT = ""
ougtLabel Disjoint = "{disjoint}"
ougtLabel Complete = "{complete}"
ougtLabel DisjointComplete = "{disjoint, complete}"

data OUGeneralization = OUGeneralization
  { ougId :: OUId
  , ougSup :: OUElement
  , ougSubs :: [OUElement]
  , ougType :: OUGeneralizationType
  } deriving (Show, Eq)

instance OUIdentified OUGeneralization where
  ouId = ougId

mkGeneralization :: OUId -> OUElement -> [OUElement] -> OUGeneralizationType -> OUGeneralization
mkGeneralization = OUGeneralization

ougLabel :: OUGeneralization -> Text
ougLabel g = ouId g <> ougtLabel (ougType g)

-- Association --

type OUMultiplicity = Text
type OUConnection = (OUElement, OUMultiplicity)

ouConnElement :: OUConnection -> OUElement
ouConnElement = fst

data OUAssocType = OUAMediation | OUACharacterization | OUContainment | OUAOther OUEStereotype deriving (Show, Eq)

printAType :: OUAssocType -> Text
printAType OUAMediation = "<<mediation>>"
printAType OUACharacterization = "<<characterizaton>>"
printAType OUContainment = "<<containment>>"
printAType (OUAOther t) = t

data OUAssoc = OUAssoc
  { ouaType :: OUAssocType
  , ouamLabel :: Maybe Text
  , ouaId :: Text
  , ouaConnection1 :: OUConnection
  , ouaConnection2 :: OUConnection
  } deriving (Show, Eq)

instance OUIdentified OUAssoc where
  ouId (OUAssoc _ _ _ (e1, _) (e2, _)) = "Assoc_" <> ouId e1 <> ouId e2

mkAssoc :: Maybe Text -> Text -> (OUElement, OUMultiplicity) -> (OUElement, OUMultiplicity) -> OUAssoc
mkAssoc = OUAssoc (OUAOther "")

mkMediation :: Text -> OUConnection -> OUConnection -> OUAssoc
mkMediation = OUAssoc OUAMediation Nothing

mkCharacterization :: Text -> OUConnection -> OUConnection -> OUAssoc
mkCharacterization = OUAssoc OUACharacterization Nothing

mkContainment :: Text -> OUConnection -> OUConnection -> OUAssoc
mkContainment = OUAssoc OUContainment Nothing

-- Part-Whole Relations

type OUAggregType = Text

data OUPHMeta = PlainPH | Essential | Inseparable | EssentialInseparable deriving (Show, Eq)

ouphMetaLabel :: OUPHMeta -> Text
ouphMetaLabel PlainPH = ""
ouphMetaLabel Essential = "{essential}"
ouphMetaLabel Inseparable = "{inseparable}"
ouphMetaLabel EssentialInseparable = "{essential, inseparable}"

data OUAssocPH = OUAssocPH
  { ouAggregType :: OUAggregType
  , ouaphId :: Text
  , ouaphConnection1 :: OUConnection
  , ouaphConnection2 :: OUConnection
  , ouaphMeta :: OUPHMeta
  } deriving (Show, Eq)

instance OUIdentified OUAssocPH where
  ouId assocPH = "AssocPH_" <> ouId (ouConnElement $ ouaphConnection1 assocPH) <> ouId (ouConnElement $ ouaphConnection2 assocPH)

mkMemberOf :: Text -> OUConnection -> OUConnection -> OUPHMeta -> OUAssocPH
mkMemberOf = OUAssocPH "M"

-- Model --

data OUModel = OUModel
  { oumGeneralizationClusters :: [[OUGeneralization]]
  , oumAssocs :: [OUAssoc]
  , oumAssocsPH :: [OUAssocPH]
  }

oumGeneralizations :: OUModel -> [OUGeneralization]
oumGeneralizations = concat . oumGeneralizationClusters

oumElements :: OUModel -> [OUElement]
oumElements ouModel =
  nub $
    concatMap (\(OUGeneralization _ sup subs _) -> [sup] <> subs) (oumGeneralizations ouModel)
    <> concatMap (\(OUAssoc _ _ _ (e1, _) (e2, _)) -> [e1, e2]) (oumAssocs ouModel)
    <> concatMap (\(OUAssocPH _ _ (e1, _) (e2, _) _) -> [e1, e2]) (oumAssocsPH ouModel)

instance Monoid OUModel where
  mempty = OUModel
    { oumGeneralizationClusters = []
    , oumAssocs = []
    , oumAssocsPH = []
    }
  mappend m1 m2 = OUModel
    { oumGeneralizationClusters = oumGeneralizationClusters m1 <> oumGeneralizationClusters m2
    , oumAssocs = oumAssocs m1 <> oumAssocs m2
    , oumAssocsPH = oumAssocsPH m1 <> oumAssocsPH m2
    }
