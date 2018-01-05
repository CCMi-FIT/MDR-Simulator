{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Rendering where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import Control.Monad (mapM_)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete (Attribute(..), EdgeType(..), Label(StrLabel), DPoint(..), ArrowType(..), ArrowShape(..), openMod, DPoint(..))
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Commands

import Metamodel.UfoA
import Metamodel.UfoAInst
import Metamodel.UfoB
import Styling

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

type OUNode = Text
type OUDot = Dot OUNode
type OUDotM = DotM OUNode ()

-- Elements --

renderElement :: OUEStyle -> OUElement -> OUDot
renderElement oueStyle e = node (oueLabel e) ([ID (ouId e), Margin (DVal 0.2)] <> oueStyle e)

--renderElementInst :: OUModel -> OUModelInst -> OUEInstStyle -> OUElementInst -> OUDot
--renderElementInst ouModel ouModelInst oueInstStyle i@(OUElementInst e _)
--  | isSuperClass ouModel e =
--    cluster (Str $ ouId i) $ do
--      graphAttrs [ID (ouId i), lbl, Tooltip (ouElementTypePrint $ oueStereotype e), color Blue, PenWidth 2, Margin $ DVal 16]
--      mapM_ (renderElementInst ouModel ouModelInst oueInstStyle) (subClassesInsts ouModelInst i)
--  | otherwise = node (oueiLabel i) ([ID (ouId i), lbl, Tooltip (ouElementTypePrint $ oueStereotype e)] <> oueInstStyle i)
--  where
--    lbl = toLabel $ H.Text [H.Format H.Underline [H.Str $ oueiLabel i]]

renderElementInst :: OUEInstStyle -> OUElementInst -> OUDot
renderElementInst oueInstStyle i@(OUElementInst e _) = node (oueiLabel i) ([ID (ouId i), lbl, Tooltip (ouElementTypePrint $ oueStereotype e)] <> oueInstStyle i)
  where
  lbl = toLabel $ H.Text [H.Format H.Underline [H.Str $ oueiLabel i]]

-- Generalization --

gLineStyle :: Attributes
gLineStyle = [PenWidth 3, color Gray]

renderGeneralization :: OUGeneralization -> OUDot
renderGeneralization g@(OUGeneralization _ sup subs _) = mapM_ (\sub -> edge (oueLabel sup) (oueLabel sub) ([textLabel (ougLabel g), edgeEnds Back] <> gLineStyle)) subs

renderGeneralizationInstCluster :: [OUGeneralizationInst] -> OUDot
renderGeneralizationInstCluster gis = mapM_ (\gi@(OUGeneralizationInst _ supi subi) -> edge (oueiLabel supi) (oueiLabel subi) ([ID (ouId gi), Tooltip (ouId gi), edgeEnds Back] <> gLineStyle)) gis

-- Assocs ---

mLabel2Label Nothing = ""
mLabel2Label (Just lbl) = "\n" <> lbl

renderAssoc :: OUAStyle -> OUAssoc -> OUDot
renderAssoc ouaStyle a@(OUAssoc aType _ aId (e1, m1) (e2, m2)) =
  edge (oueLabel e1) (oueLabel e2) ([ID (ouId a), textLabel $ printAType aType <> mLabel2Label (ouamLabel a), Tooltip aId, HeadLabel (StrLabel m2), TailLabel (StrLabel m1), arrowTo noArrow] <> ouaStyle a)

renderAssocPH :: OUAssocPH -> OUDot
renderAssocPH a@(OUAssocPH aggregType aId (e1, m1) (e2, m2) phMeta) =
  edge (oueLabel e1) (oueLabel e2) [ID (ouId a), textLabel (ouphMetaLabel phMeta), Tooltip aId, TailLabel (StrLabel $ m1 <> "<" <> aggregType <> ">"), HeadLabel (StrLabel m2), arrowFrom diamond, edgeEnds Back, color Gray20]

--renderClippedEdge :: OUModel -> OUModelInst -> OUElementInst -> OUElementInst -> Attributes -> OUDot
--renderClippedEdge ouModel ouModelInst i1 i2 attrs = edge dot1 dot2 (attrs <> [LTail ltail, LHead lhead])
--  where
--    -- handle GraphViz's shit: https://stackoverflow.com/questions/2012036/graphviz-how-to-connect-subgraphs
--    clippingNeeded = isSuperClassInst ouModel
--    dot1 = if clippingNeeded i1
--      then oueiLabel $ getLeafSubClassInst ouModel ouModelInst i1
--      else oueiLabel i1
--    dot2 = if clippingNeeded i2
--      then oueiLabel $ getLeafSubClassInst ouModel ouModelInst i2
--      else oueiLabel i2
--    lhead = if clippingNeeded i2 then "cluster_" <> oueiLabel i2 else ""
--    ltail = if clippingNeeded i1 then "cluster_" <> oueiLabel i1 else ""

renderAssocInst :: OUAInstStyle -> OUAssocInst -> OUDot
renderAssocInst ouaiStyle ai@(OUAssocInst assoc i1 i2) =
  --renderClippedEdge ouModel ouModelInst i1 i2 [ID (ouId ai), textLabel $ mLabel2Label $ ouamLabel assoc, Tooltip (ouaId assoc), HeadLabel (StrLabel m2), TailLabel (StrLabel m1), arrowTo noArrow]
  edge (oueiLabel i1) (oueiLabel i2) ([ID (ouId ai), textLabel $ mLabel2Label (ouamLabel assoc), Tooltip (ouaId assoc), arrowTo noArrow] <> ouaiStyle ai)

renderAssocPHInst ::OUAssocPHInst -> OUDot
renderAssocPHInst ai@(OUAssocPHInst assocPH i1 i2) =
  --renderClippedEdge ouModel ouModelInst i1 i2 [ID (ouId ai), Tooltip (ouaphId assocPH), TailLabel (StrLabel $ m1 <> "<" <> aggregType <> ">"), HeadLabel (StrLabel m2), arrowFrom diamond, edgeEnds Back]
  edge (oueiLabel i1) (oueiLabel i2) [ID (ouId ai), Tooltip (ouaphId assocPH), TailLabel (StrLabel $ "<" <> ouAggregType assocPH <> ">"), arrowFrom diamond, edgeEnds Back]

-- UFO-B --

--renderGateway :: OUId -> OUDot
--renderGateway gId = node gId [textLabel "", ID gId, shape BoxShape, Height 0.1, style filled, fillColor Black]

renderSituation :: OUSituation -> OUDot
renderSituation s = do
  node (ouId s) [textLabel $ ouSituationLabel s, ID (ouId s), shape BoxShape]
  mapM_ rDisposition $ zip [1..] (ousDispositions s)
    where
    rDisposition :: (Integer, OUDisposition) -> OUDot
    rDisposition (dNo, disp) = case oudmText disp of
      Nothing -> mapM_ (rEvent $ ouId s) (oudEvents disp)
      Just dText -> do
        node dId [textLabel dText, shape DiamondShape, Margin (DVal 0)]
        edge (ouId s) dId [arrowTo noArrow]
        mapM_ (rEvent dId) (oudEvents disp)
      where
      dId = ouId s <> "d" <> TL.pack (show dNo)
      rEvent :: OUId -> OUEventB -> OUDot
      rEvent dId1 e@(OUEventB _ s2) = do
      --  if TL.take 5 (ouEventBLabel e) == "merge" then
      --    node (ouId e) [textLabel "", shape BoxShape, Height 0.1, style filled, fillColor Black]
      --  else
        node (ouId e) [textLabel $ ouEventBLabel e, shape Ellipse, Margin (DVal 0)]
        edge dId1 (ouId e) [ArrowHead $ AType [(openMod, Vee)]]
        edge (ouId e) (ouId s2) [ArrowHead $ AType [(openMod, Vee)]]

-- Model --

fontStyle = [FontName "Helvetica Neue, Helvetica, sans-serif", FontSize 9]

renderModel :: OUModel -> OUStyle -> DotGraph OUNode
renderModel ouModel OUStyle{ oueStyle, ouaStyle } = digraph (Str "GA") $ do
  graphAttrs [Splines PolyLine, NodeSep 1.0, RankSep [1.0]]
  nodeAttrs $ [style filled, color LightGray, shape BoxShape] <> fontStyle
  edgeAttrs fontStyle
  mapM_ (renderElement oueStyle) (oumElements ouModel)
  mapM_ renderGeneralizationCluster (oumGeneralizationClusters ouModel)
  mapM_ (renderAssoc ouaStyle) (oumAssocs ouModel)
  mapM_ renderAssocPH (oumAssocsPH ouModel)
    where
    renderGeneralizationCluster :: [OUGeneralization] -> OUDot
    renderGeneralizationCluster generalizations =
   --   cluster (Str $ "G_" <> (ouId $ ougSup $ head generalizations)) $
        mapM_ renderGeneralization generalizations

renderModelInst ::OUModelInst -> OUStyle -> DotGraph OUNode
renderModelInst ouModelInst OUStyle{ oueInstStyle, ouaInstStyle } = digraph (Str "GI") $ do
  graphAttrs [Splines PolyLine, Compound True, NodeSep 0.5, RankSep [1], Height 0.05]
  nodeAttrs $ [style filled, color LightGray, shape BoxShape] <> fontStyle
  edgeAttrs fontStyle
  mapM_ (renderElementInst oueInstStyle) (oumiElements ouModelInst)
  mapM_ renderGeneralizationInstCluster (oumiGeneralizationClusters ouModelInst)
  mapM_ (renderAssocInst ouaInstStyle) (oumiAssocs ouModelInst)
  mapM_ renderAssocPHInst (oumiAssocsPH ouModelInst)

renderModelB :: OUModelB -> DotGraph OUNode
renderModelB ouModelB = digraph (Str "GB") $ do
  graphAttrs [Splines PolyLine]
  nodeAttrs fontStyle
  edgeAttrs fontStyle
  mapM_ renderSituation (oumbSituations ouModelB)

generateOutput :: DotGraph OUNode -> String -> IO ()
generateOutput gr fname = do
  _ <- runGraphvizCommand Dot gr Svg (fname <> ".svg")
  _ <- runGraphvizCommand Dot gr DotOutput (fname <> ".dot")
  return ()

renderOUModel :: OUModel -> OUStyle -> String -> IO ()
renderOUModel ouModel ouStyle = generateOutput (renderModel ouModel ouStyle)

renderOUModelInst :: OUModelInst -> OUStyle -> String -> IO ()
renderOUModelInst ouModelInst ouStyle = generateOutput (renderModelInst ouModelInst ouStyle)

renderOUModelB :: OUModelB -> String -> IO ()
renderOUModelB ouModelB = generateOutput (renderModelB ouModelB)


