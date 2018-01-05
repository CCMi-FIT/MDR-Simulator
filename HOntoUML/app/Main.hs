{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import qualified Data.Text.Lazy.IO as T
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete (Attribute(Margin), DPoint(DVal))

import Metamodel.UfoA
import Metamodel.UfoAInst
import Metamodel.UfoB
import Model.Models
import Rendering
import Styling

oueStyle1 :: OUEStyle
oueStyle1 e = stereotypeColoring <> specialStyling
  where
  stereotypeColoring = case oueStereotype e of
    Kind -> [color LightSlateBlue]
    Subkind -> [color LightSlateBlue]
    Role -> [color Orange]
    Phase -> [color Yellow]
    Relator -> [color Green]
    Collective -> [color Cyan]
    Quantity -> [color Cyan]
    Mode -> [color LightYellow]
    _ -> [color Gray]
  specialStyling = case oueName e of
    "Donor Centre" -> [Margin (DVal 0.3)]
    _ -> []

oueiStyle1 :: OUEInstStyle
oueiStyle1 (OUElementInst e _) = oueStyle1 e

ouaStyle1 :: OUAStyle
ouaStyle1 a = case ouaType a of
    OUAMediation -> [color Green]
    _ -> [color Gray20]

ouaiStyle1 :: OUAInstStyle
ouaiStyle1 (OUAssocInst a _ _) = ouaStyle1 a

ouStyle = OUStyle { oueStyle = oueStyle1, oueInstStyle = oueiStyle1, ouaStyle = ouaStyle1, ouaInstStyle = ouaiStyle1 }

main :: IO ()
main = do
  mapM_ T.putStrLn (checkConsistency ouModelInst <> checkMissingInsts ouModel ouModelB)
  renderOUModel ouModel ouStyle "dist/ufoa"
  renderOUModelInst ouModelInst ouStyle "dist/ufoa-inst"
  renderOUModelB ouModelB "dist/ufob"
