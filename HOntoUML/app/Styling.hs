module Styling where

import Metamodel.UfoA
import Metamodel.UfoAInst
import Data.GraphViz.Attributes

type OUEStyle = OUElement -> Attributes
type OUEInstStyle = OUElementInst -> Attributes
type OUAStyle = OUAssoc -> Attributes
type OUAInstStyle = OUAssocInst -> Attributes

data OUStyle = OUStyle
  { oueStyle :: OUEStyle
  , oueInstStyle :: OUEInstStyle
  , ouaStyle :: OUAStyle
  , ouaInstStyle :: OUAInstStyle
  }
