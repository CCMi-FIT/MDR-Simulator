{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Data.List as DL
import Data.Monoid ((<>))
import Data.IORef
import Control.Monad (unless)

import           Haste.Foreign (export)

import JQuery
import Metamodel.UfoA
import Metamodel.UfoAInst
import Metamodel.UfoB
import Model.Models

hlCol :: String
hlCol = "#9DDD17"

normCol :: String
normCol = "#000000"

bgCol :: String
bgCol = "#ffffff"

visitedCol :: String
visitedCol = "#DAF2A9"

diagAJq :: IO JQuery
diagAJq = selectById "ufoa-inst-svg"

diagBJq :: IO JQuery
diagBJq = selectById "ufob-svg"

getAShape :: OUIdentified i => i -> IO JQuery
getAShape i = --do
  diagAJq >>= selectSVG ("#" <> ouId i)

getARect :: OUIdentified i => i -> IO JQuery
getARect i = diagAJq >>= selectSVG ("#" <> ouId i <> " polygon")

getALine :: OUIdentified i => i -> IO JQuery
getALine i = diagAJq >>= selectSVG ("#" <> ouId i <> " path")

getGroup :: OUSituation -> IO JQuery
getGroup s = diagBJq >>= selectSVG ("#" <> ouId s)

getBShape :: OUSituation -> IO JQuery
getBShape s = diagBJq >>= selectSVG ("#" <> ouId s <> " > polygon")

startLoader :: IO ()
startLoader = do
  _ <- selectById "loader" >>= appearJq
  return ()

stopLoader :: IO ()
stopLoader = do
  _ <- selectById "loader" >>= disappearJq
  return ()

-- Managing model state --------------------------------------------

type ModelStateRef = IORef ModelState

initModelStateRef :: IO ModelStateRef
initModelStateRef = newIORef initModelState

readModelState :: ModelStateRef -> IO ModelState
readModelState = readIORef

writeModelState :: ModelStateRef -> ModelState -> IO ()
writeModelState = writeIORef

gotoSituation :: OUSituation -> ModelStateRef -> IO ()
gotoSituation s2 msRef = do
  ms1 <- readModelState msRef
  unless (lastMsSituation ms1 == s2) (do
    --startLoader
    let ms2 = addSituation ouModelInst ms1 s2
    mapM_ hidePart (oumiElements ouModelInst)
    mapM_ hidePart (oumiGeneralizations ouModelInst)
    mapM_ hidePart (oumiAssocs ouModelInst)
    mapM_ hidePart (oumiAssocsPH ouModelInst)
    mapM_ showPart (msElements ms2)
    mapM_ showPart (msGeneralizations ms2)
    mapM_ showPart (msAssocs ms2)
    mapM_ showPart (msAssocsPH ms2)
    mapM_ lowlightElem (msElements ms1)
    mapM_ highlightElem (msElementsNew ms1 ms2)
    mapM_ lowlightAssoc (msAssocs ms1)
    mapM_ highlightAssoc (msAssocsNew ms1 ms2)
    mapM_ lowlightAssoc (msAssocsPH ms1)
    mapM_ highlightAssoc (msAssocsPHNew ms1 ms2)
    _ <- setStory
    writeModelState msRef ms2
    --stopLoader
    )
      where
      hidePart :: OUIdentified ie => ie -> IO JQuery
      hidePart ie = getAShape ie >>= hideJq
      showPart :: OUIdentified ie => ie -> IO JQuery
      showPart ie = getAShape ie >>= showJq
      lowlightElem :: OUIdentified ie => ie -> IO JQuery
      lowlightElem ie = getARect ie >>= setAttr "stroke" "white" >>= setAttr "stroke-width" "0"
      highlightElem :: OUIdentified ie => ie -> IO JQuery
      highlightElem ie = getARect ie >>= setAttr "stroke" "red" >>= setAttr "stroke-width" "3"
      lowlightAssoc :: OUIdentified ie => ie -> IO JQuery
      lowlightAssoc ie = getALine ie >>= setAttr "stroke" "black" >>= setAttr "stroke-width" "1"
      highlightAssoc :: OUIdentified ie => ie -> IO JQuery
      highlightAssoc ie = getALine ie >>= setAttr "stroke" "red" >>= setAttr "stroke-width" "3"
      setStory :: IO JQuery
      setStory = selectById "story-ufob" >>= setHtml story
        where
        story = "<h2>" <> ousName s2 <> "</h2>" <> ousStory s2

-- Resizing --------------------------------------------------------------

 -- type ResizingRef = IORef Int
--
-- initResizingRef :: IO ResizingRef
-- initResizingRef = newIORef ResizingData
--   { isResizing = False
--   , lastX = Nothing
--   }
--
-- setResizing :: ResizingRef -> Bool -> IO ()
-- setResizing rr flag =
--   modifyIORef rr (\rd -> rd { isResizing = flag })
--
-- setLastX :: ResizingRef -> Int -> IO ()
-- setLastX rr x =
--   modifyIORef rr (\rd -> rd { lastX = Just x })
--
-- --readModelState :: ModelStateRef -> IO ModelState
-- --readModelState = readIORef
--
-- --writeModelState :: ModelStateRef -> ModelState -> IO ()
-- --writeModelState = writeIORef
--
-- startResizing :: ResizingRef -> Handler
-- startResizing rr ev = do
--   setResizing rr True
--   getEvClientX ev >>= setLastX rr
--
-- doResizing :: ResizingRef -> Handler
-- doResizing rr _ = do
--   dumptIO "moving"
--   rd <- readIORef rr
--   when (isResizing rd) (do
--     dumptIO $ show rd
--     _ <- getWindow >>= resize
--     return ()
--     )
--
-- stopResizing :: ResizingRef -> Handler
-- stopResizing rr _ = do
--   dumptIO "stopping resizing"
--   setResizing rr False

resizeLeft :: IO ()
resizeLeft = do
  w1 <- selectById "ufob-box" >>= getWidth
  w2 <- selectById "ufoa-inst-box" >>= getWidth
  _ <- selectById "ufob-box" >>= setWidth (w1 - 100)
  _ <- selectById "ufoa-inst-box" >>= setWidth (w2 + 100)
  return ()

resizeRight :: IO ()
resizeRight = do
  w1 <- selectById "ufob-box" >>= getWidth
  w2 <- selectById "ufoa-inst-box" >>= getWidth
  _ <- selectById "ufob-box" >>= setWidth (w1 + 100)
  _ <- selectById "ufoa-inst-box" >>= setWidth (w2 - 100)
  return ()

resizeDiags :: Handler
resizeDiags _ = do
  wh <- getWindow >>= getHeight
  _ <- selectById "ufob-box" >>= setHeight (wh - footer)
  _ <- selectById "ufoa-inst-box" >>= setHeight (wh - footer)
  _ <- selectById "ufoa-box" >>= setHeight (wh - footer)
  return ()
  where
    footer = 50 + 42 + 25

addEvents :: ModelStateRef -> OUSituation -> IO ()
addEvents msRef s = do
  _ <- getGroup s
    >>= onMouseEnter (\_ -> highlight s)
    >>= onMouseLeave (\_ -> lowlight s)
    >>= onClick (\_ -> setActive msRef s)
  return ()
    where
    highlight :: OUSituation -> IO ()
    highlight s1 = do
      _ <- getBShape s1 >>= setAttr "stroke" hlCol >>= setAttr "stroke-width" "3" >>= setCss "cursor" "pointer"
      return ()
    lowlight :: OUSituation -> IO ()
    lowlight s1 = do
      _ <- getBShape s1 >>= setAttr "stroke" normCol >>= setAttr "stroke-width" "1"
      return ()
    setActive :: ModelStateRef -> OUSituation -> IO ()
    setActive msRef1 s1 = do
      modelState <- readModelState msRef1
      unless (isInitialMs modelState) (do
        _ <- getBShape (lastMsSituation modelState) >>= setAttr "fill" visitedCol
        return ())
      _ <- getBShape s1 >>= setAttr "fill" hlCol
      gotoSituation s1 msRef1
      return ()

initUfoBDiag :: ModelStateRef -> Handler
initUfoBDiag msRef _ = initUfoBDiag' msRef

initUfoBDiag' :: ModelStateRef -> IO ()
initUfoBDiag' msRef = do
  let ss = oumbSituations ouModelB
  mapM_ (addEvents msRef) ss
  --stopLoader
  return ()

main :: IO ()
main = ready $ do
  export "resizeLeft" resizeLeft
  export "resizeRight" resizeRight
  _ <- getWindow >>= onResize resizeDiags >>= resize
  -- rr <- initResizingRef
  msRef <- initModelStateRef
  _ <- diagBJq >>= onLoad (initUfoBDiag msRef)
  initUfoBDiag' msRef
  return ()
