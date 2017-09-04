{-
 - Original version (Copyright 2016 Bodil Stokke):
 - https://github.com/bodil/purescript-vdom
 -
 - Porting (Copyright 2017 The Agile Monkeys S.L.)
 -
 - This is a port from PureScript to Haskell, using GHCJS
 -
 - This program is free software: you can redistribute it
 - and/or modify it under the terms of the GNU Lesser General
 - Public License as published by the Free Software Foundation,
 - either version 3 of the License, or (at your option) any
 - later version.
 -
 - This program is distributed in the hope that it will be
 - useful, but WITHOUT ANY WARRANTY; without even the implied
 - warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 - PURPOSE. See the GNU Lesser General Public License for
 - more details.
 -}
module Data.VirtualDOM where

import           Foundation
import           Foundation.Collection
import           Control.Monad (when)
import           GHCJS.Types

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import qualified Data.VirtualDOM.DOM as DOM

type Props = Map String String

data EventListener l = On String (JSVal -> IO ())

data VNode l
    = Element
        { name :: String
        , props :: Props
        , listeners :: [EventListener l]
        , children :: [VNode l]
        }
    | Text String

h :: String -> Props -> [VNode l] -> VNode l
h newName newProps = Element newName newProps []

text :: String -> VNode l
text = Text

prop :: [(String, String)] -> Props
prop = Map.fromList

with :: VNode l-> [EventListener l] -> VNode l
with (Text t) _ = Text t
with e l = e { listeners = l }

data DOM l = DOM
  { createElement :: String -> IO l
  , createElementNS :: String -> String -> IO l
  , createTextNode :: String -> IO l
  , replaceChild :: l -> l -> l -> IO ()
  , removeChild :: l -> l -> IO ()
  , appendChild :: l -> l -> IO ()
  , childCount :: l -> IO Int
  , childAt :: Int -> l -> IO (Maybe l)
  , setTextContent :: String -> l -> IO ()
  , setAttribute :: String -> String -> l -> IO ()
  , removeAttribute :: String -> l -> IO ()
  , addEventListener :: String -> (JSVal -> IO ()) -> l -> IO ()
  , removeEventListener :: String -> l -> IO ()
  }

domAPI :: DOM DOM.Node
domAPI = DOM
  { createElement = DOM.createElement
  , createElementNS = DOM.createElementNS
  , createTextNode = DOM.createTextNode
  , replaceChild = DOM.replaceChild
  , removeChild = DOM.removeChild
  , appendChild = DOM.appendChild
  , childCount = DOM.childCount
  , childAt = DOM.childAt
  , setTextContent = DOM.setTextContent
  , setAttribute = DOM.setAttribute
  , removeAttribute = DOM.removeAttribute
  , addEventListener = DOM.addEventListener
  , removeEventListener = DOM.removeEventListener
  }

newtype Elem = Elem JSVal

createEl :: DOM l-> VNode l-> IO l
createEl api (Text t) = createTextNode api t
createEl api e = do
    el <- createElement api (name e)
    mapM_ (\(k, v) -> setAttribute api k v el) (Map.toList $ props e)
    mapM_ (addListener api el) (listeners e)
    mapM_ (createEl api >=> flip (appendChild api) el) (children e)
    return el

addListener :: DOM l -> l -> EventListener l -> IO ()
addListener api target (On n handler) = addEventListener api n handler target

removeListener :: DOM l -> l -> EventListener l -> IO ()
removeListener api target (On n _) = removeEventListener api n target

changed :: VNode l -> VNode l-> Bool
changed (Text t1) (Text t2) = t1 /= t2
changed e1 e2 = name e1 /= name e2

updateListeners :: DOM l -> l -> [EventListener l] -> [EventListener l] -> IO ()
updateListeners api me oldListeners newListeners = do
  mapM_ (removeListener api me) oldListeners
  mapM_ (addListener api me) newListeners

updateProps :: DOM l -> l -> Props -> Props -> IO ()
updateProps api target old new =
    mapM_ update (Map.keys (Map.union old new))
  where
    update key =
        case (Map.lookup key old, Map.lookup key new) of
            (Nothing, Just value) ->
                setAttribute api key value target
            (Just _, Nothing) ->
                removeAttribute api key target
            (Just prev, Just next) ->
                when (prev /= next) (setAttribute api key next target)
            (Nothing, Nothing) ->
                return ()

patch :: DOM l-> l -> Maybe (VNode l) -> Maybe (VNode l) -> IO ()
patch api target' old' new' = patchIndexed api target' old' new' 0

patchIndexed :: DOM l-> l -> Maybe (VNode l) -> Maybe (VNode l) -> Int -> IO ()
patchIndexed _ _ Nothing Nothing _ = return ()

patchIndexed api parent Nothing (Just new) _ = do
  el <- createEl api new
  appendChild api el parent

patchIndexed api parent (Just _) Nothing index = do
  child <- childAt api index parent
  case child of
    Just n -> removeChild api n parent
    Nothing -> return ()

patchIndexed api parent (Just (Text old)) (Just (Text new)) index =
  when (old /= new) $ do
    me <- childAt api index parent
    maybe (return ()) (setTextContent api new) me

patchIndexed api parent (Just (Text _)) (Just new@(Element _ _ _ _)) _ = do
  setTextContent api "" parent -- remove inner text
  n <- createEl api new
  appendChild api n parent

patchIndexed api parent (Just (Element _ _ _ _)) (Just (Text new)) _ = do
  cs <- childCount api parent
  mapM_ (f parent) (reverse [0..cs-1])
  setTextContent api new parent

  where
    f p i = do
      child <- childAt api i p
      case child of
        Just n -> removeChild api n p
        Nothing -> return ()

patchIndexed api parent (Just old) (Just new) index = do
  me' <- childAt api index parent
  case me' of
    Nothing -> return ()
    Just me ->
      if changed old new
          then do
            n <- createEl api new
            replaceChild api n me parent
          else do
            case (old, new) of
              (Element {props = oldProps, listeners = oldListeners}, Element {props = newProps, listeners = newListeners}) -> do
                updateListeners api me oldListeners newListeners
                updateProps api me oldProps newProps
              (_, _) -> return ()
            walkChildren api me old new

walkChildren :: DOM l -> l -> VNode l -> VNode l -> IO ()
walkChildren api target old new =
    if oldLength > newLength
        then do
            walkIndexes' [0 .. (newLength - 1)] -- walk up to last child of new
            walkIndexes' [(oldLength - 1) .. newLength] -- delete children backwards from end
        else
            walkIndexes' [0 .. (newLength - 1)]
  where
    walkIndexes' =
        mapM_ (\i' ->
            let
                i = Offset i'
            in
                patchIndexed api target (children old ! i) (children new ! i) i')
    oldLength = fromIntegral $ toInteger $ length $ children old
    newLength = fromIntegral $ toInteger $ length $ children new

foreign import javascript unsafe "$r = document.body;"
    js_body :: IO JSVal

getBody :: IO DOM.Node
getBody = do
    putStrLn "Getting body"
    DOM.Node <$> js_body
