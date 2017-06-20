module Data.VirtualDOM.DOM where

import Foundation
import Foundation.Collection

import GHCJS.Types
import GHCJS.Foreign.Callback
import qualified Data.JSString as JSString
import qualified JavaScript.Array.Internal as JSArray
import JavaScript.Array.Internal (JSArray)

newtype Node = Node JSVal

toJSString :: String -> JSString
toJSString = JSString.pack . toList



createElement :: String -> IO Node
createElement name = Node <$> js_createElement (toJSString name)

createElementNS :: String -> String -> IO Node
createElementNS ns name = Node <$> js_createElementNS (toJSString ns) (toJSString name)

createTextNode :: String -> IO Node
createTextNode name = Node <$> js_createTextNode (toJSString name)

replaceChild :: Node -> Node -> Node -> IO ()
replaceChild (Node new) (Node old) (Node parent) = js_replaceChild new old parent

removeChild :: Node -> Node -> IO ()
removeChild (Node node) (Node parent) = js_removeChild node parent

appendChild :: Node -> Node -> IO ()
appendChild (Node node) (Node parent) = js_appendChild node parent

childCount :: Node -> IO Int
childCount (Node e) = do
    children <- JSArray.toList <$> js_children e
    let (CountOf l) = length children
    return l

childAt :: Int -> Node -> IO (Maybe Node)
childAt idx (Node e) = do
    children <- JSArray.toList <$> js_children e
    return (Node <$> children ! Offset idx)

setTextContent :: String -> Node -> IO ()
setTextContent value (Node node) = js_setTextContent (toJSString value) node

setAttribute :: String -> String -> Node -> IO ()
setAttribute name value (Node node) = js_setAttribute (toJSString name) (toJSString value) node

removeAttribute :: String -> Node -> IO ()
removeAttribute name (Node node) = js_removeAttribute (toJSString name) node

addEventListener :: String -> (JSVal -> IO ()) -> Node -> IO ()
addEventListener eventName f (Node node) = do
    cb <- asyncCallback1 f
    js_addEventListener (toJSString eventName) cb node

-- FFI

foreign import javascript unsafe "document.createElement($1)"
    js_createElement :: JSString -> IO JSVal

foreign import javascript unsafe "document.createElementNS($1, $2)"
    js_createElementNS :: JSString -> JSString -> IO JSVal

foreign import javascript unsafe "document.createTextNode($1)"
    js_createTextNode :: JSString -> IO JSVal

foreign import javascript unsafe "$3.replaceChild($1, $2);"
    js_replaceChild :: JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2.removeChild($1);"
    js_removeChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2.appendChild($1);"
    js_appendChild :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$r = $1.children;"
    js_children :: JSVal -> IO JSArray

foreign import javascript unsafe "$2.textContent = $1;"
    js_setTextContent :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "$3.setAttribute($1, $2);"
    js_setAttribute :: JSString -> JSString -> JSVal -> IO ()

foreign import javascript unsafe "$2.removeAttribute($1);"
    js_removeAttribute :: JSString -> JSVal -> IO ()

foreign import javascript unsafe "$3.addEventListener($1, $2);"
    js_addEventListener :: JSString -> Callback (JSVal -> IO()) -> JSVal -> IO ()

--

