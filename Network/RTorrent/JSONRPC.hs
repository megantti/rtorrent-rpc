{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

{-|
Module      : JSONRPC
Copyright   : (c) Kai Lindholm, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.JSONRPC (jsonRPCcall, jsonRPCdecode) where

import Control.Monad
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB

import Network.RTorrent.CommandList
import Network.RTorrent.SCGI
import Network.RTorrent.Value
import qualified Network.RTorrent.Command.Internals as C
import Data.Aeson.Encode.Pretty (encodePretty)

jsonRPCcall :: C.RTMethodCall -> V.Vector Value
jsonRPCcall = V.imap call . C.runRTMethodCall
  where
    call i (method, params) =
        ValueStruct (M.fromList [
        ("jsonrpc", ValueString "2.0"),
        ("method", ValueString method),
        ("id", ValueInt i),
        ("params", ValueArray params)
        ])

jsonRPCdecode :: Value -> Either String Value
jsonRPCdecode (ValueArray val) = do
      im <- V.sequence items
      let m = maximum . V.toList . V.map fst $ im
      let v = V.replicate (m+1) (Left "JSON-RPC response is missing a part.")
      let vs = V.update v im
      ValueArray <$> sequence vs
  where
    --step im 
    decodeItem :: Value -> Either String (Int, Either String Value)
    decodeItem (ValueStruct a) = do
        js <- maybe (Left "Invalid JSON-RPC response.") Right $ M.lookup "jsonrpc" a
        when (js /= ValueString "2.0") $ Left "Invalid JSON-RPC response."
        maybe (Right ()) 
            (\e -> Left ("JSON-RPC error: " 
                        ++ map (toEnum . fromEnum) (LB.unpack (encodePretty e)))) 
            $ M.lookup "error" a
        res <- maybe (Left "Invalid JSON-RPC response.") Right $ M.lookup "result" a
        iv <- maybe (Left "Invalid JSON-RPC response.") Right $ M.lookup "id" a
        let res' = case res of
                    ValueArray v -> ValueArray v
                    a -> ValueArray (V.singleton a)
        let i = case iv of
                ValueInt i -> Right i
                _ -> Left "Invalid JSON-RPC response."
        (\j -> (j, Right res')) <$> i
    decodeItem _ = Left "Invalid JSON-RPC response."
    --items :: Either String (IM.IntMap Value)
    items :: V.Vector (Either String (Int, Either String Value))
    items = V.map decodeItem val


jsonRPCdecode _ = Left "Invalid JSON-RPC response."
