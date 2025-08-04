{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Value
Copyright   : (c) Kai Lindholm, 2025
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

Data types and classes for values we use to communicate over JSON-RPC.
-}

module Network.RTorrent.Value (
    Value(..), Vector, KeyMap, 
    RpcType(..), Err(..), handleError) where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AK
import Data.Scientific
import GHC.Generics (Generic)

-- | ExceptT with an error message.
type Err m a = ExceptT String m a
handleError h m = do
    Right x <- runExceptT (catchError m (lift . h))
    return x

type Vector = V.Vector Value
type KeyMap = M.Map T.Text Value

-- | Values we use to communicate with RTorrent.
-- These are a subset of JSON.
data Value = ValueArray !Vector | 
             ValueInt !Int |
             ValueString !T.Text | 
             ValueStruct !KeyMap
    deriving (Show, Eq, Generic)

instance ToJSON Value where
    toJSON (ValueArray a) = A.Array (V.map A.toJSON a)
    toJSON (ValueString t) = A.String t
    toJSON (ValueInt i) = A.Number (scientific (toEnum i) 0)
    toJSON (ValueStruct v) = A.Object (AK.fromMapText (M.map A.toJSON v))

instance FromJSON Value where
    parseJSON (A.String s) = return (ValueString s)
    parseJSON (A.Object o) = ValueStruct <$> traverse A.parseJSON (AK.toMapText o)
    parseJSON (A.Number n) = 
        if isInteger n 
        then return . ValueInt . fromEnum $ coefficient n
        else fail "Number is not an integer."
    parseJSON (A.Array a) = ValueArray <$> V.mapM A.parseJSON a
    parseJSON _ = fail "Not supported type in JSON."

-- | A class for converting between @Value@s and other types we use.
class RpcType a where
    toValue :: a -> Value
    fromValue :: MonadFail m => Value -> Err m a

instance RpcType Value where
    toValue = id
    fromValue = error "abc"

instance RpcType Int where
    toValue = ValueInt
    fromValue (ValueInt a) = return a
    fromValue v = throwError $ "RpcType fromValue failed: not an integer: " ++ show v

instance RpcType T.Text where
    toValue = ValueString
    fromValue (ValueString a) = return a
    fromValue v = throwError $ "RpcType fromValue failed: not a string: " ++ show v

instance RpcType a => RpcType (V.Vector a) where
    toValue v = ValueArray (V.map toValue v)
    fromValue (ValueArray v) = V.mapM fromValue v
    fromValue v = throwError $ "RpcType fromValue failed: not an array: " ++ show v
    
