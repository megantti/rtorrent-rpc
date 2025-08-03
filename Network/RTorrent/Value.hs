{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Command.Internals
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Value (Value(..), RpcType(..), Err(..), handleError) where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
--import qualified Data.Aeson as A
--

type Vector = V.Vector Value
type KeyMap = M.Map T.Text Value

data Value = ValueArray !Vector | 
             ValueInt !Int |
             ValueString !T.Text | 
             ValueStruct !KeyMap
             --ValueBase64 !B.ByteString
    deriving (Show, Generic)

instance ToJSON Value -- where
    --toJSON (ValueArray a) = 
    --


type Err m a = ExceptT String m a
handleError h m = do
    Right x <- runExceptT (catchError m (lift . h))
    return x


class RpcType a where
    toValue :: a -> Value
    fromValue :: MonadFail m => Value -> Err m a

instance RpcType Value where
    toValue = id
    fromValue = error "abc"

instance RpcType Int where
    toValue = ValueInt
    fromValue (ValueInt a) = return a

instance RpcType T.Text where
    toValue = ValueString
    fromValue (ValueString a) = return a
