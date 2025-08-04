{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Command.Internals
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Command.Internals (
      (:*:)(..)
    , Command (Ret, commandCall, commandValue, levels) 

    , AnyCommand (..)
    
    , RTMethodCall (..)
    , mkRTMethodCall
    , parseSingle
    , parseValue
    , getArray
    , getArray'
    , single
) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Identity

import Control.Monad ((<=<), zipWithM)

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Data.Vector.Split (splitPlaces)
import Network.RTorrent.Value

-- | A strict 2-tuple for easy combining of commands.
data (:*:) a b = (:*:) !a !b
infixr 6 :*:

instance (NFData a, NFData b) => NFData (a :*: b) where
    rnf (a :*: b) = rnf a `seq` rnf b

instance (Show a, Show b) => Show (a :*: b) where
    show (a :*: b) = show a ++ " :*: " ++ show b

instance (Command a, Command b) => Command (a :*: b) where
    type Ret (a :*: b) = Ret a :*: Ret b

    commandCall (a :*: b) = RTMethodCall (val a <> val b)
        where
          val :: Command c => c -> V.Vector (T.Text, V.Vector Value)
          val = runRTMethodCall . commandCall

    commandValue (a :*: b) (ValueArray xs) = 
          (:*:) <$> (commandValue a . ValueArray $ as)
                <*> (commandValue b . ValueArray $ bs)
        where
            (as, bs) = V.splitAt (levels a) xs
    commandValue _ _ = fail "commandValue in Command (a :*: b) instance failed"
            
    levels (a :*: b) = levels a + levels b 

-- Helpers for values
getArray :: (Monad m, MonadFail m) => Value -> m (V.Vector Value)
getArray (ValueArray ar) = return ar
getArray _ = fail "getArray in Network.RTorrent.Commands failed"

getArray' :: Value -> V.Vector Value
getArray' (ValueArray ar) = ar
getArray' _ = error "getArray' in Network.RTorrent.Commands failed"

-- | Extract a value from a singleton array.
single :: (Monad m, MonadFail m)  => Value -> m Value
single (ValueArray ar) = if V.null ar 
        then fail "Array has no values"
        else return $ V.head ar
single v = fail $ "Failed to match a singleton array, got: " ++ show v

parseValue :: (Monad m, MonadFail m, RpcType a) => Value -> m a
parseValue = handleError (\e -> fail $ "parseValue failed: " ++ e) . fromValue

-- | Parse a value wrapped in a singleton array.
parseSingle :: (Monad m, MonadFail m, RpcType a) => Value -> m a
parseSingle = parseValue <=< single

-- | A newtype wrapper for method calls. 
-- 
-- You shouldn't directly use the constructor 
-- if you don't know what you are doing.
newtype RTMethodCall = RTMethodCall {
    runRTMethodCall :: V.Vector (T.Text, V.Vector Value)
}
    deriving Show

-- | Make a command that should be used when defining 'commandCall'.
mkRTMethodCall :: T.Text -- ^ The name of the method (i.e. get_up_rate)
        -> V.Vector Value -- ^ List of parameters
        -> RTMethodCall
mkRTMethodCall name params = RTMethodCall . V.singleton $ (name, params)

-- | A typeclass for commands that can be send to RTorrent.
class Command a where
    -- | Return type of the command.
    type Ret a 

    -- | Construct a request.
    commandCall :: a -> RTMethodCall 
    -- | Parse the resulting value.
    commandValue :: (Applicative m, Monad m, MonadFail m) => 
        a -> Value -> m (Ret a)

    levels :: a -> Int
    levels _ = 1

-- | Existential wrapper for any command.
-- 
-- @Command@s wrapped in @AnyCommand@ won't parse their results.
--
-- @AnyCommand@ can be used when you want to call multiple commands
-- but don't care about their return values.
data AnyCommand where
    AnyCommand :: Command a => a -> AnyCommand

instance Command AnyCommand where
    type Ret AnyCommand = Value
    commandCall (AnyCommand cmd) = commandCall cmd
    commandValue _ = single
    levels (AnyCommand cmd) = levels cmd

instance Command a => Command (V.Vector a) where
    type Ret (V.Vector a) = V.Vector (Ret a)
    commandCall = RTMethodCall . V.concatMap (runRTMethodCall . commandCall)
    commandValue cmds = 
        V.zipWithM (\cmd -> commandValue cmd . ValueArray) cmds
        . V.fromList 
        . splitPlaces (map levels (V.toList cmds))
        <=< getArray
    levels = sum . V.map levels 


instance Command a => Command [a] where
    type Ret [a] = [Ret a]
    commandCall = RTMethodCall 
                . V.concatMap (runRTMethodCall . commandCall)
                . V.fromList
    commandValue cmds = 
        zipWithM (\cmd -> commandValue cmd . ValueArray) cmds
        . splitPlaces (map levels cmds) 
        <=< getArray
    levels = sum . map levels 

