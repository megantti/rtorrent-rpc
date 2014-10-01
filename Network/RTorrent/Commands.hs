{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Commands
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Commands (
      (:*:)(..)
    , Command (Ret, commandCall, commandValue, levels) 

    -- * AnyCommand

    , AnyCommand (..)
    
    -- * Internal 
    , RTMethodCall (..)
    , runRTMethodCall
    , mkRTMethodCall
    , parseSingle
    , getArray, single
) where

import Control.DeepSeq
import Control.Monad.Error
import Control.Monad.Identity

import Data.List.Split (splitPlaces)

import Network.XmlRpc.Internals

-- | A strict 2-tuple for easy combining of commands.
data (:*:) a b = (:*:) !a !b
infixr 6 :*:

instance (NFData a, NFData b) => NFData (a :*: b) where
    rnf (a :*: b) = rnf a `seq` rnf b

instance (Show a, Show b) => Show (a :*: b) where
    show (a :*: b) = show a ++ " :*: " ++ show b

instance (Command a, Command b) => Command (a :*: b) where
    type Ret (a :*: b) = Ret a :*: Ret b

    commandCall (a :*: b) = RTMethodCall $ ValueArray (val a ++ val b)
        where
          val :: Command c => c -> [Value]
          val = getArray . runRTMethodCall . commandCall

    commandValue (a :*: b) (ValueArray xs) = (commandValue a . ValueArray $ as) 
                                    :*: (commandValue b . ValueArray $ bs)
        where
            (as, bs) = splitAt l xs
            l = levels a
    commandValue _ _ = error "commandValue in Command (a :*: b) instance failed"
            
    levels (a :*: b) = levels a + levels b 

-- Helpers for values
getArray :: Value -> [Value]
getArray (ValueArray ar) = ar
getArray _ = error "getArray in Network.RTorrent.Commands failed"

single :: Value -> Value
single (ValueArray [ar]) = ar
single v = error $ "Failed to match a singleton array, got: " ++ show v

parseValue :: (NFData a, XmlRpcType a) => Value -> a
parseValue = force . fromRight . runIdentity . runErrorT . fromValue 
  where
    fromRight (Right r) = r
    fromRight (Left e) = error $ "parseValue failed: " ++ e

parseSingle :: (XmlRpcType a, NFData a) => Value -> a
parseSingle = parseValue . single . single

-- | A newtype wrapper for method calls. 
-- 
-- You shouldn't directly use the constructor if you don't know what you are doing.
newtype RTMethodCall = RTMethodCall Value
    deriving Show

runRTMethodCall :: RTMethodCall -> Value
runRTMethodCall (RTMethodCall v) = v

-- | Make a command that should be used when defining 'commandCall'.
mkRTMethodCall :: String -- ^ The name of the method (i.e. get_up_rate)
        -> [Value] -- ^ List of parameters
        -> RTMethodCall
mkRTMethodCall name params = RTMethodCall $ ValueArray [ValueStruct 
                        [ ("methodName", ValueString name)
                        , ("params", ValueArray params)]]

-- | A typeclass for commands that can be send to RTorrent.
class Command a where
    -- | Return type of the command.
    type Ret a 

    -- | Construct a request.
    commandCall :: a -> RTMethodCall 
    -- | Parse the resulting value.
    commandValue :: a -> Value -> Ret a

    levels :: a -> Int
    levels _ = 1

-- | Existential wrapper for any command.
data AnyCommand where
    AnyCommand :: Command a => a -> AnyCommand

instance Command AnyCommand where
    type Ret AnyCommand = Value
    commandCall (AnyCommand cmd) = commandCall cmd
    commandValue _ = head . getArray
    levels (AnyCommand cmd) = levels cmd

instance Command a => Command [a] where
    type Ret [a] = [Ret a]
    commandCall = RTMethodCall . ValueArray 
                . concatMap ( getArray 
                            . runRTMethodCall . commandCall)
    commandValue cmds = zipWith (\cmd -> commandValue cmd . ValueArray) cmds
                                       . splitPlaces (map levels cmds) 
                                       . getArray 
    levels cmds = sum $ map levels cmds

