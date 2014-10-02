{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Command
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Command (
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
    , bool
    , mapStrict
) where

import Control.Applicative
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

    commandValue (a :*: b) (ValueArray xs) = 
              (commandValue a . ValueArray $ as)
          :*: (commandValue b . ValueArray $ bs)
        where
            (as, bs) = splitAt (levels a) xs
    commandValue _ _ = error "commandValue in Command (a :*: b) instance failed"
            
    levels (a :*: b) = levels a + levels b 

-- Helpers for values
getArray :: Value -> [Value]
getArray (ValueArray ar) = ar
getArray _ = error "getArray in Network.RTorrent.Commands failed"

-- | Extract a value from a singleton array.
single :: Value -> Value
single (ValueArray [ar]) = ar
single v@(ValueStruct vars) = maybe 
    err
    (\(c, s) -> error $ "Server returned error " ++ show (int c) ++ 
                                ": " ++ str s)
    (liftA2 (,) (lookup "faultCode" vars)
                (lookup "faultString" vars))
  where
    int (ValueInt i) = i
    int _ = err
    str (ValueString s) = s
    str _ = err
    err :: a
    err = error $ "Failed to match a singleton array, got: " ++ show v
single v = error $ "Failed to match a singleton array, got: " ++ show v

bool :: Value -> Bool
bool (ValueInt 0) = False
bool (ValueInt 1) = True
bool (ValueBool b) = b
bool v = error $ "Failed to match a bool, got: " ++ show v

parseValue :: (XmlRpcType a) => Value -> a
parseValue = fromRight . runIdentity . runErrorT . fromValue 
  where
    fromRight (Right r) = r
    fromRight (Left e) = error $ "parseValue failed: " ++ e

-- | Parse a value wrapped in two singleton arrays.
parseSingle :: XmlRpcType a => Value -> a
parseSingle = parseValue . single . single

-- | Map that is very strict: it evaluates all of its elements to WNHF.
mapStrict :: (a -> b) -> [a] -> [b]
mapStrict f = go
  where
    go [] = []
    go (a:as) = ((:) $! f a) $! go as

-- | A newtype wrapper for method calls. 
-- 
-- You shouldn't directly use the constructor 
-- if you don't know what you are doing.
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
-- 
-- Commands wrapped in AnyCommand won't parse their results.
data AnyCommand where
    AnyCommand :: Command a => a -> AnyCommand

instance Command AnyCommand where
    type Ret AnyCommand = Value
    commandCall (AnyCommand cmd) = commandCall cmd
    commandValue _ = single . single
    levels (AnyCommand cmd) = levels cmd

instance Command a => Command [a] where
    type Ret [a] = [Ret a]
    commandCall = RTMethodCall . ValueArray 
                . concatMap ( getArray 
                            . runRTMethodCall . commandCall)
    commandValue cmds = 
        mapStrict id
        . zipWith (\cmd -> commandValue cmd . ValueArray) cmds
        . splitPlaces (map levels cmds) 
        . getArray 
    levels = sum . map levels 

