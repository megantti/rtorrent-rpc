{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , runRTMethodCall
    , mkRTMethodCall
    , parseSingle
    , getArray
    , getArray'
    , single
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
          val = getArray' . runRTMethodCall . commandCall

    commandValue (a :*: b) (ValueArray xs) = 
          (:*:) <$> (commandValue a . ValueArray $ as)
                <*> (commandValue b . ValueArray $ bs)
        where
            (as, bs) = splitAt (levels a) xs
    commandValue _ _ = fail "commandValue in Command (a :*: b) instance failed"
            
    levels (a :*: b) = levels a + levels b 

-- Helpers for values
getArray :: Monad m => Value -> m [Value]
getArray (ValueArray ar) = return ar
getArray _ = fail "getArray in Network.RTorrent.Commands failed"

getArray' :: Value -> [Value]
getArray' (ValueArray ar) = ar
getArray' _ = error "getArray' in Network.RTorrent.Commands failed"

-- | Extract a value from a singleton array.
single :: Monad m => Value -> m Value
single (ValueArray [ar]) = return ar
single v@(ValueStruct vars) = maybe 
    err
    (\(c, s) -> do
        i <- int c
        s' <- str s
        fail $ "Server returned error " ++ show i ++ 
                                ": " ++ s')
    (liftA2 (,) (lookup "faultCode" vars)
                (lookup "faultString" vars))
  where
    int (ValueInt i) = return i
    int _ = err
    str (ValueString s) = return s
    str _ = err
    err :: Monad m => m a
    err = fail $ "Failed to match a singleton array, got: " ++ show v
single v = fail $ "Failed to match a singleton array, got: " ++ show v

parseValue :: (Monad m, XmlRpcType a) => Value -> m a
parseValue = fromRight . runIdentity . runErrorT . fromValue 
  where
    fromRight (Right r) = return r
    fromRight (Left e) = fail $ "parseValue failed: " ++ e

-- | Parse a value wrapped in two singleton arrays.
parseSingle :: (Monad m, XmlRpcType a) => Value -> m a
parseSingle = parseValue <=< single <=< single

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
    commandValue :: (Applicative m, Monad m) => 
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
    commandValue _ = single <=< single
    levels (AnyCommand cmd) = levels cmd

instance Command a => Command [a] where
    type Ret [a] = [Ret a]
    commandCall = RTMethodCall . ValueArray 
                . concatMap ( getArray'
                            . runRTMethodCall . commandCall)
    commandValue cmds = 
        zipWithM (\cmd -> commandValue cmd . ValueArray) cmds
        . splitPlaces (map levels cmds) 
        <=< getArray 
    levels = sum . map levels 

