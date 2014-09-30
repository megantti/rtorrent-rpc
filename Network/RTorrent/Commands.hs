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
    , Command (Ret, commandCall, commandValue) 
    , RTMethodCall
    , runRTMethodCall
    , makeRTMethodCall
    , MultiCommand
    , mkMultiCommand
) where

import Data.Monoid

import Network.XmlRpc.Internals

-- | A strict 2-tuple for easy combining of commands.
data (:*:) a b = (:*:) !a !b
infixr 6 :*:

instance (Show a, Show b) => Show (a :*: b) where
    show (a :*: b) = show a ++ " :*: " ++ show b

getArray :: Value -> [Value]
getArray (ValueArray ar) = ar
getArray _ = error "getArray in Network.RTorrent.Commands failed"

instance (Command a, Command b) => Command (a :*: b) where
    type Ret (a :*: b) = Ret a :*: Ret b

    commandCall (a :*: b) = RTMethodCall $ ValueArray (val a ++ val b)
        where
          val :: Command c => c -> [Value]
          val = getArray . runRTMethodCall . commandCall

    commandValue (a :*: b) (ValueArray xs) = (commandValue a . ValueArray $ take l xs) 
                                    :*: (commandValue b . ValueArray $ drop l xs)
        where
            l = levels a
    commandValue _ _ = error "commandValue in Command (a :*: b) instance failed"
            
    levels (a :*: b) = levels a + levels b 

-- | A newtype wrapper for method calls. 
newtype RTMethodCall = RTMethodCall Value
    deriving Show

data MkCommand where
    MkCommand :: Command a => a -> MkCommand

instance Command MkCommand where
    type Ret MkCommand = Value
    commandCall (MkCommand cmd) = commandCall cmd
    commandValue _ = head . getArray
    levels (MkCommand cmd) = levels cmd

-- | A command that can be used to run multiple commands
-- without interpreting their results.
newtype MultiCommand = MultiCommand [MkCommand]

instance Monoid MultiCommand where
    mempty = MultiCommand []
    (MultiCommand as) `mappend` (MultiCommand bs) = MultiCommand (as ++ bs)

instance Command MultiCommand where
    type Ret MultiCommand = Value
    commandCall (MultiCommand m) = RTMethodCall . ValueArray 
                . concatMap ( getArray 
                            . runRTMethodCall . commandCall)
                $ m
    commandValue _ = id
    levels (MultiCommand cmds) = sum $ map levels cmds

mkMultiCommand :: Command a => a -> MultiCommand
mkMultiCommand = MultiCommand . (:[]) . MkCommand

runRTMethodCall :: RTMethodCall -> Value
runRTMethodCall (RTMethodCall v) = v

-- | Make a command that should be used when defining 'commandCall'.
makeRTMethodCall :: String -- ^ The name of the method (i.e. get_up_rate)
        -> [Value] -- ^ List of parameters
        -> RTMethodCall
makeRTMethodCall name params = RTMethodCall $ ValueArray [ValueStruct 
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
