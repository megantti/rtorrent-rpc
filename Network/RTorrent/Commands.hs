{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
) where

import Network.XmlRpc.Internals
import Data.Tagged

-- | A strict 2-tuple for easy combining of commands
data (:*:) a b = (:*:) !a !b
infixr 6 :*:

instance (Show a, Show b) => Show (a :*: b) where
    show (a :*: b) = show a ++ " :*: " ++ show b

instance (Command a, Command b) => Command (a :*: b) where
    type Ret (a :*: b) = Ret a :*: Ret b

    commandCall (a :*: b) = RTMethodCall $ ValueArray (val a ++ val b)
        where
          val :: Command c => c -> [Value]
          val = getArray . runRTMethodCall . commandCall
          getArray (ValueArray ar) = ar
          getArray _ = error "getArray in Command (a :*: b) instance failed"

    commandValue (a :*: b) (ValueArray xs) = (commandValue a . ValueArray $ take l xs) 
                                    :*: (commandValue b . ValueArray $ drop l xs)
        where
            l = unTagged $ levels `asTypeOf` fmap (const 0) (tagSelf a)
    commandValue _ _ = error "commandValue in Command (a :*: b) instance failed"
            
    levels = Tagged $ unTagged (levels :: Tagged a Int) + unTagged (levels :: Tagged b Int)

-- | A newtype wrapper for method calls. 
newtype RTMethodCall = RTMethodCall Value
    deriving Show

runRTMethodCall :: RTMethodCall -> Value
runRTMethodCall (RTMethodCall v) = v

-- | Make a command that should be used when defining 'commandCall'.
makeRTMethodCall :: String -- ^ The name of the method (i.e. get_up_rate)
        -> [Value] -- ^ List of parameters
        -> RTMethodCall
makeRTMethodCall name params = RTMethodCall $ ValueArray [ValueStruct 
                        [ ("methodName", ValueString name)
                        , ("params", ValueArray params)]]

-- | A typeclass for commands that can be send to RTorrent
class Command a where
    -- | Return type of the command
    type Ret a 

    -- | Construct a request
    commandCall :: a -> RTMethodCall 
    -- | Parse the resulting value
    commandValue :: a -> Value -> Ret a

    levels :: Tagged a Int
    levels = 1
