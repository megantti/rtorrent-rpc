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
    , mkRTMethodCall
    , MultiCommand
    , mkMultiCommand

    , TorrentAction (..)
    , sequenceActions
    , TorrentCommand (..)
    , Param (..)
    , getArray, single
    , parseValue
    , (<+>)
) where

import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import Control.Applicative

import Control.DeepSeq
import Control.Monad.Error
import Control.Monad.Identity

import Network.RTorrent.Torrent
import Network.XmlRpc.Internals

-- | A strict 2-tuple for easy combining of commands.
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

    commandValue (a :*: b) (ValueArray xs) = (commandValue a . ValueArray $ take l xs) 
                                    :*: (commandValue b . ValueArray $ drop l xs)
        where
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
    

-- | A newtype wrapper for method calls. 
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

--- MultiCommands

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

-- | Torrent commands.
data TorrentAction a = TorrentAction [(String, [Param])] (Value -> a) TorrentId

-- | Sequence multiple torrent actions, for example with @f = []@.
--
-- 'Applicative' is a bit too strong, as @pure@ would be enough.
sequenceActions :: (Applicative f, Monoid (f a), Foldable f) =>
             f (TorrentId -> TorrentAction a)
             -> TorrentId 
             -> TorrentAction (f a)
sequenceActions = runTorrentCommand . F.foldMap (TorrentCommand . (fmap . fmap) pure)

-- | Wrapper to get a monoid instance.
newtype TorrentCommand a = TorrentCommand { runTorrentCommand :: TorrentId -> TorrentAction a} 

instance Functor TorrentCommand where
    fmap f = TorrentCommand . (fmap f .) . runTorrentCommand

instance Functor TorrentAction where
    fmap f (TorrentAction cmds p tid) = TorrentAction cmds (f . p) tid

instance Monoid a => Monoid (TorrentCommand a) where
    mempty = TorrentCommand $ TorrentAction [] mempty
    (TorrentCommand a) `mappend` (TorrentCommand b) = TorrentCommand $ \tid -> 
      let
        parse arr = parseA (ValueArray $ take len vals) 
          `mappend` parseB (ValueArray $ drop len vals)
          where len = length cmdsA
                vals = getArray arr
        TorrentAction cmdsA parseA _ = a tid
        TorrentAction cmdsB parseB _ = b tid
      in TorrentAction (cmdsA ++ cmdsB) parse tid

data Param = 
    PString String
  | PInt Int
  | PPriority Priority

instance Show Param where
    show (PString str) = show str
    show (PInt i) = show i
    show (PPriority p) = show (fromEnum p)

instance XmlRpcType Param where
    toValue (PString str) = toValue str
    toValue (PInt i) = toValue i
    toValue (PPriority p) = toValue p

    fromValue = fail "No fromValue for Params"
    getType _ = TUnknown

instance Command (TorrentAction a) where
    type Ret (TorrentAction a) = a

    commandCall (TorrentAction cmds _ tid) = 
          RTMethodCall 
        . ValueArray
        . concatMap (\(cmd, params) -> 
                        getArray 
                      . runRTMethodCall $ mkRTMethodCall cmd 
                                    (toValue tid : map toValue params))
        $ cmds

    commandValue (TorrentAction _ parse _) = parse
    levels (TorrentAction cmds _ _) = length cmds

infixr 6 <+>
-- | Combine two torrent commands to get a new one.
(<+>) :: (TorrentId -> TorrentAction a) -> (TorrentId -> TorrentAction b) -> TorrentId -> TorrentAction (a :*: b)
(a <+> b) tid = TorrentAction (cmdsA ++ cmdsB) parse tid
  where
    parse arr =     parseA (ValueArray $ take len vals) 
                :*: parseB (ValueArray $ drop len vals) 
      where len = length cmdsA
            vals = getArray arr
    TorrentAction cmdsA parseA _ = a tid
    TorrentAction cmdsB parseB _ = b tid

