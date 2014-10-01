{-# LANGUAGE TypeOperators, TypeFamilies #-}

{-|
Module      : Action
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental
-}

module Network.RTorrent.Action (
      Action (..)
    , ActionM (..)
    , Param (..)
    , simpleAction

    , sequenceActions
    , TorrentAction

    , AllAction (..)

    , (<+>)

    , allToMulti 
) where

import Control.DeepSeq
import Control.Applicative

import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F

import Network.XmlRpc.Internals
import Network.RTorrent.Torrent
import Network.RTorrent.Commands
import Network.RTorrent.Priority

-- | A type for actions that can act on different things like torrents and files.
--
-- @i@ means the identifier type and @a@ the return type.
data Action i a 
    = Action [(String, [Param])] (Value -> a) i

type TorrentAction = Action TorrentId

-- | Wrapper to get a monoid instance.
newtype ActionM i a = ActionM { runActionM :: i -> Action i a} 

-- | A simple action that can be used when constructing new ones.
-- 
-- Watch out for using @Bool@ as @a@ since using it with this function will probably result in an exception.
-- One workaround is to get an @Int@ and use @Bool@'s @Enum@ instance.
simpleAction :: (XmlRpcType a, NFData a) => 
       String
    -> [Param] 
    -> i
    -> Action i a
simpleAction cmd params = Action [(cmd, params)] parseSingle

instance Functor (Action i) where
    fmap f (Action cmds p fid) = Action cmds (f . p) fid

instance Functor (ActionM i) where
    fmap f = ActionM . (fmap f .) . runActionM

instance XmlRpcType i => Command (Action i a) where
    type Ret (Action i a) = a

    commandCall (Action cmds _ tid) = 
          RTMethodCall 
        . ValueArray
        . concatMap (\(cmd, params) -> 
                        getArray 
                      . runRTMethodCall $ mkRTMethodCall cmd 
                                    (toValue tid : map toValue params))
        $ cmds

    commandValue (Action _ parse _) = parse
    levels (Action cmds _ _) = length cmds

data Param = 
    PString String
  | PInt Int
  | PTorrentPriority TorrentPriority
  | PFilePriority FilePriority

instance Show Param where
    show (PString str) = show str
    show (PInt i) = show i
    show (PTorrentPriority p) = show (fromEnum p)
    show (PFilePriority p) = show (fromEnum p)

instance XmlRpcType Param where
    toValue (PString str) = toValue str
    toValue (PInt i) = toValue i
    toValue (PTorrentPriority p) = toValue p
    toValue (PFilePriority p) = toValue p

    fromValue = fail "No fromValue for Params"
    getType _ = TUnknown

-- | Sequence multiple actions, for example with @f = []@.
--
-- 'Applicative' is a bit too strong, as @pure@ would be enough.
sequenceActions :: (Applicative f, Monoid (f a), Foldable f) =>
             f (i -> Action i a)
             -> i
             -> Action i (f a)
sequenceActions = runActionM . F.foldMap (ActionM . (fmap . fmap) pure)

instance Monoid a => Monoid (ActionM i a) where
    mempty = ActionM $ Action [] mempty
    (ActionM a) `mappend` (ActionM b) = ActionM $ \tid -> 
      let
        parse arr = parseA (ValueArray $ take len vals) 
          `mappend` parseB (ValueArray $ drop len vals)
          where len = length cmdsA
                vals = getArray arr
        Action cmdsA parseA _ = a tid
        Action cmdsB parseB _ = b tid
      in Action (cmdsA ++ cmdsB) parse tid


infixr 6 <+>
-- | Combine two actions to get a new one.
(<+>) :: (i -> Action i a) -> (i -> Action i b) -> i -> Action i (a :*: b)
(a <+> b) tid = Action (cmdsA ++ cmdsB) parse tid
  where
    parse arr =     parseA (ValueArray $ take len vals) 
                :*: parseB (ValueArray $ drop len vals) 
      where len = length cmdsA
            vals = getArray arr
    Action cmdsA parseA _ = a tid
    Action cmdsB parseB _ = b tid

data AllAction i a = AllAction i String (i -> Action i a)

makeMultiCall :: [(String, [Param])] -> [String]
makeMultiCall = ("" :) 
              . map (\(cmd, params) -> cmd ++ "=" ++ makeList params)
  where
    makeList :: Show a => [a] -> String
    makeList params = ('{' :) . go params $ "}"
      where
        go :: Show a => [a] -> ShowS
        go [x] = shows x 
        go (x:xs) = shows x . (',' :) . go xs
        go [] = id

allToMulti :: AllAction i a -> TorrentId -> Action TorrentId [a]
allToMulti (AllAction emptyId multicall action) = 
    Action [(multicall, map PString $ makeMultiCall cmds)] 
           (map (parse . ValueArray . map (ValueArray . (:[])) . getArray) . getArray
            . single . single)
  where 
    Action cmds parse _ = action emptyId
    
instance NFData a => Command (AllAction i a) where
    type Ret (AllAction i a) = [a]
    commandCall (AllAction emptyId multicall action) = 
                      mkRTMethodCall multicall
                    . map ValueString
                    . makeMultiCall 
                    $ cmds
      where
        Action cmds _ _ = action emptyId


    commandValue (AllAction emptyId _ action) = 
        force 
        . map ( parse 
            . ValueArray 
            . map (ValueArray . (:[])) 
            . getArray) 
        . getArray . single . single
      where
        Action _ parse _ = action emptyId
