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
    , TorrentAction
    , ActionB (..)
    , simpleAction

    , sequenceActions
    , (<+>)

    , Param (..)

    -- * Internal
    , AllAction (..)
    , allToMulti 
) where

import Control.Applicative

import Data.Monoid
import Data.Traversable

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

-- | Wrapper to get monoid and applicative instances.
newtype ActionB i a = ActionB { runActionB :: i -> Action i a} 

-- | A simple action that can be used when constructing new ones.
-- 
-- Watch out for using @Bool@ as @a@ since using it with this function will probably result in an exception.
-- One workaround is to get an @Int@ and use @Bool@'s @Enum@ instance.
simpleAction :: XmlRpcType a => 
       String
    -> [Param] 
    -> i
    -> Action i a
simpleAction cmd params = Action [(cmd, params)] parseSingle

instance Functor (Action i) where
    fmap f (Action cmds p fid) = Action cmds (f . p) fid

instance Functor (ActionB i) where
    fmap f = ActionB . (fmap f .) . runActionB

instance Applicative (ActionB a) where
    pure a = ActionB $ Action [] (const a)  
    (ActionB a) <*> (ActionB b) = ActionB $ \tid -> let 
        parse arr = parseA (ValueArray valsA) 
                    $ parseB (ValueArray valsB) 
          where (valsA, valsB) = splitAt len $ getArray arr
        len = length cmdsA
        Action cmdsA parseA _ = a tid
        Action cmdsB parseB _ = b tid
      in Action (cmdsA ++ cmdsB) parse tid

instance Monoid a => Monoid (ActionB i a) where
    mempty = pure mempty
    mappend = liftA2 mappend  

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

-- | Parameters for actions.
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
sequenceActions :: Traversable f => f (i -> Action i a) -> i -> Action i (f a)
sequenceActions = runActionB . traverse ActionB

infixr 6 <+>
-- | Combine two actions to get a new one.
(<+>) :: (i -> Action i a) -> (i -> Action i b) -> i -> Action i (a :*: b)
a <+> b = runActionB $ (:*:) <$> ActionB a <*> ActionB b

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

wrapForParse :: Value -> [Value]
wrapForParse = map ( 
                   ValueArray 
                 . map (ValueArray . (:[])) 
                 . getArray) 
               . getArray . single . single

allToMulti :: AllAction i a -> TorrentId -> Action TorrentId [a]
allToMulti (AllAction emptyId multicall action) = 
    Action [(multicall, map PString $ makeMultiCall cmds)] 
           (map parse . wrapForParse)
  where 
    Action cmds parse _ = action emptyId
    
instance Command (AllAction i a) where
    type Ret (AllAction i a) = [a]
    commandCall (AllAction emptyId multicall action) = 
                      mkRTMethodCall multicall
                    . map ValueString
                    . makeMultiCall 
                    $ cmds
      where
        Action cmds _ _ = action emptyId

    commandValue (AllAction emptyId _ action) = 
        mapStrict parse . wrapForParse
      where
        Action _ parse _ = action emptyId

