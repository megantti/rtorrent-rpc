{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes #-}

{-|
Module      : Action.Internals
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

-}

module Network.RTorrent.Action.Internals (
      Action (..)
    , simpleAction
    , pureAction

    , sequenceActions
    , (<+>)

    , Param (..)
    , ActionB (..)

    , AllAction (..)
    , allToMulti 
) where

import Control.Applicative
import Control.Monad

import Data.Monoid
import Data.Traversable hiding (mapM)

import Network.XmlRpc.Internals
import Network.RTorrent.Command.Internals
import Network.RTorrent.Priority

-- | A type for actions that can act on different things like torrents and files.
--
-- @a@ is the return type.
data Action i a 
    = Action [(String, [Param])] (forall m. (Monad m, Applicative m) => Value -> m a) i

-- | Wrapper to get monoid and applicative instances.
newtype ActionB i a = ActionB { runActionB :: i -> Action i a} 

-- | A simple action that can be used when constructing new ones.
-- 
-- Watch out for using @Bool@ as @a@ since using it with this function will probably result in an error,
-- since RTorrent actually returns 0 or 1 instead of a bool.
-- One workaround is to get an @Int@ and use @Bool@'s @Enum@ instance.
simpleAction :: XmlRpcType a => 
       String
    -> [Param] 
    -> i
    -> Action i a
simpleAction cmd params = Action [(cmd, params)] parseSingle

instance Functor (Action i) where
    fmap f (Action cmds p fid) = Action cmds (fmap f . p) fid

instance Functor (ActionB i) where
    fmap f = ActionB . (fmap f .) . runActionB

instance Applicative (ActionB i) where
    pure a = ActionB $ Action [] (const (pure a))  

    (ActionB a) <*> (ActionB b) = ActionB $ \tid -> let 
        parse :: (Monad m, Applicative m) => (Value -> m (a -> b)) -> (Value -> m a) -> Value -> m b
        parse parseA parseB arr = do
            (valsA, valsB) <- splitAt len <$> getArray arr
            parseA (ValueArray valsA) 
              <*> parseB (ValueArray valsB) 
        len = length cmdsA
        Action cmdsA pA _ = a tid
        Action cmdsB pB _ = b tid
      in Action (cmdsA ++ cmdsB) (parse pA pB) tid

instance Monoid a => Monoid (ActionB i a) where
    mempty = pure mempty
    mappend = liftA2 mappend  

instance XmlRpcType i => Command (Action i a) where
    type Ret (Action i a) = a

    commandCall (Action cmds _ tid) = 
          RTMethodCall 
        . ValueArray
        . concatMap (\(cmd, params) -> 
                        getArray'
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

-- | An action that does nothing but return the value.
pureAction :: a -> i -> Action i a
pureAction a = Action [] (const (return a))

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

wrapForParse :: Monad m => Value -> m [Value]
wrapForParse = mapM ( 
                 return . ValueArray 
                 . map (ValueArray . (:[])) 
                 <=< getArray) 
               <=< getArray <=< single <=< single

allToMulti :: AllAction i a -> j -> Action j [a]
allToMulti (AllAction emptyId multicall action) = 
    Action [(multicall, map PString $ makeMultiCall cmds)] 
           (mapM parse <=< wrapForParse)
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
        mapM parse <=< wrapForParse
      where
        Action _ parse _ = action emptyId

