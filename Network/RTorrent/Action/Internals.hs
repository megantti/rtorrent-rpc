{-# LANGUAGE TypeOperators, TypeFamilies, RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Action.Internals
Copyright   : (c) Kai Lindholm, 2014, 2025
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
import Control.Monad.Except (throwError)

import Data.Monoid
import Data.Traversable hiding (mapM)

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T

import Network.RTorrent.Command.Internals
import Network.RTorrent.Priority
import Network.RTorrent.Value

-- | A type for actions that can act on different things like torrents and files.
--
-- @a@ is the return type.
data Action i a = Action {
          actionCmds :: V.Vector (T.Text, V.Vector Param)                
          -- ^ Commands and parameters
        , actionParse :: forall m. (Monad m, MonadFail m) => Value -> m a 
          -- ^ Value parser
        , actionIndex :: i
          -- ^ Index at which the action is executed.
    }

-- | Wrapper to get monoid and applicative instances.
newtype ActionB i a = ActionB { runActionB :: i -> Action i a}

-- | A simple action that can be used when constructing new ones.
-- For example, 
--
-- @
-- getTorrentDir :: TorrentId -> TorrentAction Text
-- getTorrentDir = simpleAction "d.directory" []
--
-- setTorrentDir :: Text -> TorrentId -> TorrentAction Int
-- setTorrentDir dir = simpleAction "d.directory.set" [PString dir]
-- @
--
-- A list of commands can be found in 
--
-- <https://github.com/rakshasa/rtorrent/wiki/rTorrent-0.9-Comprehensive-Command-list-%28WIP%29>
--
-- but to get a proper explanation for the commands a dive into the source code of RTorrent is possibly needed.
simpleAction :: RpcType a =>
       T.Text
    -> [Param]
    -> i
    -> Action i a
simpleAction cmd params = Action (V.singleton (cmd, V.fromList params)) parseValue

instance Functor (Action i) where
    fmap f (Action cmds p fid) = Action cmds (fmap f . p) fid

instance Functor (ActionB i) where
    fmap f = ActionB . (fmap f .) . runActionB

instance Applicative (ActionB i) where
    pure a = ActionB $ Action V.empty (const (pure a))

    (ActionB a) <*> (ActionB b) = ActionB $ \tid -> let
        arrayd 1 x = V.head x
        arrayd _ x = ValueArray x

        parse :: (Monad m, MonadFail m) => (Value -> m (a -> b)) -> (Value -> m a) -> Value -> m b
        parse parseA parseB arr = do
            (valsA, valsB) <- V.splitAt len <$> getArray arr
            parseA (arrayd (length cmdsA) valsA)
              <*> parseB (arrayd (length cmdsB) valsB)
        len = length cmdsA
        Action cmdsA pA _ = a tid
        Action cmdsB pB _ = b tid
      in Action (cmdsA <> cmdsB) (parse pA pB) tid

instance Semigroup a => Semigroup (ActionB i a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (ActionB i a) where
    mempty = pure mempty

instance RpcType i => Command (Action i a) where
    type Ret (Action i a) = a

    commandCall (Action cmds _ tid) =
        RTMethodCall
        . V.map (\(cmd, params) ->
               (cmd, V.cons (toValue tid) (V.map toValue params)))
        $ cmds
    commandValue (Action cmds parse _) = parse <=< deconstructArray
      where
        deconstructArray = if length cmds > 1 then return else single

    levels (Action cmds _ _) = length cmds

-- | Parameters for actions.
data Param =
    PString T.Text
  | PInt Int
  | PBool Bool
  | PTorrentPriority TorrentPriority
  | PFilePriority FilePriority
  | PFilter [T.Text]

instance Show Param where
    show (PString str) = show str
    show (PInt i) = show i
    show (PBool b) = show b
    show (PTorrentPriority p) = show (fromEnum p)
    show (PFilePriority p) = show (fromEnum p)
    show (PFilter f) = show f

instance RpcType Param where
    toValue (PString str) = toValue str
    toValue (PBool b) = toValue b
    toValue (PInt i) = toValue i
    toValue (PTorrentPriority p) = toValue p
    toValue (PFilePriority p) = toValue p
    toValue (PFilter p) = ValueArray . V.fromList . map ValueString $ p

    fromValue _ = throwError "No fromValue for Params"

-- | Sequence multiple actions, for example with @f = []@ or @f = Vector@.
sequenceActions :: Traversable f => f (i -> Action i a) -> i -> Action i (f a)
sequenceActions = runActionB . traverse ActionB

-- | An action that does nothing but return the value.
pureAction :: a -> i -> Action i a
pureAction a = Action V.empty (const (return a))

infixr 6 <+>
-- | Combine two actions to get a new one.
(<+>) :: (i -> Action i a) -> (i -> Action i b) -> i -> Action i (a :*: b)
a <+> b = runActionB $ (:*:) <$> ActionB a <*> ActionB b

data AllAction i a = AllAction 
    i  -- ^ Dummy index
    T.Text -- ^ Function call
    (V.Vector Param) -- ^ Parameters 
    (i -> Action i a) -- ^ Action at each index

makeMultiCallStr :: [(String, [Param])] -> [String]
makeMultiCallStr = ("" :)
              . map (\(cmd, params) -> cmd ++ "=" ++ makeList params)
  where
    makeList :: Show a => [a] -> String
    makeList params = ('{' :) . go params $ "}"
      where
        go :: Show a => [a] -> ShowS
        go [x] = shows x
        go (x:xs) = shows x . (',' :) . go xs
        go [] = id

makeMultiCall :: V.Vector (T.Text, V.Vector Param) -> V.Vector T.Text
makeMultiCall = V.map (\(cmd, params) -> cmd <> "=" <> makeList params)
  where
    makeList :: Show a => V.Vector a -> T.Text
    makeList = T.cons '{' . flip T.snoc '}' . T.intercalate "," . V.toList . V.map (T.pack . show)

-- | Turn an 'AllAction' to a regular 'Action'.
allToMulti :: AllAction i a -> j -> Action j (V.Vector a)
allToMulti (AllAction emptyId multicall filt action) j =
    Action {
        actionCmds = V.singleton (multicall,
                (filt <>)
                . V.map PString $ makeMultiCall cmds),
        actionParse = let 
            deconstructArray = if length cmds > 1 then return else single
            in mapM (parse <=< deconstructArray)
                <=< getArray
        ,
        actionIndex = j
    }
  where
    Action cmds parse _ = action emptyId

instance Command (AllAction i a) where
    type Ret (AllAction i a) = V.Vector a
    commandCall (AllAction emptyId multicall filt action) =
                      mkRTMethodCall multicall
                    . (V.map toValue filt <>)
                    . V.map ValueString
                    . makeMultiCall
                    $ cmds
      where
        Action cmds _ _ = action emptyId

    commandValue (AllAction emptyId _ filt action) =
        mapM (parse
              <=< deconstructArray)
            <=< getArray
            <=< single
      where
        Action cmds parse _ = action emptyId
        deconstructArray = if length cmds > 1 then return else single
