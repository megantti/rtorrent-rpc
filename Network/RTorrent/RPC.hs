{-|
Module      : RPC
Copyright   : (c) Kai Lindholm, 2014
License     : MIT
Maintainer  : megantti@gmail.com
Stability   : experimental

This package can be used for communicating with RTorrent over its XML-RPC interface.

As an example, you can request torrent info and bandwidth usage:

> result <- callCommand "localhost" 5000 $ GetTorrentInfo :*: getUpRate :*: getDownRate
> case result of 
>   Right (torrentInfo :*: uploadRate :*: downloadRate) -> ...
where

>>> :t torrentInfo
[TorrentInfo]

>>> :t uploadRate
Int

assuming you have set @scgi_port = localhost:5000@ in your @.rtorrent.rc@.

-}

module Network.RTorrent.RPC (
      callCommand 
    , module Network.RTorrent.Commands 
    , module Network.RTorrent.CommandList
    ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans
import Control.Monad.Error (ErrorT(..), throwError, strMsg)
import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Network
import Network.XmlRpc.Internals

import Network.RTorrent.Commands (Command (Ret)
                                 , (:*:) (..)
                                 , MultiCommand
                                 , mkMultiCommand)
import qualified Network.RTorrent.Commands as C
import Network.RTorrent.CommandList
import Network.RTorrent.SCGI

callRTorrent :: HostName -> Int -> C.RTMethodCall -> ErrorT String IO Value
callRTorrent host port calls = do
    let request = Body [] . mconcat . LB.toChunks $ renderCall call 
    Body _ content <- ErrorT $ query host port request
    let cs = map (toEnum . fromEnum) $ BS.unpack content
    response <- parseResponse cs
    case response of
        Return ret -> case ret of
            ValueArray arr -> return $ ValueArray arr
            val -> throwError . strMsg $ "Got value of type " ++ show (getType val)
        Fault code err -> throwError . strMsg $ show code ++ ": " ++ err
  where
    call = MethodCall "system.multicall" [C.runRTMethodCall calls]

-- | Call RTorrent with a command.
-- This opens only one connection even when using ':*:' to combine commands.
callCommand :: Command a => 
    HostName -- ^ Hostname
    -> Int  -- ^ Port
    -> a -- ^ Command to send to RTorrent
    -> IO (Either String (Ret a))
callCommand host port command = handleException $ runErrorT . (>>= lift . evaluate) $ 
    C.commandValue command <$> callRTorrent host port (C.commandCall command)
  where
    handleException f = catch f (\e -> return . Left . show $ (e :: SomeException))

