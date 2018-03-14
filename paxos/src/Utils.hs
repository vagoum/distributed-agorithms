module Agents where


import Control.Concurrent           (threadDelay)
import Control.Monad                (forM, replicateM)
import Control.Distributed.Process
import Data.Binary
import Data.Typeable
import Types


masterLog :: String -> Process ()
masterLog s = say $ "[Master] " ++ s

proposerLog :: String -> Process ()
proposerLog s = say $ "[Proposer] " ++ s

acceptorLog :: String -> Process ()
acceptorLog s = say $ "[Acceptor] " ++ s

brodcastMessage :: (Binary a, Typeable a) => a -> [ProcessId] -> Process ()
brodcastMessage msg l = mapM_ (`send` msg) l
