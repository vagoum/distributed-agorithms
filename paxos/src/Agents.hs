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

data AcceptorState = AcceptorState {
        acpid :: ProcessId
      , tMax :: Ticket
      , proposal :: Maybe (Ticket, Command)
}

data ProposerState = ProposerState {
         prpid    :: ProcessId
      ,  ticket :: Ticket
      ,  cmd    :: Command
}

master :: Int -> Int -> Process ()
master a p = do
    masterLog "Is awake"
    masterPid <- getSelfPid
    acceptors <- replicateM a (spawnLocal $ acceptor masterPid)
    proposers <- forM [1..p] (\ cmd -> spawnLocal $ proposer acceptors (fromIntegral cmd))
    liftIO $ threadDelay (5*10^6)

acceptor :: ProcessId -> Process ()
acceptor masterPid = acceptorLog "is up"

proposer :: [ProcessId] -> Command -> Process ()
proposer acceptors cmd = do
    proposerLog "is up"
    selfPid <- getSelfPid
    let majority = (length acceptors) `div` 2 +1
    runRound selfPid acceptors 1 cmd
    where runRound :: ProcessId -> [ProcessId] -> Ticket -> Command -> Process ()
          runRound self acceptors t cmd = do
            brodcastMessage (Prepare t self) acceptors



