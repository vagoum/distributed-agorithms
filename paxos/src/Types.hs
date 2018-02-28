{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Binary
import GHC.Generics
import Control.Distributed.Process   (ProcessId)

type Ticket  = Integer
type Command = Integer


-- | The basic Message types needed for the Paxos algorithm. Ticket is used
-- in every message sent to prevent replayed messages from older rounds.The
-- 'PromiseΝοtOk' message is an optimization that can be enabled so that the
-- Acceptor sends to the Proposer the latest ticket, instead of have him retry
-- with the next one.

data Prepare      = Prepare      Ticket                 ProcessId
    deriving (Show, Binary, Generic)
data PromiseOk    = PromiseOk    Ticket (Maybe Command) ProcessId
    deriving (Show, Binary, Generic)
data PromiseNotOk = PromiseNotOk Ticket
data Propose      = Propose      Ticket Command         ProcessId
    deriving (Show, Binary, Generic)
data Success      = Success                             ProcessId
    deriving (Show, Binary, Generic)
data Execute      = Execute      Ticket Command         ProcessId
    deriving (Show, Binary, Generic)
data Executed     = Executed     Ticket                 ProcessId
    deriving (Show, Binary, Generic)
