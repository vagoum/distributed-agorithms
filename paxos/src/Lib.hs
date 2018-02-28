{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import Agents
import System.Environment               (getArgs)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process.Node  (runProcess, initRemoteTable, newLocalNode)
import Control.Distributed.Process.Closure 






someFunc :: IO ()
someFunc = do
    [a', p'] <- getArgs
    let a = read a'
        p = read p'
    Right transport  <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode transport initRemoteTable
    runProcess node $ master a p 