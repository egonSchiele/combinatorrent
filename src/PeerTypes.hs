module PeerTypes
where

import Control.Concurrent
import Control.Concurrent.CML

import Network

data Peer = Peer { peerHost :: HostName,
                   peerPort :: PortID }

data PeerMessage = ChokePeer
                 | UnchokePeer
                 | PeerStats (Channel (Double, Bool))

type PeerChannel = Channel PeerMessage


data MgrMessage = Connect ThreadId (Channel PeerMessage)
                | Disconnect ThreadId

type MgrChannel = Channel MgrMessage
