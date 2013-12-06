module Server.Player where

import Data.IxSet

import Server.Types

unPlayerId :: PlayerId -> Int
unPlayerId (PlayerId x) = x

instance Indexable Player where
    empty = ixSet
                [ ixFun $ \p -> [ playerId p ]
                , ixFun $ \p -> [ Name $ playerName p ]
                , ixFun $ \p -> [ Token $ playerToken p ]
                ]

