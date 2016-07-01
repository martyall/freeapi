{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}

module Queries where

import Types
import QueryAlgebras
import Control.Monad.Free
import Database.Neo4j.Transactional.Cypher
--------------------------------------------------------------------------------
-- | Neo4j Interpreter
--------------------------------------------------------------------------------


