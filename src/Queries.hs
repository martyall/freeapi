{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}

module Queries where

import Types
import QueryAlgebras

import Control.Error
import Control.Monad.Free
import Data.Text (Text)
import Database.Neo4j
import Database.Neo4j.Transactional.Cypher


getUsernameFromId :: UserId -> VFAlgebra db Username
getUsernameFromId uId = username <$> readOp SUserCrud uId

getPersonsMedias :: UserId -> VFAlgebra db [MediaBase cxt]
getPersonsMedias = undefined

-- In an ideal world of `media == media vfile` this function will be a lot
-- more simple.
ownsComment :: UserId -> CommentId -> SCommentType ct -> VFAlgebra db Bool
ownsComment uId cId ct = do
  case ct of
    SMediaComment -> do
      medId <- commentSourceId <$> readOp (SCommentCrud SMediaComment) cId
      med <- readOp SMediaCrud medId
      return $ (== uId) . mediaOwner $ med
    SMediaVfileComment -> do
      mvfId <- commentSourceId <$> readOp (SCommentCrud SMediaVfileComment) cId
      mvf <- readOp SMediaVfileCrud mvfId
      return $ (== uId) . mvfOwner $ mvf

--------------------------------------------------------------------------------
-- | Neo4j Interpreter
--------------------------------------------------------------------------------

-- withConn :: Neo4j a -> IO a
-- withConn = withConnection "localhost" 7474

-- runQuery :: Text -> Params -> IO (Either TransError Result)
-- runQuery q params = withConn (runTransaction $ cypher q params)

crudNeoF :: CrudF 'Neo (IO (Either VfilesError a))
         -> IO (Either VfilesError a)
crudNeoF = undefined

crudNeo :: VFAlgebra 'Neo a
        -> IO (Either VfilesError a)
crudNeo = (iterM crudNeoF) . runExceptT

crudPGF :: CrudF 'PG (IO (Either VfilesError a))
        -> IO (Either VfilesError a)
crudPGF = undefined

crudPG :: VFAlgebra 'PG a
       -> IO (Either VfilesError a)
crudPG = (iterM crudPGF) . runExceptT

crudF :: VFSum (IO (Either VfilesError a))
      -> IO (Either VfilesError a)
crudF (InL pg) = crudPGF pg
crudF (InR neo) = crudNeoF neo

crud :: VFQA a
     -> IO (Either VfilesError a)
crud = (iterM crudF) . runExceptT


--crudPGF :: VFAlgebra 'PG a -> IO (Either VfilesError a)
--crudPGF = undefined
--
--crudSum :: VFSum a -> IO (Either VfilesError a)
--crudSum (InL pg) = crudPG $ ExceptT pg
--crudSum (InR neo) = crudNeo $ ExceptT neo
--
--crud :: VFQA a -> IO (Either VfilesError a)
--crud = iterM crudSum
