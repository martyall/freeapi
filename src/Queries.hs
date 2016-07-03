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
ownsComment :: UserId -> CommentBase ct 'DB -> VFAlgebra db Bool
ownsComment uId com = do
  case commentType com of
    SMediaComment -> do
      medId <- commentSourceId <$> readOp (SCommentCrud SMediaComment) (commentId com)
      med <- readOp SMediaCrud medId
      return $ (== uId) . mediaOwner $ med
    SMediaVfileComment -> do
      mvfId <- commentSourceId <$> readOp (SCommentCrud SMediaVfileComment) (commentId com)
      mvf <- readOp SMediaVfileCrud mvfId
      return $ (== uId) . mvfOwner $ mvf

updateComment :: UserId -> CommentBase ct 'DB -> VFQA ()
updateComment uId com = liftPG $ do
  canEdit <- ownsComment uId com
  if canEdit
  then updateOp (SCommentCrud (commentType com)) com
  else throwE (VfilesError "User doesn't have permission")

--------------------------------------------------------------------------------
-- | Neo4j CRUD Interpreter
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

--------------------------------------------------------------------------------
-- | PostgreSQL CRUD Interpreter
--------------------------------------------------------------------------------

crudPGF :: CrudF 'PG (IO (Either VfilesError a))
        -> IO (Either VfilesError a)
crudPGF = undefined

crudPG :: VFAlgebra 'PG a
       -> IO (Either VfilesError a)
crudPG = (iterM crudPGF) . runExceptT

--------------------------------------------------------------------------------
-- | "Vfiles Query Algebra" (VFQA) Interpreter
--------------------------------------------------------------------------------

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
