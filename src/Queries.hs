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


---- TODO: should incorporate another Query algebra to the effect of
---- Object `getObjectBy` primaryKey
--getPersonsMedias :: UserId -> PGCrud [MediaBase cxt]
--getPersonsMedias = undefined

updateComment :: UserId -> CommentIdentifier ct -> Text -> VFCrud (CommentBase ct 'DB)
updateComment userId comId@(CommentIdentifier _ comType) txt = do
  let desiredKey = CrudKey RWP (SCommentCrud comType)
  commentKey <- liftPG $ requestKey desiredKey userId comId
  com <- liftPG $ readPG commentKey comId
  com <- liftPG $ updatePG commentKey $ com {commentText = txt}
  liftNeo $ updateNeo commentKey $ com {commentText = txt}
  return com

editMediaCaption :: UserId -> MediaId -> Maybe Caption -> PGCrud (MediaBase 'DB)
editMediaCaption userId mediaId cap = do
  mediaKey <- requestKey (CrudKey RWP SMediaCrud) userId mediaId
  media <- readPG mediaKey mediaId
  updatePG mediaKey $ media {mediaCaption = cap}

createAndFileMedia :: UserId -> MediaVfileNew -> VFCrud (MediaVfileBase 'DB)
createAndFileMedia userId nMediaVf = do
  mvfKey <- liftPG $ insertKey (CrudKey WP SMediaVfileCrud) userId nMediaVf
  mediaVf <- liftPG $ createPG mvfKey nMediaVf
  liftNeo $ createNeo mvfKey mediaVf
  return mediaVf

--------------------------------------------------------------------------------
-- | Neo4j CRUD Interpreter
--------------------------------------------------------------------------------

-- withConn :: Neo4j a -> IO a
-- withConn = withConnection "localhost" 7474

-- runQuery :: Text -> Params -> IO (Either TransError Result)
-- runQuery q params = withConn (runTransaction $ cypher q params)

crudNeoF :: NeoCrudF (IO (Either VfilesError a))
         -> IO (Either VfilesError a)
crudNeoF = undefined

crudNeo :: NeoCrud a
        -> IO (Either VfilesError a)
crudNeo = (iterM crudNeoF) . runExceptT

--------------------------------------------------------------------------------
-- | PostgreSQL CRUD Interpreter
--------------------------------------------------------------------------------

crudPGF :: PGCrudF (IO (Either VfilesError a))
        -> IO (Either VfilesError a)
crudPGF = undefined

crudPG :: PGCrud a
       -> IO (Either VfilesError a)
crudPG = (iterM crudPGF) . runExceptT

--------------------------------------------------------------------------------
-- | "Vfiles Query Algebra" (VFQA) Interpreter
--------------------------------------------------------------------------------

crudF :: VFCrudF (IO (Either VfilesError a))
      -> IO (Either VfilesError a)
crudF (InPGCrud pg) = crudPGF pg
crudF (InNeoCrud neo) = crudNeoF neo

crud :: VFCrud a
     -> IO (Either VfilesError a)
crud = (iterM crudF) . runExceptT
