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


getUsernameFromId :: UserId -> PGCrud Username
getUsernameFromId uId = username <$> readPG SUserCrud uId

-- TODO: should incorporate another Query algebra to the effect of
-- Object `getObjectBy` primaryKey
getPersonsMedias :: UserId -> PGCrud [MediaBase cxt]
getPersonsMedias = undefined

ownsComment :: UserId -> CommentBase ct 'DB -> Bool
ownsComment uId com = (== uId) . commentOwner $ com

updateComment :: UserId -> CommentIdentifier ct -> Text -> VFCrud ()
updateComment uId comId@(CommentIdentifier _ comType) txt = do
  com <- liftPG $ readPG (SCommentCrud comType) comId
  if uId `ownsComment` com
  then do
    liftPG $ updatePG (SCommentCrud comType) $ com {commentText = txt}
    liftNeo $ updateNeo (SCommentCrud comType) $ com {commentText = txt}
  else throwE $ VfilesError "User doesn't have permission to edit comment"

ownsMedia :: UserId -> MediaBase 'DB -> Bool
ownsMedia uId med = (== uId) . mediaOwner $ med

editMediaCaption :: UserId -> MediaId -> Maybe Caption -> VFCrud ()
editMediaCaption uId mId cap = do
  media <- liftPG $ readPG SMediaCrud mId
  if uId `ownsMedia` media
  then do
    liftPG $ updatePG SMediaCrud $ media {mediaCaption = cap}
    liftNeo $ updateNeo SMediaCrud $ media {mediaCaption = cap}
  else
    throwE $ VfilesError "User doesn't have permission to edit caption"

ownsVfile :: UserId -> VfileBase 'DB -> Bool
ownsVfile uId vf = (== uId) . vfileOwner $ vf

createAndFileMedia :: UserId -> MediaVfileNew -> VFCrud (MediaVfileBase 'DB)
createAndFileMedia uId nMediaVf = do
  vf <- liftPG $ readPG SVfileCrud $ mvfVfile' nMediaVf
  if uId `ownsVfile` vf
  then do
    mediaVf <- liftPG $ createPG SMediaVfileCrud nMediaVf
    liftNeo $ createNeo SMediaVfileCrud mediaVf
    return  mediaVf
  else throwE $ VfilesError "User doesn't have permission to file media"

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
