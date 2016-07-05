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


getUsernameFromId :: UserId -> VFAlgebra 'PG Username
getUsernameFromId uId = username <$> readOp SUserCrud uId

-- TODO: should incorporate another Query algebra to the effect of
-- Object `getObjectBy` primaryKey
getPersonsMedias :: UserId -> VFAlgebra db [MediaBase cxt]
getPersonsMedias = undefined

ownsComment :: UserId -> CommentBase ct 'DB -> Bool
ownsComment uId com = (== uId) . commentOwner $ com

updateComment :: UserId -> CommentBase ct 'DB -> VFQA ()
updateComment uId com =
  if ownsComment uId com
  then do
    withPG $ updateOp (SCommentCrud (commentType com)) com
    withNeo $ updateOp (SCommentCrud (commentType com)) com
  else throwE $ VfilesError "User doesn't have permission to edit comment"

ownsMedia :: UserId -> MediaId -> VFAlgebra 'PG Bool
ownsMedia uId mId = do
  med <- readOp SMediaCrud mId
  return $ (== uId) . mediaOwner $ med

editMediaCaption :: UserId -> MediaId -> Maybe Caption -> VFQA ()
editMediaCaption uId mId cap = do
  canEdit <- withPG $ ownsMedia uId mId
  if canEdit
  then do
    media <- withPG $ readOp (SMediaCrud) mId
    withPG $ updateOp (SMediaCrud) $ media {mediaCaption = cap}
    withNeo $ updateOp (SMediaCrud) $ media {mediaCaption = cap}
  else
    throwE $ VfilesError "User doesn't have permission to edit caption"

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
