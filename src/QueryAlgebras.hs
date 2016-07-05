{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module QueryAlgebras where

import Types
import Control.Error
import Control.Monad.Free

--------------------------------------------------------------------------------
-- | Basic CRUD operations
--------------------------------------------------------------------------------

data VFCrudable = UserCrud
                | MediaCrud
                | VfileCrud
                | MediaVfileCrud
                | CommentCrud CommentType
                deriving (Show)

data SCrudable a where
  SUserCrud       :: SCrudable 'UserCrud
  SMediaCrud      :: SCrudable 'MediaCrud
  SVfileCrud      :: SCrudable 'VfileCrud
  SMediaVfileCrud :: SCrudable 'MediaVfileCrud
  SCommentCrud    :: SCommentType ct -> SCrudable ('CommentCrud ct)

type family NewData (c :: VFCrudable) :: * where
  NewData 'UserCrud = UserNew
  NewData 'MediaCrud = MediaNew
  NewData 'VfileCrud = VfileNew
  NewData 'MediaVfileCrud = MediaVfileNew
  NewData ('CommentCrud ct) = CommentNew ct

type family BaseData (c :: VFCrudable) :: * where
  BaseData 'UserCrud = UserBase
  BaseData 'MediaCrud = MediaBase 'DB
  BaseData 'VfileCrud = VfileBase 'DB
  BaseData 'MediaVfileCrud = MediaVfileBase 'DB
  BaseData ('CommentCrud ct) = CommentBase ct 'DB

type family ReadData (c :: VFCrudable) :: * where
  ReadData 'UserCrud = UserId
  ReadData 'MediaCrud = MediaId
  ReadData 'VfileCrud = VfileId
  ReadData 'MediaVfileCrud = MVFIdentifier
  ReadData ('CommentCrud ct) = CommentId

data CrudF (db :: DBType) next :: * where
  CreateOp :: SCrudable c
           -> NewData c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF db next
  ReadOp   :: SCrudable c
           -> ReadData c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF 'PG next
  UpdateOp :: SCrudable c
           -> BaseData c
           -> (Either VfilesError () -> next)
           -> CrudF db next
  DeleteOp :: SCrudable c
           -> ReadData c
           -> (Either VfilesError () -> next)
           -> CrudF db next

instance Functor (CrudF db) where
  fmap f (CreateOp c n next) = CreateOp c n $ f . next
  fmap f (ReadOp c i next) = ReadOp c i $ f . next
  fmap f (UpdateOp c b next) = UpdateOp c b $ f . next
  fmap f (DeleteOp c i next) = DeleteOp c i $ f . next

type VFAlgebra db a = ExceptT VfilesError (Free (CrudF db)) a

--------------------------------------------------------------------------------
-- | Smart Constructors
--------------------------------------------------------------------------------

createOp :: SCrudable c -> NewData c -> VFAlgebra db (BaseData c)
createOp SUserCrud n = ExceptT . Free $ CreateOp SUserCrud n  Pure
createOp SMediaCrud n = ExceptT . Free $ CreateOp SMediaCrud n Pure
createOp SVfileCrud n = ExceptT . Free $ CreateOp SVfileCrud n Pure
createOp SMediaVfileCrud n = ExceptT . Free $ CreateOp SMediaVfileCrud n Pure
createOp (SCommentCrud ct) n = ExceptT . Free $ CreateOp (SCommentCrud ct) n Pure

readOp :: SCrudable c -> ReadData c -> VFAlgebra 'PG (BaseData c)
readOp SUserCrud r = ExceptT . Free $ ReadOp SUserCrud r Pure
readOp SMediaCrud r = ExceptT . Free $ ReadOp SMediaCrud r Pure
readOp SVfileCrud r = ExceptT . Free $ ReadOp SVfileCrud r Pure
readOp SMediaVfileCrud r = ExceptT . Free $ ReadOp SMediaVfileCrud r Pure
readOp (SCommentCrud ct) r = ExceptT . Free $ ReadOp (SCommentCrud ct) r Pure

updateOp :: SCrudable c -> BaseData c -> VFAlgebra db ()
updateOp SUserCrud b = ExceptT . Free $ UpdateOp SUserCrud b Pure
updateOp SMediaCrud b = ExceptT . Free $ UpdateOp SMediaCrud b Pure
updateOp SVfileCrud b = ExceptT . Free $ UpdateOp SVfileCrud b Pure
updateOp SMediaVfileCrud b = ExceptT . Free $ UpdateOp SMediaVfileCrud b Pure
updateOp (SCommentCrud ct) b = ExceptT . Free $ UpdateOp (SCommentCrud ct) b Pure

deleteOp :: SCrudable c -> ReadData c -> VFAlgebra db ()
deleteOp SUserCrud r = ExceptT . Free $ DeleteOp SUserCrud r Pure
deleteOp SMediaCrud r = ExceptT . Free $ DeleteOp SMediaCrud r Pure
deleteOp SVfileCrud r = ExceptT . Free $ DeleteOp SVfileCrud r Pure
deleteOp SMediaVfileCrud r = ExceptT . Free $ DeleteOp SMediaVfileCrud r Pure
deleteOp (SCommentCrud ct) r = ExceptT . Free $ DeleteOp (SCommentCrud ct) r Pure

--------------------------------------------------------------------------------
-- | Composite Query Algebra
--------------------------------------------------------------------------------

data VFSum a = InL (CrudF 'PG a)
             | InR (CrudF 'Neo a)

instance Functor VFSum where
  fmap f (InL a) = InL (fmap f a)
  fmap f (InR a) = InR (fmap f a)

type VFQA a = ExceptT VfilesError (Free VFSum) a

withPG :: forall a. VFAlgebra 'PG a -> VFQA a
withPG = mapExceptT $ hoistFree InL

withNeo :: forall a. VFAlgebra 'Neo a -> VFQA a
withNeo = mapExceptT $ hoistFree InR
