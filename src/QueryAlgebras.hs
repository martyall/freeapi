{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}

module QueryAlgebras where

import Types

--------------------------------------------------------------------------------
-- | VFCRUD types
--------------------------------------------------------------------------------

data VFCrudable = UserCrud
                | MediaCrud
                | VfileCrud
                | MediaVfileCrud
                | CommentCrud Comment
                deriving (Show)

data SCrudable a where
  SUserCrud       :: SCrudable 'UserCrud
  SMediaCrud      :: SCrudable 'MediaCrud
  SVfileCrud      :: SCrudable 'VfileCrud
  SMediaVfileCrud :: SCrudable 'MediaVfileCrud
  SCommentCrud    :: SComment com -> SCrudable ('CommentCrud com)

type family NewData (c :: VFCrudable) :: * where
  NewData 'UserCrud = UserNew
  NewData 'MediaCrud = MediaNew
  NewData 'VfileCrud = VfileNew
  NewData 'MediaVfileCrud = MediaVfileNew
  NewData ('CommentCrud com) = CommentNew

type family BaseData (c :: VFCrudable) :: * where
  BaseData 'UserCrud = UserBase
  BaseData 'MediaCrud = MediaBase 'DB
  BaseData 'VfileCrud = VfileBase 'DB
  BaseData 'MediaVfileCrud = MediaVfileBase 'DB
  BaseData ('CommentCrud com) = CommentBase com

type family ReadData (c :: VFCrudable) :: * where
  ReadData 'UserCrud = UserId
  ReadData 'MediaCrud = MediaId
  ReadData 'VfileCrud = VfileId
  ReadData 'MediaVfileCrud = MVFIdentifier
  ReadData ('CommentCrud com) = CommentId

data CrudF next :: * where
  CreateOp :: SCrudable c
           -> NewData c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF next
  ReadOp   :: SCrudable c
           -> ReadData c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF next
  UpdateOp :: SCrudable c
           -> BaseData c
           -> (Either VfilesError () -> next)
           -> CrudF next
  DeleteOp :: SCrudable c
           -> ReadData c
           -> (Either VfilesError () -> next)
           -> CrudF next

instance Functor CrudF where
  fmap f (CreateOp c n next) = CreateOp c n $ f . next
  fmap f (ReadOp c i next) = ReadOp c i $ f . next
  fmap f (UpdateOp c b next) = UpdateOp c b $ f . next
  fmap f (DeleteOp c i next) = DeleteOp c i $ f . next

--------------------------------------------------------------------------------
-- | 
--------------------------------------------------------------------------------
