{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}

module QueryAlgebras where

import Types

--------------------------------------------------------------------------------
-- | CRUD queries
--------------------------------------------------------------------------------

data Crudable = UserCrud
              | MediaCrud
              | VfileCrud
              | MediaVfileCrud
              | CommentCrud
                deriving (Show)

data SCrudable a where
  SUserCrud       :: SCrudable 'UserCrud
  SMediaCrud      :: SCrudable 'MediaCrud
  SVfileCrud      :: SCrudable 'VfileCrud
  SMediaVfileCrud :: SCrudable 'MediaVfileCrud
  SCommentCrud    :: SCrudable 'CommentCrud

type family New (c :: Crudable) :: * where
  New 'UserCrud = UserNew
  New 'MediaCrud = MediaNew
  New 'VfileCrud = VfileNew
  New 'MediaVfileCrud = MediaVfileNew
  New 'CommentCrud = CommentNew

type family Base (c :: Crudable) :: * where
  Base 'UserCrud = UserBase
  Base 'MediaCrud = MediaBase
  Base 'VfileCrud = VfileBase
  Base 'MediaVfileCrud = MediaVfileBase
  Base 'CommentCrud    = CommentBase

type family ID (c :: Crudable) :: * where
  ID 'UserCrud = UserId
  ID 'MediaCrud = MediaId
  ID 'VfileCrud = VfileId
  ID 'MediaVfileCrud = MediaVfileId
  ID 'CommentCrud = CommentId

data CrudF next :: * where
  CreateOp :: SCrudable c -> New c -> (Base c -> next) -> CrudF next
  ReadOp   :: SCrudable c -> ID c -> (Maybe (Base c) -> next) -> CrudF next
  UpdateOp :: SCrudable c -> Base c -> next -> CrudF next
  DeleteOp :: SCrudable c -> ID c -> next -> CrudF next

instance Functor CrudF where
  fmap f (CreateOp c n next) = CreateOp c n $ f . next
  fmap f (ReadOp c i next) = ReadOp c i $ f . next
  fmap f (UpdateOp c b next) = UpdateOp c b $ f next
  fmap f (DeleteOp c i next) = DeleteOp c i $ f next

--------------------------------------------------------------------------------
-- | Builder queries
--------------------------------------------------------------------------------
