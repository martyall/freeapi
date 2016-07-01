{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}

module QueryAlgebras where

import Types
import Control.Monad.Free

--------------------------------------------------------------------------------
-- | VFCRUD types
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
  SCommentCrud    :: SComment ct -> SCrudable ('CommentCrud ct)

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
  BaseData ('CommentCrud ct) = CommentBase ct

type family ReadData (c :: VFCrudable) :: * where
  ReadData 'UserCrud = UserId
  ReadData 'MediaCrud = MediaId
  ReadData 'VfileCrud = VfileId
  ReadData 'MediaVfileCrud = MVFIdentifier
  ReadData ('CommentCrud ct) = CommentId

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

type VFAlgebra a = Free CrudF (Either VfilesError a)

--------------------------------------------------------------------------------
-- | Smart Constructors
--------------------------------------------------------------------------------

createOp :: SCrudable c -> NewData c -> VFAlgebra (BaseData c)
createOp SUserCrud n = Free $ CreateOp SUserCrud n  Pure
createOp SMediaCrud n = Free $ CreateOp SMediaCrud n Pure
createOp SVfileCrud n = Free $ CreateOp SVfileCrud n Pure
createOp SMediaVfileCrud n = Free $ CreateOp SMediaVfileCrud n Pure
createOp (SCommentCrud ct) n = Free $ CreateOp (SCommentCrud ct) n Pure

readOp :: SCrudable c -> ReadData c -> VFAlgebra (BaseData c)
readOp SUserCrud r = Free $ ReadOp SUserCrud r Pure
readOp SMediaCrud r = Free $ ReadOp SMediaCrud r Pure
readOp SVfileCrud r = Free $ ReadOp SVfileCrud r Pure
readOp SMediaVfileCrud r = Free $ ReadOp SMediaVfileCrud r Pure
readOp (SCommentCrud ct) r = Free $ ReadOp (SCommentCrud ct) r Pure

updateOp :: SCrudable c -> BaseData c -> VFAlgebra ()
updateOp SUserCrud b = Free $ UpdateOp SUserCrud b Pure
updateOp SMediaCrud b = Free $ UpdateOp SMediaCrud b Pure
updateOp SVfileCrud b = Free $ UpdateOp SVfileCrud b Pure
updateOp SMediaVfileCrud b = Free $ UpdateOp SMediaVfileCrud b Pure
updateOp (SCommentCrud ct) b = Free $ UpdateOp (SCommentCrud ct) b Pure

deleteOp :: SCrudable c -> ReadData c -> VFAlgebra ()
deleteOp SUserCrud r = Free $ DeleteOp SUserCrud r Pure
deleteOp SMediaCrud r = Free $ DeleteOp SMediaCrud r Pure
deleteOp SVfileCrud r = Free $ DeleteOp SVfileCrud r Pure
deleteOp SMediaVfileCrud r = Free $ DeleteOp SMediaVfileCrud r Pure
deleteOp (SCommentCrud ct) r = Free $ DeleteOp (SCommentCrud ct) r Pure
