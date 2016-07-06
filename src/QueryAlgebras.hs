{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell#-}

module QueryAlgebras where

import Types
import Control.Error
import Control.Monad.Free
import Data.Singletons.TH

--------------------------------------------------------------------------------
-- | Basic CRUD operations
--------------------------------------------------------------------------------

data VFCrudable = UserCrud
                | MediaCrud
                | VfileCrud
                | MediaVfileCrud
                | CommentCrud CommentType
                deriving (Show)

genSingletons [ ''VFCrudable ]

type family NewData (db :: DBType) (c :: VFCrudable) :: * where
  NewData 'PG 'UserCrud = UserNew
  NewData 'PG 'MediaCrud = MediaNew
  NewData 'PG 'VfileCrud = VfileNew
  NewData 'PG 'MediaVfileCrud = MediaVfileNew
  NewData 'PG ('CommentCrud ct) = CommentNew ct
  NewData 'Neo 'UserCrud = UserBase
  NewData 'Neo 'MediaCrud = MediaBase 'DB
  NewData 'Neo 'VfileCrud = VfileBase 'DB
  NewData 'Neo 'MediaVfileCrud = MediaVfileBase 'DB
  NewData 'Neo ('CommentCrud ct) = CommentBase ct 'DB

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
  CreateOp :: Sing c
           -> NewData db c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF db next
  ReadOp   :: Sing c
           -> ReadData c
           -> (Either VfilesError (BaseData c) -> next)
           -> CrudF 'PG next
  UpdateOp :: Sing c
           -> BaseData c
           -> (Either VfilesError () -> next)
           -> CrudF db next
  DeleteOp :: Sing c
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

createOp :: Sing c -> NewData db c -> VFAlgebra db (BaseData c)
createOp c n = ExceptT . Free $ CreateOp c n Pure

readOp :: Sing c -> ReadData c -> VFAlgebra 'PG (BaseData c)
readOp c n = ExceptT . Free $ ReadOp c n Pure

updateOp :: Sing c -> BaseData c -> VFAlgebra db ()
updateOp c n = ExceptT . Free $ UpdateOp c n Pure

deleteOp :: Sing c -> ReadData c -> VFAlgebra db ()
deleteOp c n = ExceptT . Free $ DeleteOp c n Pure

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
