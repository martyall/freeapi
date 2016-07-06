{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell#-}

module QueryAlgebras where

import Types
import Control.Error
import Control.Monad.Free
import Data.Singletons.TH


data VFCrudable = UserCrud
                | MediaCrud
                | VfileCrud
                | MediaVfileCrud
                | CommentCrud CommentType
                deriving (Show)

genSingletons [ ''VFCrudable ]

type family NewData (c :: VFCrudable) :: * where
  NewData 'UserCrud  = UserNew
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

--------------------------------------------------------------------------------
-- | Basic PG-CRUD operations
--------------------------------------------------------------------------------

data PGCrudF next :: * where
  CreatePG :: Sing (c :: VFCrudable)
           -> NewData c
           -> (Either VfilesError (BaseData c) -> next)
           -> PGCrudF next
  ReadPG   :: Sing (c :: VFCrudable)
           -> ReadData c
           -> (Either VfilesError (BaseData c) -> next)
           -> PGCrudF next
  UpdatePG :: Sing (c :: VFCrudable)
           -> BaseData c
           -> (Either VfilesError () -> next)
           -> PGCrudF next
  DeletePG :: Sing (c :: VFCrudable)
           -> ReadData c
           -> (Either VfilesError () -> next)
           -> PGCrudF next

instance Functor PGCrudF where
  fmap f (CreatePG c n next) = CreatePG c n $ f . next
  fmap f (ReadPG c i next) = ReadPG c i $ f . next
  fmap f (UpdatePG c b next) = UpdatePG c b $ f . next
  fmap f (DeletePG c i next) = DeletePG c i $ f . next

type PGCrud a = ExceptT VfilesError (Free PGCrudF) a

--------------------------------------------------------------------------------
-- | Smart PG-CRUD Constructors
--------------------------------------------------------------------------------

createPG :: Sing (c :: VFCrudable) -> NewData c -> PGCrud (BaseData c)
createPG c n = ExceptT . Free $ CreatePG c n Pure

readPG :: Sing (c :: VFCrudable) -> ReadData c -> PGCrud (BaseData c)
readPG c n = ExceptT . Free $ ReadPG c n Pure

updatePG :: Sing (c :: VFCrudable) -> BaseData c -> PGCrud ()
updatePG c n = ExceptT . Free $ UpdatePG c n Pure

deletePG :: Sing (c :: VFCrudable) -> ReadData c -> PGCrud ()
deletePG c n = ExceptT . Free $ DeletePG c n Pure

--------------------------------------------------------------------------------
-- | Basic Neo-CRUD operations
--------------------------------------------------------------------------------

data NeoCrudF next :: * where
  CreateNeo :: Sing (c :: VFCrudable)
            -> BaseData c
            -> (Either VfilesError () -> next)
            -> NeoCrudF next
  UpdateNeo :: Sing (c :: VFCrudable)
            -> BaseData c
            -> (Either VfilesError () -> next)
            -> NeoCrudF next
  DeleteNeo :: Sing (c :: VFCrudable)
            -> ReadData c
            -> (Either VfilesError () -> next)
            -> NeoCrudF next

instance Functor NeoCrudF where
  fmap f (CreateNeo c n next) = CreateNeo c n $ f . next
  fmap f (UpdateNeo c b next) = UpdateNeo c b $ f . next
  fmap f (DeleteNeo c i next) = DeleteNeo c i $ f . next

type NeoCrud a = ExceptT VfilesError (Free NeoCrudF) a

--------------------------------------------------------------------------------
-- | Smart Neo-CRUD Constructors
--------------------------------------------------------------------------------

createNeo :: Sing (c :: VFCrudable) -> BaseData c -> NeoCrud ()
createNeo c n = ExceptT . Free $ CreateNeo c n Pure

updateNeo :: Sing (c :: VFCrudable) -> BaseData c -> NeoCrud ()
updateNeo c n = ExceptT . Free $ UpdateNeo c n Pure

deleteNeo :: Sing (c :: VFCrudable) -> ReadData c -> NeoCrud ()
deleteNeo c n = ExceptT . Free $ DeleteNeo c n Pure

--------------------------------------------------------------------------------
-- | Composite Query Algebra
--------------------------------------------------------------------------------

data VFSum a = InPGCrud (PGCrudF a)
             | InNeoCrud (NeoCrudF a)

instance Functor VFSum where
  fmap f (InPGCrud a) = InPGCrud (fmap f a)
  fmap f (InNeoCrud a) = InNeoCrud (fmap f a)

type VFCrud a = ExceptT VfilesError (Free VFSum) a

withPG :: forall a. PGCrud a -> VFCrud a
withPG = mapExceptT $ hoistFree InPGCrud

withNeo :: forall a. NeoCrud a -> VFCrud a
withNeo = mapExceptT $ hoistFree InNeoCrud
