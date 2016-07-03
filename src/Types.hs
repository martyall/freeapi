{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Data.Text (Text)


--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------
newtype UserId = UserId Integer deriving (Eq, Show)

newtype Username = Username Text deriving (Eq, Show)

data UserNew = UserNew { email'     :: Text
                       , username'  :: Username
                       } deriving (Eq, Show)

data UserBase = UserBase { userId   :: UserId
                         , email    :: Text
                         , username :: Username
                         } deriving (Eq, Show)

type instance Context UserBase DB = UserId
type instance Context UserBase None = UserBase
--------------------------------------------------------------------------------
-- | Media
--------------------------------------------------------------------------------
newtype MediaId = MediaId Integer deriving (Eq, Show)

data MediaNew = MediaNew { mediaCaption'  :: Maybe Caption
                         , mediaRef'      :: Text
                         , mediaOwner'    :: UserId
                         } deriving (Eq, Show)

data MediaBase r = MediaBase { mediaId      :: MediaId
                             , mediaCaption :: Maybe Caption
                             , mediaRef     :: Text
                             , mediaOwner   :: Context UserBase r
                             }

deriving instance Eq (Context UserBase r) => Eq (MediaBase r)
deriving instance Show (Context UserBase r) => Show (MediaBase r)

type instance Context (MediaBase r) DB = MediaId
type instance Context (MediaBase r) None = MediaBase None
--------------------------------------------------------------------------------
-- | Vfile
--------------------------------------------------------------------------------
newtype VfileId = VfileId Integer deriving (Eq, Show)

data VfileNew = VfileNew { vfileCaption'  :: Maybe Caption
                         , vfileOwner'    :: UserId
                         } deriving (Eq, Show)

data VfileBase r = VfileBase { vfileId      :: VfileId
                             , vfileCaption :: Maybe Caption
                             , vfileOwner   :: Context UserBase r
                             }

deriving instance Eq (Context UserBase r) => Eq (VfileBase r)
deriving instance Show (Context UserBase r) => Show (VfileBase r)


data Vfile r = Vfile { vfileBase :: Context (VfileBase r) r
                     , medias    :: [Context (MediaVfileBase r) r]
                     }

deriving instance (Eq (Context (MediaVfileBase r) r), Eq (Context (VfileBase r) r))
  => Eq (Vfile r)
deriving instance (Show (Context (MediaVfileBase r) r), Show (Context (VfileBase r) r))
  => Show (Vfile r)

type instance Context (VfileBase r) DB = VfileId
type instance Context (VfileBase r) None = VfileBase None
--------------------------------------------------------------------------------
-- | MediaVfile
--------------------------------------------------------------------------------
newtype MediaVfileId = MediaVfileId Integer deriving (Eq, Show)

data MediaVfileNew = MediaVfileNew { mvfCaption' :: Maybe Caption
                                   , mvfMedia'   :: MediaId
                                   , mvfVfile'   :: VfileId
                                   } deriving (Eq, Show)

data MediaVfileBase r = MediaVfileBase { mvfId      :: MediaVfileId
                                       , mvfCaption :: Maybe Caption
                                       , mvfMedia   :: Context (MediaBase r) r
                                       , mvfVfile   :: Context (VfileBase r) r
                                       , mvfOwner   :: Context UserBase r
                                       }

deriving instance ( Eq (Context (MediaBase r) r)
                  , Eq (Context (VfileBase r) r)
                  , Eq (Context UserBase r)
                  ) => Eq (MediaVfileBase r)
deriving instance ( Show (Context (MediaBase r) r)
                  , Show (Context (VfileBase r) r)
                  , Show (Context UserBase r)
                  ) => Show (MediaVfileBase r)

data MVFIdentifier = MVFId MediaVfileId
                   | MVFPair (MediaId, VfileId)
                     deriving (Eq, Show)

type instance Context (MediaVfileBase r) DB = MediaVfileId
type instance Context (MediaVfileBase r) None = MediaVfileBase None
--------------------------------------------------------------------------------
-- | Comment
--------------------------------------------------------------------------------
data CommentType = MediaComment | MediaVfileComment deriving (Eq, Show)

newtype CommentId = CommentId Integer deriving (Eq, Show)

data CommentNew ct = CommentNew { commentText'     :: Text
                                , commentSourceId' :: CommentSourceId ct
                                , commentOwner'    :: UserId
                                }

deriving instance Eq (CommentSourceId ct) => Eq (CommentNew ct)
deriving instance Show (CommentSourceId ct) => Show (CommentNew ct)

data CommentBase ct cxt = CommentBase { commentId        :: CommentId
                                      , commentText      :: Text
                                      , commentSourceId  :: CommentSourceId ct
                                      , commentOwner     :: Context UserBase cxt
                                      }

deriving instance ( Eq (CommentSourceId ct)
                  , Eq (Context UserBase cxt)
                  ) => Eq (CommentBase ct cxt)
deriving instance ( Show (CommentSourceId ct)
                  , Show (Context UserBase cxt)
                  ) => Show (CommentBase ct cxt)

type family CommentSourceId (a :: CommentType) :: *

type instance CommentSourceId 'MediaComment = MediaId
type instance CommentSourceId 'MediaVfileComment = MVFIdentifier

data SCommentType ct where
  SMediaComment :: SCommentType 'MediaComment
  SMediaVfileComment :: SCommentType 'MediaVfileComment

--------------------------------------------------------------------------------
-- | DB Types
--------------------------------------------------------------------------------

data DBType = PG | Neo deriving (Show)

data ResourceContext = DB | None deriving (Show)

data SResourceContext (cxt :: ResourceContext) where
  SDB   :: SResourceContext 'DB
  SNone :: SResourceContext 'None

type family Context (a :: *) (r :: ResourceContext) :: *

--------------------------------------------------------------------------------

newtype VfilesError = VfilesError Text deriving (Eq, Show)
newtype Caption = Caption Text deriving (Eq, Show)
