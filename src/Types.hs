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

data UserNew = UserNew { email'     :: Text
                       , username'  :: Text
                       } deriving (Eq, Show)

data UserBase = UserBase { userId   :: UserId
                         , email    :: Text
                         , username :: Text
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
                                       }

deriving instance (Eq (Context (MediaBase r) r), Eq (Context (VfileBase r) r))
  => Eq (MediaVfileBase r)
deriving instance (Show (Context (MediaBase r) r), Show (Context (VfileBase r) r))
  => Show (MediaVfileBase r)


data MVFIdentifier = MVFId MediaVfileId
                   | MVFPair (MediaId, VfileId)
                     deriving (Eq, Show)

type instance Context (MediaVfileBase r) DB = MediaVfileId
type instance Context (MediaVfileBase r) None = MediaVfileBase None
--------------------------------------------------------------------------------
-- | Comment
--------------------------------------------------------------------------------
data Comment = MediaComment | MediaVfileComment deriving (Eq, Show)

newtype CommentId = CommentId Integer deriving (Eq, Show)

data CommentNew = CommentNew { commentText'  :: Text
                             , commentMedia' :: MediaId
                             } deriving (Eq, Show)

data CommentBase men = CommentBase { commentId    :: CommentId
                                   , commentText  :: Text
                                   , commentItem  :: CommentSourceId men
                                   }

deriving instance Eq (CommentSourceId men) => Eq (CommentBase men)
deriving instance Show (CommentSourceId men) => Show (CommentBase men)

type family CommentSourceId (a :: Comment) :: *

type instance CommentSourceId 'MediaComment = MediaId
type instance CommentSourceId 'MediaVfileComment = MediaVfileId

data SComment c where
  SMediaComment :: SComment 'MediaComment
  SMediaVfileComment :: SComment 'MediaVfileComment

--------------------------------------------------------------------------------
newtype Caption = Caption Text deriving (Eq, Show)

newtype VfilesError = VfilesError Text deriving (Eq, Show)

data ResourceContext = DB | None deriving (Show)

type family Context (a :: *) (r :: ResourceContext) :: *

