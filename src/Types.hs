{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies, GADTs, DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}


module Types where

import Data.Text (Text)
import Data.Singletons.TH hiding ((:<))
import Data.Kind



--------------------------------------------------------------------------------
-- | DB Types
--------------------------------------------------------------------------------

data DBType = PG | Neo

data ResourceContext = DB | None

type family Context (a :: *) (r :: ResourceContext) :: *

--------------------------------------------------------------------------------

newtype VfilesError = VfilesError Text
newtype Caption = Caption Text

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------
newtype UserId = UserId Integer deriving Eq

newtype Username = Username Text

data UserNew = UserNew { email'     :: Text
                       , username'  :: Username
                       }

data UserBase = UserBase { userId   :: UserId
                         , email    :: Text
                         , username :: Username
                         }

type instance Context UserBase DB = UserId
type instance Context UserBase None = UserBase
--------------------------------------------------------------------------------
-- | Media
--------------------------------------------------------------------------------
newtype MediaId = MediaId Integer

data MediaNew = MediaNew { mediaCaption'  :: Maybe Caption
                         , mediaRef'      :: Text
                         , mediaOwner'    :: UserId
                         }

data MediaBase r = MediaBase { mediaId      :: MediaId
                             , mediaCaption :: Maybe Caption
                             , mediaRef     :: Text
                             , mediaOwner   :: Context UserBase r
                             }

type instance Context (MediaBase r) DB = MediaId
type instance Context (MediaBase r) None = MediaBase None
--------------------------------------------------------------------------------
-- | Vfile
--------------------------------------------------------------------------------
newtype VfileId = VfileId Integer

data VfileNew = VfileNew { vfileCaption'  :: Maybe Caption
                         , vfileOwner'    :: UserId
                         }

data VfileBase r = VfileBase { vfileId      :: VfileId
                             , vfileCaption :: Maybe Caption
                             , vfileOwner   :: Context UserBase r
                             }


data Vfile r = Vfile { vfileBase :: Context (VfileBase r) r
                     , medias    :: [Context (MediaVfileBase r) r]
                     }

type instance Context (VfileBase r) DB = VfileId
type instance Context (VfileBase r) None = VfileBase None
--------------------------------------------------------------------------------
-- | MediaVfile
--------------------------------------------------------------------------------
newtype MediaVfileId = MediaVfileId Integer

data MediaVfileNew = MediaVfileNew { mvfCaption' :: Maybe Caption
                                   , mvfMedia'   :: MediaId
                                   , mvfVfile'   :: VfileId
                                   }

data MediaVfileBase r = MediaVfileBase { mvfId      :: MediaVfileId
                                       , mvfCaption :: Maybe Caption
                                       , mvfMedia   :: Context (MediaBase r) r
                                       , mvfVfile   :: Context (VfileBase r) r
                                       , mvfOwner   :: Context UserBase r
                                       }

data MVFIdentifier = MVFId MediaVfileId
                   | MVFPair MediaId VfileId

type instance Context (MediaVfileBase r) DB = MediaVfileId
type instance Context (MediaVfileBase r) None = MediaVfileBase None
--------------------------------------------------------------------------------
-- | Comment
--------------------------------------------------------------------------------
data CommentType = MediaComment
                 | MediaVfileComment
                   deriving (Eq, Show)

genSingletons [ ''CommentType ]

newtype CommentId = CommentId Integer

data CommentNew (ct :: CommentType) =
  CommentNew { commentText'     :: Text
             , commentType'     :: Sing ct
             , commentSourceId' :: CommentSourceId ct
             , commentOwner'    :: UserId
             }

data CommentBase (ct :: CommentType) (cxt :: ResourceContext) =
  CommentBase { commentId        :: CommentId
              , commentText      :: Text
              , commentType      :: Sing ct
              , commentSourceId  :: CommentSourceId ct
              , commentOwner     :: Context UserBase cxt
              }

data CommentIdentifier (ct :: CommentType) = CommentIdentifier Integer (Sing ct)

type family CommentSourceId (a :: CommentType) :: *

type instance CommentSourceId 'MediaComment = MediaId
type instance CommentSourceId 'MediaVfileComment = MVFIdentifier

--------------------------------------------------------------------------------
-- | Permissions
--------------------------------------------------------------------------------
data VFCrudable = UserCrud
                | MediaCrud
                | VfileCrud
                | MediaVfileCrud
                | CommentCrud CommentType
                deriving (Show)

genSingletons [ ''VFCrudable ]

data CrudKey (perms :: [Perms]) (c :: VFCrudable) =
  CrudKey (Permissions perms) (Sing c)

data Perms = R | W | D deriving (Eq)

data Permissions (perms :: [Perms]) where
  RWDP   :: Permissions '[ 'R, 'W, 'D ]
  RWP    :: Permissions '[ 'R, 'W ]
  RP     :: Permissions '[ 'R ]
  WP     :: Permissions '[ 'W ]
  EmptyP :: Permissions '[]

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem x '[] = False
  Elem x (x ': xs) = True
  Elem x (y ': xs) = Elem x xs
