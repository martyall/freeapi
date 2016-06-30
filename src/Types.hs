{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
--------------------------------------------------------------------------------
-- | Media
--------------------------------------------------------------------------------
newtype MediaId = MediaId Integer deriving (Eq, Show)

data MediaNew = MediaNew { mediaCaption'  :: Maybe Caption
                         , mediaRef'      :: Text
                         , mediaOwner'    :: UserId
                         } deriving (Eq, Show)

data MediaBase = MediaBase { mediaId      :: MediaId
                           , mediaCaption :: Maybe Caption
                           , mediaRef     :: Text
                           , mediaOwner   :: UserId
                           } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Vfile
--------------------------------------------------------------------------------
newtype VfileId = VfileId Integer deriving (Eq, Show)

data VfileNew = VfileNew { vfileCaption'  :: Maybe Caption
                         , vfileOwner'    :: UserBase
                         } deriving (Eq, Show)

data VfileBase = VfileBase { vfileId      :: VfileId
                           , vfileCaption :: Maybe Caption
                           , vfileOwner   :: UserId
                           } deriving (Eq, Show)

data Vfile = Vfile { vfileBase :: VfileBase
                   , medias    :: [MediaVfileBase]
                   } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | MediaVfile
--------------------------------------------------------------------------------
newtype MediaVfileId = MediaVfileId Integer deriving (Eq, Show)

data MediaVfileNew = MediaVfileNew { mvfCaption'  :: Maybe Caption
                                   , mvfVfile'    :: Vfile
                                   } deriving (Eq, Show)

data MediaVfileBase = MediaVfileBase { mvfId      :: MediaVfileId
                                     , mvfCaption :: Maybe Caption
                                     } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Comment
--------------------------------------------------------------------------------
newtype CommentId = CommentId Integer deriving (Eq, Show)

data CommentNew = CommentNew { commentText'  :: Text
                             , commentMedia' :: MediaId
                             } deriving (Eq, Show)

data CommentBase = CommentBase { commentId    :: CommentId
                               , commentText  :: Text
                               , commentMedia :: MediaId
                               } deriving (Eq, Show)

--------------------------------------------------------------------------------
newtype Caption = Caption Text deriving (Eq, Show)
