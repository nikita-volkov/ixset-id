{-# LANGUAGE DeriveDataTypeable, RecordWildCards, UndecidableInstances, GeneralizedNewtypeDeriving, FlexibleContexts #-}
module Data.IxSet.Id where

import Control.Monad.State
import Control.Applicative
import Data.IxSet as IxSet
import Data.SafeCopy
import Data.Data (Data, Typeable)



newtype Id a = Id {idValue :: Integer}
  deriving (Eq, Ord, Enum, Show, Data, Typeable, SafeCopy)



data Identified a = Identified {identifiedId :: Id a, identifiedValue :: a}
  deriving (Eq, Ord, Show, Data, Typeable)

instance (SafeCopy a) => SafeCopy (Identified a) where
  putCopy (Identified id value) = contain $ do
    safePut $ idValue id
    safePut value
  getCopy = contain $ Identified <$> (Id <$> safeGet) <*> safeGet



-- | Identified IxSet. A wrapper over IxSet managing the identification features
data IIS a = 
  IIS {
    iisNextId :: Id a,
    iisValue :: IxSet (Identified a)
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance (Indexable (Identified a), SafeCopy a, Typeable a, Ord a) => SafeCopy (IIS a) where
  putCopy IIS{..} = contain $ do
    safePut $ idValue iisNextId
    safePut $ IxSet.toList iisValue
  getCopy = contain $ 
    IIS 
      <$> (Id <$> safeGet)
      <*> (IxSet.fromList <$> safeGet)



emptyIIS :: (Indexable (Identified a)) => IIS a
emptyIIS = IIS (Id 1) IxSet.empty

insertIIS :: (Ord a, Typeable a, Indexable (Identified a)) =>
  a -> 
  IIS a -> (Identified a, IIS a)
insertIIS v = runState $ do
  IIS{..} <- get
  let identified = Identified iisNextId v
  put $ IIS
    (succ iisNextId)
    (IxSet.insert identified iisValue)
  return identified

-- | If a lookup succeeds, replace the match with a value, otherwise insert it.
replaceIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  k ->
  a -> 
  IIS a -> (Identified a, IIS a)
replaceIIS k v = runState $ do
  iis@IIS{..} <- get
  case getOne $ iisValue @= k of
    Just existing@(Identified id _) -> do
      let new = Identified id v
      put $ IIS iisNextId 
        $ IxSet.insert new
        $ IxSet.delete existing
        $ iisValue
      return new
    Nothing -> do
      let identified = Identified iisNextId v
      put $ IIS
        (succ iisNextId)
        (IxSet.insert identified iisValue)
      return identified

updateIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  k -> 
  (Maybe a -> a) -> 
  IIS a -> (Identified a, IIS a)
updateIIS k f = runState $ do
  iis@IIS{..} <- get
  case getOne $ iisValue @= k of
    Just identified@(Identified id value) -> do
      let newIdentified = Identified id $ f $ Just value
      put $ iis {
        iisValue = 
          IxSet.insert newIdentified $ IxSet.delete identified 
            $ iisValue
      }
      return $ newIdentified
    Nothing -> state $ insertIIS $ f Nothing

alterIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  k -> 
  (Maybe a -> Maybe a) -> 
  IIS a -> (Maybe (Identified a), IIS a)
alterIIS k f = runState $ do
  iis@IIS{..} <- get
  case getOne $ iisValue @= k of
    Just identified@(Identified id value) -> 
      case f $ Just value of
        Just newValue -> do
          let newIdentified = Identified id $ newValue
          put $ iis {
            iisValue = 
              IxSet.insert newIdentified $ IxSet.delete identified 
                $ iisValue
          }
          return $ Just newIdentified
        Nothing -> do
          put $ iis {
            iisValue = 
              IxSet.delete identified $ iisValue
          }
          return Nothing
    Nothing -> 
      case f Nothing of
        Just newValue -> fmap Just . state $ insertIIS newValue
        Nothing -> return Nothing

deleteIIS :: (Ord a, Typeable a, Indexable (Identified a)) =>
  Identified a ->
  IIS a -> IIS a
deleteIIS v iis = 
  iis {
    iisValue = IxSet.delete v $ iisValue iis
  }

deleteAtIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  k ->
  IIS a -> IIS a
deleteAtIIS k iis = 
  iis {
    iisValue = IxSet.deleteIx k $ iisValue iis
  }
