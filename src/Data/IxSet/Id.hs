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



data IdentifiedIxSet a = 
  IdentifiedIxSet {
    identifiedIxSetNextId :: Id a,
    identifiedIxSetValue :: IxSet (Identified a)
  }
  deriving (Eq, Ord, Show, Data, Typeable)

instance (Indexable (Identified a), SafeCopy a, Typeable a, Ord a) => SafeCopy (IdentifiedIxSet a) where
  putCopy IdentifiedIxSet{..} = contain $ do
    safePut $ idValue identifiedIxSetNextId
    safePut $ IxSet.toList identifiedIxSetValue
  getCopy = contain $ 
    IdentifiedIxSet 
      <$> (Id <$> safeGet)
      <*> (IxSet.fromList <$> safeGet)



emptyIIS :: (Indexable (Identified a)) => IdentifiedIxSet a
emptyIIS = IdentifiedIxSet (Id 1) IxSet.empty

insertIIS :: (Ord a, Typeable a, Indexable (Identified a)) =>
  a -> IdentifiedIxSet a -> (Identified a, IdentifiedIxSet a)
insertIIS v = runState $ do
  IdentifiedIxSet{..} <- get
  let identified = Identified identifiedIxSetNextId v
  put $ IdentifiedIxSet
    (succ identifiedIxSetNextId)
    (IxSet.insert identified identifiedIxSetValue)
  return identified

-- | If a lookup succeeds, replace the match with a value, otherwise insert it.
replaceIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  a -> k ->
  IdentifiedIxSet a -> (Identified a, IdentifiedIxSet a)
replaceIIS v k = runState $ do
  iis@IdentifiedIxSet{..} <- get
  case getOne $ identifiedIxSetValue @= k of
    Just existing@(Identified id _) -> do
      let new = Identified id v
      put $ IdentifiedIxSet identifiedIxSetNextId 
        $ IxSet.insert new
        $ IxSet.delete existing
        $ identifiedIxSetValue
      return new
    Nothing -> do
      let identified = Identified identifiedIxSetNextId v
      put $ IdentifiedIxSet
        (succ identifiedIxSetNextId)
        (IxSet.insert identified identifiedIxSetValue)
      return identified

alterIIS :: (Ord a, Typeable a, Indexable (Identified a), Typeable k) =>
  (Maybe a -> Maybe a) -> 
  k -> 
  IdentifiedIxSet a -> (Maybe (Identified a), IdentifiedIxSet a)
alterIIS f k = runState $ do
  iis@IdentifiedIxSet{..} <- get
  case getOne $ identifiedIxSetValue @= k of
    Just identified@(Identified id value) -> 
      case f $ Just value of
        Just newValue -> do
          let newIdentified = Identified id $ newValue
          put $ iis {
            identifiedIxSetValue = 
              IxSet.insert newIdentified $ IxSet.delete identified 
                $ identifiedIxSetValue
          }
          return $ Just newIdentified
        Nothing -> do
          put $ iis {
            identifiedIxSetValue = 
              IxSet.delete identified $ identifiedIxSetValue
          }
          return Nothing
    Nothing -> 
      case f Nothing of
        Just newValue -> fmap Just . state $ insertIIS newValue
        Nothing -> return Nothing
