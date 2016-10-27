{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module AcidDAO
       ( initialState
       ) where
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (get, put)
import           Data.Acid              (AcidState, Query, Update, makeAcidic,
                                         openLocalState)
import           Data.Acid.Advanced     (query', update')
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Data.Vector            (findIndex, empty)
import           Model

$(deriveSafeCopy 0 'base ''Status)
$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''TaskList)
$(deriveSafeCopy 0 'base ''Schedule)

addTask_ :: Id -> Name -> Update Schedule (Maybe Task)
addTask_ i name = do
    Schedule s <- get
    return $
        case findIndex ((==i).tlid) s of
            Nothing  -> Nothing
            Just fnd -> undefined --TODO

deleteTask_ :: Id -> Update Schedule (Maybe Task)
deleteTask_ = undefined

addTaskList_ :: Id -> Name -> Update Schedule (Maybe TaskList)
addTaskList_ i name = undefined

deleteTaskList_ :: Id -> Update Schedule (Maybe TaskList)
deleteTaskList_ tid = undefined

$(makeAcidic ''Schedule ['addTask_, 'deleteTask_, 'addTaskList_, 'deleteTaskList_])

instance DAO (AcidState Schedule) where
    addTask acid tid name = update' acid (AddTask_ tid name)

initialState::Schedule
initialState = Schedule empty
