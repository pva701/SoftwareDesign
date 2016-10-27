{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module AcidDAO
       ( initialState
       ) where
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (get, modify, put, when)
import           Data.Acid              (AcidState, Query, Update, makeAcidic,
                                         openLocalState)
import           Data.Acid.Advanced     (query', update')
import           Data.Map               as Map (Map, delete, empty, insert, lookup,
                                                member)
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Model
import Data.Hashable

$(deriveSafeCopy 0 'base ''Status)
$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''TaskList)
$(deriveSafeCopy 0 'base ''Schedule)

addTask_ :: Id -> Name -> Update Schedule (Maybe Task)
addTask_ tId nm = do
    Schedule s <- get
    case Map.lookup tId s of
        Nothing          -> return Nothing
        Just (tl, tasks) -> do
            let key = hash nm
            let task = Task key nm NotCompleted
            let newTasks = Map.insert key task tasks
            put $ Schedule $ Map.insert tId (tl, newTasks) s
            return $ Just task

deleteTask_ :: Id -> Id -> Update Schedule (Maybe Task)
deleteTask_ tlid tid = do
    Schedule s <- get
    case Map.lookup tlid s of
        Nothing          -> return Nothing
        Just (tl, tasks) ->
            case Map.lookup tid tasks of
                Nothing   -> return Nothing
                Just task -> do
                    let newTasks = Map.delete tid tasks
                    put $ Schedule $ Map.insert tlid (tl, newTasks) s
                    return $ Just task

addTaskList_ :: Name -> Update Schedule (Maybe TaskList)
addTaskList_ tName = do
    Schedule s <- get
    let ind = hash tName
    let tlist = TaskList ind tName
    let newSchedule = Schedule $ insert ind (tlist, empty) s
    put newSchedule
    return $ Just tlist

deleteTaskList_ :: Id -> Update Schedule (Maybe TaskList)
deleteTaskList_ tId = do
    Schedule s <- get
    case Map.lookup tId s of
        Nothing      -> return Nothing
        Just (tl, _) -> do
            put (Schedule $ Map.delete tId s)
            return $ Just tl

getSchedule_::Query Schedule Schedule
getSchedule_ = ask

$(makeAcidic ''Schedule ['addTask_, 'deleteTask_, 'addTaskList_, 'deleteTaskList_, 'getSchedule_])

instance DAO (AcidState Schedule) where
    addTask acid tid name = update' acid (AddTask_ tid name)
    addTaskList acid name = update' acid (AddTaskList_ name)
    deleteTask acid tlid tid = update' acid (DeleteTask_ tlid tid)
    deleteTaskList acid tlid = update' acid (DeleteTaskList_ tlid)
    getSchedule acid = query' acid GetSchedule_

initialState::Schedule
initialState = Schedule empty
