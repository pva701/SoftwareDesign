{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module AcidDAO
       ( initialState
       ) where
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, modify, put, when)
import           Data.Acid            (AcidState, Query, Update, makeAcidic)
import           Data.Acid.Advanced   (query', update')
import           Data.Map             as Map (Map, delete, empty, insert, keysSet, lookup,
                                              size)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Model

$(deriveSafeCopy 0 'base ''Status)
$(deriveSafeCopy 0 'base ''Task)
$(deriveSafeCopy 0 'base ''TaskList)
$(deriveSafeCopy 0 'base ''Schedule)

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

deleteTaskList_ :: Id -> Update Schedule (Maybe TaskList)
deleteTaskList_ tId = do
    Schedule s <- get
    case Map.lookup tId s of
        Nothing      -> return Nothing
        Just (tl, _) -> do
            put (Schedule $ Map.delete tId s)
            return $ Just tl
getKey::Map Id a -> Id
getKey x = if size x == 0 then 1 else (+1) $ maximum $ Map.keysSet x

addTask_ :: Id -> Name -> Update Schedule (Maybe Task)
addTask_ tId nm = do
    Schedule s <- get
    case Map.lookup tId s of
        Nothing          -> return Nothing
        Just (tl, tasks) -> do
            let key = getKey tasks
            let task = Task key nm NotCompleted
            let newTasks = Map.insert key task tasks
            put $ Schedule $ Map.insert tId (tl, newTasks) s
            return $ Just task

addTaskList_ :: Name -> Update Schedule (Maybe TaskList)
addTaskList_ tName = do
    Schedule s <- get
    let ind = getKey s
    let tlist = TaskList ind tName
    let newSchedule = Schedule $ insert ind (tlist, empty) s
    put newSchedule
    return $ Just tlist

getSchedule_::Query Schedule Schedule
getSchedule_ = ask

completeTask_::Id -> Id -> Update Schedule (Maybe Task)
completeTask_ tlid tid = do
    Schedule s <- get
    case Map.lookup tlid s of
        Nothing          -> return Nothing
        Just (tl, tasks) ->
            case Map.lookup tid tasks of
                Nothing   -> return Nothing
                Just (Task _id _name _) -> do
                    let newTask = Task _id _name Completed
                    let newTasks = Map.insert _id newTask $ Map.delete tid tasks
                    put $ Schedule $ Map.insert tlid (tl, newTasks) s
                    return $ Just newTask

$(makeAcidic ''Schedule ['addTask_, 'deleteTask_, 'addTaskList_, 'deleteTaskList_, 'getSchedule_, 'completeTask_])

instance DAO (AcidState Schedule) where
    addTask acid tid name = update' acid (AddTask_ tid name)
    addTaskList acid name = update' acid (AddTaskList_ name)
    deleteTask acid tlid tid = update' acid (DeleteTask_ tlid tid)
    deleteTaskList acid tlid = update' acid (DeleteTaskList_ tlid)
    getSchedule acid = query' acid GetSchedule_
    completeTask acid tlid tid = update' acid (CompleteTask_ tlid tid)

initialState::Schedule
initialState = Schedule empty
