{-# LANGUAGE DeriveDataTypeable #-}
module Model
       ( Id
       , Name
       , Status(..)
       , Task(..)
       , TaskList(..)
       , Schedule(..)
       , DAO(..)
       ) where
import Control.Monad.IO.Class
import Data.Data            ( Data, Typeable )
import Data.Map

type Name = String
data Status = Completed
            | NotCompleted
            deriving (Eq, Ord, Read, Show, Data, Typeable)
type Id = Int

data Task = Task
     { tid::Id
     , name::Name
     , status::Status
     } deriving (Eq, Ord, Read, Show, Data, Typeable)

data TaskList = TaskList
     { tlid::Id
     , tname::Name
     } deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype Schedule = Schedule (Map Int (TaskList, Map Int Task))

class DAO f where
    addTask::MonadIO m => f
                       -> Int --TaskList id
                       -> Name --Name of task
                       -> m (Maybe Task) --Nothing if no such task list id

    addTaskList::MonadIO m => f
                           -> Name --TaskList name
                           -> m (Maybe TaskList)

    deleteTask::MonadIO m => f
                          -> Id --TaskList id
                          -> Id --Task id
                          -> m (Maybe Task)

    deleteTaskList::MonadIO m => f
                              -> Id --TaskList id
                              -> m (Maybe TaskList)

    getSchedule::MonadIO m => f -> m Schedule

    completeTask::MonadIO m => f
                            -> Id
                            -> Id
                            -> m (Maybe Task)
