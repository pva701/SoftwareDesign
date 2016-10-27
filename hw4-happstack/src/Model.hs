{-# LANGUAGE DeriveDataTypeable #-}
module Model
       ( Id
       , Name
       , Status
       , Task(..)
       , TaskList(..)
       , Schedule(..)
       , DAO(..)
       ) where
import Control.Monad.IO.Class
import Data.Data            ( Data, Typeable )
import Data.Vector (Vector)

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
     , tasks:: Vector Task
     } deriving (Eq, Ord, Read, Show, Data, Typeable)

newtype Schedule = Schedule (Vector TaskList)

class DAO f where
    addTask::MonadIO m => f
                       -> Int --TaskList id
                       -> Name --Name of task
                       -> m (Maybe Task) --Nothing if no such task list id

    addTaskLists::MonadIO m => f
                            -> Name --TaskList name
                            -> m (Maybe TaskList)

    deleteTask::MonadIO m => f
                          -> Id --Task id
                          -> m (Maybe Task)

    deleteTaskList::MonadIO m => f
                              -> Id --TaskList id
                              -> m (Maybe TaskList)
