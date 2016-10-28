{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Controller
       ( addTaskController
       , addTaskListController
       , deleteTaskController
       , deleteTaskListController
       , indexController
       , completeTaskController
       ) where
import           Data.Aeson       (ToJSON, encode)
import           Data.Map         as Map (toList)
import           GHC.Generics     (Generic)
import           Happstack.Server (Response, ServerPart, ok, toResponse)
import           Index            (index)
import           Model            (DAO (..), Id, Name, Schedule (..),
                                   Task (..), TaskList (..))
data OperationStatus = Success   {success::Bool}
                     | SuccessId {success::Bool, id::Int}
    deriving (Show, Generic)

instance ToJSON OperationStatus

toJSONResponse::Maybe a->ServerPart Response
toJSONResponse x = ok $ toResponse $ encode $
    case x of
        Nothing -> Success False
        Just _  -> Success True

addTaskController::DAO d=>d->Id->Name->ServerPart Response
addTaskController dao tlid tname = do
    task <- addTask dao tlid tname
    case task of
        Nothing        -> ok $ toResponse $ encode $ Success False
        Just Task{..}  -> ok $ toResponse $ encode $ SuccessId True tid

addTaskListController::DAO d=>d->Name->ServerPart Response
addTaskListController dao name = do
    taskList <- addTaskList dao name
    case taskList of
        Nothing            -> ok $ toResponse $ encode $ Success False
        Just TaskList{..}  -> ok $ toResponse $ encode $ SuccessId True tlid

deleteTaskController::DAO d=>d->Id->Id->ServerPart Response
deleteTaskController dao tlid tid = do
    taskList <- deleteTask dao tlid tid
    toJSONResponse taskList

deleteTaskListController::DAO d=>d->Id->ServerPart Response
deleteTaskListController dao tlid = do
    taskList <- deleteTaskList dao tlid
    toJSONResponse taskList

indexController::DAO d=>d->ServerPart Response
indexController dao = do
    Schedule s <- getSchedule dao
    let tlist = map snd $ Map.toList s
    let tlist2 = map (\(x, y)->(x, map snd $ Map.toList y)) tlist
    Index.index tlist2

completeTaskController::DAO d=>d->Id->Id->ServerPart Response
completeTaskController dao tlid tid = do
    task <- completeTask dao tlid tid
    toJSONResponse task
