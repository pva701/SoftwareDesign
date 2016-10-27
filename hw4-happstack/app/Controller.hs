module Controller
       ( addTaskController
       , addTaskListController
       , deleteTaskController
       , deleteTaskListController
       , index
       ) where
import           Happstack.Server (Response, ServerPart, ok, toResponse)
import           Data.Map      as Map        (keysSet, toList)
import           Model            (Schedule(..), DAO (..), Id, Name)

data ResponseStatus = ResponseStatus {status::String}

addTaskController::DAO d=>d->Id->Name->ServerPart Response
addTaskController dao tid name = do
    task <- addTask dao tid name
    case task of
        Nothing -> ok $ toResponse "Error during creating task"
        Just _  -> ok $ toResponse $ "Task Added: " ++ name

addTaskListController::DAO d=>d->Name->ServerPart Response
addTaskListController dao name = do
    taskList <- addTaskList dao name
    case taskList of
        Nothing -> ok $ toResponse "Error during creating"
        Just _  -> ok $ toResponse $ "Task List Added: " ++ name

deleteTaskController::DAO d=>d->Id->ServerPart Response
deleteTaskController dao tid = ok $ toResponse "Delete Task"

deleteTaskListController::DAO d=>d->Id->ServerPart Response
deleteTaskListController dao tid = ok $ toResponse "Delete Task List"

index::DAO d=>d->ServerPart Response
index dao = do
    Schedule s <- getSchedule dao
    ok $ toResponse $ show $ map (\(_, sc)->show (fst sc) ++ " " ++ (show $ Map.toList (snd sc))) $ Map.toList s --"Index"
