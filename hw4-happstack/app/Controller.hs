module Controller
       ( addTaskController
       , addTaskListController
       , deleteTaskController
       , deleteTaskListController
       , index
       ) where
import           Model (DAO, Id, Name)
import           Happstack.Server  (Response, ServerPart)
--controllers::ServerPart Response
--controllers = undefined

addTaskController::DAO d=>d->Id->Name->ServerPart Response
addTaskController = undefined

addTaskListController::DAO d=>d->Name->ServerPart Response
addTaskListController = undefined

deleteTaskController::DAO d=>d->Id->ServerPart Response
deleteTaskController = undefined

deleteTaskListController::DAO d=>d->Id->ServerPart Response
deleteTaskListController = undefined

index::DAO d=>d->ServerPart Response
index = undefined
