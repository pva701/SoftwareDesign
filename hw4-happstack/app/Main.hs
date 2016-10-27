module Main where

import           AcidDAO           (initialState)
import           Control.Exception (bracket)
import           Control.Monad     (msum)
import           Controller        (addTaskController, addTaskListController,
                                    deleteTaskController, deleteTaskListController, index)
import           Data.Acid         (openLocalState)
import           Data.Acid.Local   (createCheckpointAndClose)
import           Happstack.Server  (Method (DELETE, GET, PUT), Response, ServerPart, dir,
                                    method, nullConf, path, simpleHTTP, nullDir, notFound, toResponse)
import           Model             (DAO)

handlers :: DAO d=>d->ServerPart Response
handlers acid = msum [
    dir "task" $ path $ \tid->      method DELETE >> deleteTaskController acid tid,
    dir "taskList" $ path $ \tid->  method DELETE >> deleteTaskListController acid tid,
    dir "task" $
        path $ \tid->
        path $ \name->              method PUT    >> addTaskController acid tid name,
    dir "taskList" $ path $ \name-> method PUT    >> addTaskListController acid name,
    nullDir >>                      method GET    >> index acid,
    notFound $ toResponse "Not found"
    ]

main :: IO ()
main =
    bracket (openLocalState initialState) createCheckpointAndClose
    (\acid-> simpleHTTP nullConf $ (handlers acid))
