module Main where

import           AcidDAO           (initialState)
import           Control.Exception (bracket)
import           Control.Monad     (msum)
import           Controller        (addTaskController, addTaskListController,
                                    completeTaskController, deleteTaskController,
                                    deleteTaskListController, indexController)
import           Data.Acid         (openLocalState)
import           Data.Acid.Local   (createCheckpointAndClose)
import           Happstack.Server  (Browsing (..), Method (DELETE, GET, POST, PUT),
                                    Response, ServerPart, dir, method, notFound, nullConf,
                                    nullDir, path, serveDirectory, simpleHTTP, toResponse)
import           Model             (DAO)

handlers :: DAO d=>d->ServerPart Response
handlers acid = msum [
    dir "css" $ serveDirectory EnableBrowsing [] "app/pages/css",
    dir "js" $ serveDirectory EnableBrowsing [] "app/pages/js",
    dir "fonts" $ serveDirectory EnableBrowsing [] "app/pages/fonts",
    dir "task" $
        path $ \tlid  ->
        path $ \tid ->              method DELETE >> deleteTaskController acid tlid tid,
    dir "taskList" $ path $ \tid->  method DELETE >> deleteTaskListController acid tid,
    dir "task" $
        path $ \tid ->
        path $ \name->              method PUT    >> addTaskController acid tid name,
    dir "taskList" $ path $ \name-> method PUT    >> addTaskListController acid name,
    dir "completeTask" $
        path $ \tlid  ->
        path $ \tid ->              method POST   >> completeTaskController acid tlid tid,
    nullDir >>                      method GET    >> indexController acid,
    notFound $ toResponse "Not found"
    ]

main :: IO ()
main = bracket (openLocalState initialState) createCheckpointAndClose
       (\acid-> simpleHTTP nullConf $ (handlers acid))
