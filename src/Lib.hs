{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE DeriveAnyClass #-}


module Lib
    ( webAppEntry
    ) where


    import Bd
    import MyType (Renamer, Linker, Neighbours)
    import Network.Wai.Handler.Warp   (run)

    import Servant
  
    import Servant.API.Generic
    import Servant.Server.Generic
    
    import Debug.Trace
    import Data.Int
    import Control.Monad.IO.Class

    
 
    data Routes route = Routes
        { _put :: route :- "graph" :> "node" :> "body" :> Capture "label" Char :> Get '[JSON] String
        , _delete :: route :- "graph" :> "node" :>  Capture "id" Int64 :> Get '[JSON] String
        , _rename :: route :- "graph" :> "node" :>  ReqBody '[JSON] Renamer :> Get '[JSON] String
        , _link :: route :- "graph" :> "link" :>  ReqBody '[JSON] Linker :> Get '[JSON] String
        ,  _get :: route :- "users" :> Capture "id" Int64 :> Get '[JSON] String
        , _neighb :: route :- "graph" :> "node1" :> Capture "id" Int64 :>  Get '[JSON] String
        }
      deriving (Generic)

     --Получить все соседей (id, label) заданного узла (GET /graph/node/{id}/neighbours)

    record :: Routes AsServer
    record = Routes
        { _put = \x -> liftIO(mainDB addNode x)
        , _delete = \x -> liftIO(mainDB deleteNode x)
        , _rename = \x -> liftIO(mainDB renameNode x)
        , _link = \x -> liftIO(mainDB linkNodes x)
        ,  _get =  \_ -> liftIO(mainDB getNodes 0) 
        , _neighb = \x -> liftIO(mainDB getNeighb x) 
        }
    app :: Application
    app = genericServe record
      
    webAppEntry :: IO ()
    webAppEntry = 
      do
        run 8000 app