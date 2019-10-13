{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE DeriveAnyClass #-}


module Lib
  (
    webAppEntry
  ) where


  import Bd
  import MyType (Renamer, Linker, Creator)

  import Control.Monad.IO.Class
  import Data.Int
  import Network.Wai.Handler.Warp   (run)
  import Servant
  import Servant.API.Generic
  import Servant.Server.Generic
  
  data Routes route = Routes
    { _put :: route :- "graph" :> "node" :> "body" :> ReqBody '[JSON] Creator :> Get '[JSON] String
    , _delete :: route :- "graph" :> "node" :>  Capture "id" Int64 :> Get '[JSON] String
    , _rename :: route :- "graph" :> "node" :>  ReqBody '[JSON] Renamer :> Get '[JSON] String
    , _link :: route :- "graph" :> "link" :>  ReqBody '[JSON] Linker :> Get '[JSON] String
    ,  _get :: route :- "graph" :> "node" :> Get '[JSON] String
    , _neighb :: route :- "graph" :> "node" :> Capture "id" Int64 :> "neighbours" :> Get '[JSON] String
    }
    deriving (Generic)

  record :: Routes AsServer
  record = Routes
    { _put = \x -> liftIO(mainDB addNode x)
    , _delete = \x -> liftIO(mainDB deleteNode x)
    , _rename = \x -> liftIO(mainDB renameNode x)
    , _link = \x -> liftIO(mainDB linkNodes x)
    ,  _get = liftIO(mainDB getNodes 0) 
    , _neighb = \x -> liftIO(mainDB getNeighb x) 
    }

  app :: Application
  app = genericServe record
    
  webAppEntry :: IO ()
  webAppEntry = do
      run 8001 app