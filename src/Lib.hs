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
  import Data.Text(pack, Text)
  import Network.Wai.Handler.Warp   (run)
  import Servant
  import Servant.API.Generic
  import Servant.Server.Generic
  import Servant.Swagger
  import Servant.Swagger.UI

  
  data Routes route = Routes
    { _put :: route :- "graph" :> "node" :> "body" :> ReqBody '[JSON] Creator :> Put '[JSON] (Either Text [Int64])
    , _delete :: route :- "graph" :> "node" :>  Capture "id" Int64 :> Delete '[JSON] (Either Text [Int64])
    , _rename :: route :- "graph" :> "node" :>  ReqBody '[JSON] Renamer :> Put '[JSON] (Either Text [Int64])
    , _link :: route :- "graph" :> "link" :>  ReqBody '[JSON] Linker :> Put '[JSON] (Either Text [Int64])
    ,  _get :: route :- "graph" :> "node" :> Get '[JSON] (Either Text [(Int64, Text)])
    , _neighb :: route :- "graph" :> "node" :> Capture "id" Int64 :> "neighbours" :> Get '[JSON]  (Either Text [(Int64, Text)])
    }
    deriving (Generic)

  record :: Routes AsServer
  record = Routes
    { _put = \x -> liftIO(mainDB addNode x)
    , _delete = \x -> liftIO(mainDB deleteNode x)
    , _rename = \x -> liftIO(mainDB renameNode  x)
    , _link = \x -> liftIO(mainDB linkNodes x)
    ,  _get = liftIO(mainDB getNodes 0) 
    , _neighb = \x -> liftIO(mainDB getNeighb x) 
    }

  app :: Application
  app = serve (Proxy :: Proxy (ToServant Routes AsApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json")) (toServant record :<|> swaggerSchemaUIServer (toSwagger (Proxy :: Proxy (ToServant Routes AsApi))))
    
  webAppEntry :: IO ()
  webAppEntry = do
      run 8001 app

