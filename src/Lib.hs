{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE DeriveAnyClass #-}


module Lib
    ( webAppEntry
    ) where


    import Network.Wai.Handler.Warp   (run)

    import Servant
  
    import Servant.API.Generic
    import Servant.Server.Generic
    import Bd
    import Debug.Trace
    import Control.Monad.IO.Class


 
    data Routes route = Routes
        { -- _put :: route :- "graph" :> "node" :> "body" :> Capture "label" Char :> Get '[JSON] String
          _q :: route :- "users" :> Capture "id" Int :> Get '[JSON] String
        , _put0 :: route :- Capture "id" Int :> Get '[JSON] Bool
        }
      deriving (Generic)

     --PUT /graph/node/ body: {"label":label}

    record :: Routes AsServer
    record = Routes
        { --_put = \_ -> liftIO(addNode label)
          _q =  \_ -> liftIO(f1) 
        , _put0 = return . odd
        }
    app :: Application
    app = genericServe record
      
    webAppEntry :: IO ()
    webAppEntry = 
      do
        run 8000 app