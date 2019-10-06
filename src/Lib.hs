{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( webAppEntry
    ) where


    import Network.Wai.Handler.Warp   (run)

    import Servant
  
    import Servant.API.Generic
    import Servant.Server.Generic
      
    data Routes route = Routes
        { _get :: route :- Capture "id" Int :> Get '[JSON] String
        }
      deriving (Generic)
      
    record :: Routes AsServer
    record = Routes
        { _get = return . show . (+ 1)
        }
      
    app :: Application
    app = genericServe record
      
    webAppEntry :: IO ()
    webAppEntry = run 8000 app