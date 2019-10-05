{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( webAppEntry
    ) where

import Servant(serve, Proxy(..), Server, JSON, Get, (:>))
import Data.Aeson(ToJSON)
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)

type UserAPI = "users1" :> Get '[JSON] [User]


--похоже просто, если пришло users, то сделай Get с такими параметрами 

data User = User
  { name :: String
  , email :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
---ToJSON User allows the User to be converted to JSON (implementation is provided by generic).

users1 :: [User]
users1 =
  [ User "Isaac Newton"    "isaac@newton.co.uk"
  , User "Albert Einstein" "ae@mc2.org"
  ]

server :: Server UserAPI
server = return users1  ---both return does is wrap a value into a container
---For example an element can be wrapped in a list: return 2 == [2]
---похоже, что упаковка в монады

userAPI :: Proxy UserAPI
userAPI = Proxy

---Library author needed type information for a function, but they didn’t need a value. Proxy does that.
---It’s useful if you store data at type level, for example with the datakinds language extension, which was seen earlier.

---Proxy is a type that holds no data, 
---but has a phantom parameter of arbitrary type (or even kind). Its use is to provide type information, 
---even though there is no value available of that type (or it may be too costly to create one).

app :: Application ---This combines the proxy and server.
app = serve userAPI server

webAppEntry :: IO ()
webAppEntry = run 6865 app
