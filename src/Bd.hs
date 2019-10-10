{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Bd
    ( f1
    ) where
    
    
    import Data.ByteString.Internal
    import Data.Either
    import Prelude
    import Data.Int
    import Data.Functor.Contravariant
    import Hasql.Session (Session)
    import Hasql.Statement (Statement(..))
    import qualified Hasql.Session as Session
    import qualified Hasql.Decoders as Decoders
    import qualified Hasql.Encoders as Encoders
    import qualified Hasql.Connection as Connection


    f1 ::  IO (String)
    f1 = do
        Right connection <- Connection.acquire connectionSettings
        Right result <- Session.run (getNodes) connection
        return . show $ result
        where
          connectionSettings = Connection.settings "localhost" 5432 "derin" "qwerty" "graph"
      
            

    getNodes ::  Session [Int64]
    getNodes = do
        Session.statement () getNodesStatement 
    
 
      
    getNodesStatement :: Statement () [Int64] --- надо Int64 - для idNode
    getNodesStatement = Statement sql encoder decoder True where
        sql = "select \"idNode\" from node;"
        encoder = Encoders.noParams -- без параметров
        decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))
      