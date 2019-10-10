{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}


module Bd
    ( mainDB, addNode, getNodes, deleteNode, renameNode, linkNodes, getNeighb
    ) where
    
    import MyType
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

    connect :: IO (Either Connection.ConnectionError Connection.Connection) 
    connect = Connection.acquire connectionSettings
        where
            connectionSettings = Connection.settings "localhost" 5432 "derin" "qwerty" "graph"


    checker ::  Show a => (Either Session.QueryError a) -> IO (String)
    
    checker (Right result) = return . show $ result
    checker (Left result) = return . show $ result

    executer :: a -> (a -> Session [Int64]) -> (Either Connection.ConnectionError Connection.Connection) -> IO (String)
    --- первый параметр - параметр для запроса, второй - функция для запроса
    executer _  _ (Left con) = return . show $ con 
    executer param fun (Right con) = do
        result <- (Session.run (fun param)  con)
        checker result

    mainDB :: (a -> Session [Int64]) -> a -> IO (String)
    mainDB param fun = do
        connection <- connect   
        executer fun param connection

       {- Right result <- Session.run (getNodes) connection
        return . show $ result
        where
          connectionSettings = Connection.settings "localhost" 5432 "derin" "qwerty" "graph"
      
            -}

    addNode :: Char -> Session [Int64]
    addNode a = do
        Session.statement (a) addNodeStatement
          
       
            
    addNodeStatement :: Statement (Char) [Int64] --- надо Int64 - для idNode
    addNodeStatement = Statement sql encoder decoder True where
        sql = "INSERT INTO node(label) VALUES($1) RETURNING \"idNode\";"
        encoder = (Encoders.param (Encoders.nonNullable Encoders.char))
        decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))
        
    deleteNode :: Int64 -> Session [Int64]
    deleteNode a = do
        Session.statement (a) deleteNodeStatement
              
           
                
    deleteNodeStatement :: Statement (Int64) [Int64] --- надо Int64 - для idNode
    deleteNodeStatement = Statement sql encoder decoder True where
        sql = "DELETE FROM node WHERE \"idNode\" = $1 RETURNING \"idNode\";"
        encoder = (Encoders.param (Encoders.nonNullable Encoders.int8))
        decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

    renameNode :: Renamer -> Session [Int64]
    renameNode (MyType.Renamer a b) = do
        Session.statement (a, b) renameNodeStatement
                  
              
                    
    renameNodeStatement :: Statement (Int64, Char) [Int64] --- надо Int64 - для idNode
    renameNodeStatement = Statement sql encoder decoder True where
        sql = "UPDATE node SET label = $2 WHERE \"idNode\" = $1 RETURNING \"idNode\";"
        encoder = (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.char))
        decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))



    linkNodes :: Linker -> Session [Int64]
    linkNodes (MyType.Linker a b) = do
            Session.statement (a, b) linkNodesStatement
                      
                  
                        
    linkNodesStatement :: Statement (Int64, Int64) [Int64] --- надо Int64 - для idNode
    linkNodesStatement = Statement sql encoder decoder True where
            sql = "INSERT INTO edge(\"firstNode\", \"secondNode\") VALUES($1, $2) RETURNING \"idEdge\";"
            encoder = (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
            decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

    getNodes :: a -> Session [Int64]
    getNodes _  = do
        Session.statement () getNodesStatement 
    
 
      
    getNodesStatement :: Statement () [Int64] --- надо Int64 - для idNode
    getNodesStatement = Statement sql encoder decoder True where
        sql = "select \"idNode\" from node;"
        encoder = Encoders.noParams -- без параметров
        decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

    
    getNeighb :: Int64 -> Session [Int64]
    getNeighb a = do
            Session.statement (a) getNeighbStatement
                  
               
                    
    getNeighbStatement :: Statement (Int64) [Int64] --- надо Int64 - для idNode
    getNeighbStatement = Statement sql encoder decoder True where
            sql = "SELECT \"idNode\", \"label\" FROM node JOIN edge ON \"firstNode\" = $1 AND \"secondNode\" = \"idNode\";"
            encoder = (Encoders.param (Encoders.nonNullable Encoders.int8))
            decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))