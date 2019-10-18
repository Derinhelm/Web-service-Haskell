{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}


module Bd
  (
  mainDB 
  , addNode
  , getNodes
  , deleteNode
  , renameNode
  , linkNodes
  , getNeighb
  ) where
  
  import MyType

  import Data.ByteString.Internal
  import Data.Either
  import Data.Int
  import Data.Functor.Contravariant
  import Data.Text(pack, Text)
  import Hasql.Session (Session)
  import qualified Hasql.Session as Session
  import qualified Hasql.Statement as Statement
  import qualified Hasql.Decoders as Decoders
  import qualified Hasql.Encoders as Encoders
  import qualified Hasql.Connection as Connection
  import Prelude

  connect :: IO (Either Connection.ConnectionError Connection.Connection) 
  connect = Connection.acquire connectionSettings
    where
      connectionSettings = Connection.settings "localhost" 5432 "derin" "qwerty" "graph"


  checker ::  Show a => (Either Session.QueryError a) -> IO (String)
  checker (Right result) = return . show $ result
  checker (Left result) = return . show $ result


  --- первый параметр - параметр для запроса, второй - функция для запроса
  executer :: Show b => a -> (a -> Session b) -> (Either Connection.ConnectionError Connection.Connection) -> IO (String)
  executer _  _ (Left con) = return . show $ con 
  executer param fun (Right con) = do
      result <- (Session.run (fun param)  con)
      checker result

  mainDB :: Show b => (a -> Session b) -> a -> IO (String)
  mainDB param fun = do
      connection <- connect   
      executer fun param connection



  addNode :: Creator -> Session [Int64]
  addNode (Creator a) = do
      Session.statement (Data.Text.pack a) addNodeStatement
      
     
      
  addNodeStatement :: Statement.Statement (Text) [Int64]
  addNodeStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "INSERT INTO node(label) VALUES($1) RETURNING id_node;"
      encoder = (Encoders.param (Encoders.nonNullable Encoders.text))
      decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))
    
  deleteNode :: Int64 -> Session [Int64]
  deleteNode a = do
      Session.statement (a) deleteNodeStatement
        
  deleteNodeStatement :: Statement.Statement (Int64) [Int64] 
  deleteNodeStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "DELETE FROM node WHERE id_node = $1 RETURNING id_node;"
      encoder = (Encoders.param (Encoders.nonNullable Encoders.int8))
      decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

  renameNode :: Renamer -> Session [Int64]
  renameNode (MyType.Renamer a b) = do
    Session.statement (a, (Data.Text.pack b)) renameNodeStatement
          
        
          
  renameNodeStatement :: Statement.Statement (Int64, Text) [Int64] 
  renameNodeStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "UPDATE node SET label = $2 WHERE id_node = $1 RETURNING id_node;"
      encoder = (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.text))
      decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))



  linkNodes :: Linker -> Session [Int64]
  linkNodes (MyType.Linker a b) = do
      Session.statement (a, b) linkNodesStatement
            
          
            
  linkNodesStatement :: Statement.Statement (Int64, Int64) [Int64] 
  linkNodesStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "INSERT INTO edge(first_node, second_node) VALUES($1, $2) RETURNING id_edge;"
      encoder = (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <> (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
      decoder = Decoders.rowList (Decoders.column (Decoders.nonNullable Decoders.int8))

  getNodes :: a -> Session [(Int64, Text)]
  getNodes _  = do
      Session.statement () getNodesStatement 
  
  x :: Decoders.Row (Int64, Text)
  x = (,) <$> (Decoders.column . Decoders.nonNullable) Decoders.int8 <*> (Decoders.column . Decoders.nonNullable) Decoders.text 
    
 
    
  getNodesStatement :: Statement.Statement () [(Int64, Text)] 
  getNodesStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "select id_node, label from node;"
      encoder = Encoders.noParams -- без параметров
      decoder =  Decoders.rowList (x)

  
  getNeighb :: Int64 -> Session ([(Int64, Text)])
  getNeighb a = do
      Session.statement (a) getNeighbStatement
          
         
          
  getNeighbStatement :: Statement.Statement (Int64) ([(Int64, Text)]) 
  getNeighbStatement = Statement.Statement sql encoder decoder True 
    where
      sql = "SELECT id_node, label FROM node JOIN edge ON first_node = $1 AND second_node = id_node;"
      encoder = (Encoders.param (Encoders.nonNullable Encoders.int8))
      decoder =   Decoders.rowList (x)