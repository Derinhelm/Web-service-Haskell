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

    


    f1 :: IO ()
    f1 = do
        Right connection <- Connection.acquire connectionSettings
        Right result <- Session.run (sum1 3 8) connection
        print result
        where
          connectionSettings = Connection.settings "localhost" 5432 "derin" "qwerty" "graph"
      
            
    sum1 :: Int64 -> Int64 -> Session Int64
    sum1 a b = do
        -- Get the sum of a and b
        Session.statement (a, b) sumStatement --(a, b) - параметр, sumStatement - результат
    
 
      
    sumStatement :: Statement (Int64, Int64) Int64
    sumStatement = Statement sql encoder decoder True where
        sql = "select $1 + $2"
        encoder =
          (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <>
          (snd >$< Encoders.param (Encoders.nonNullable Encoders.int8))
        decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
      