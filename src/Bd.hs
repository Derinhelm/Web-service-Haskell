{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Bd
    ( f1
    ) where
    
    import     Hasql.Connection
    import     Hasql.Decoders
    import     Hasql.Encoders
    import     Hasql.Session
    import     Hasql.Statement
    import Data.ByteString.Internal
    import Data.Int
    import Data.Either



    createConnect :: IO()
    createConnect =  do 
                        Right connection1 <- acquire s
                        Right result <- run sql1 connection1
                        putStrLn . show $ result
                            where 
                                s =  settings  "localhost" 5432 "derin" "qwerty" "graph"
                                sql1 = Hasql.Session.sql "INSERT INTO node VALUES('7', 'pw');"


    f1 :: IO ()
    f1 = createConnect 