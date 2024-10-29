module Main

import Network.HTTP
import Network.HTTP.URL
import Data.Nat 
import Control.Monad.Error.Either
import Control.Monad.Error.Interface

ResultMonad : Type -> Type
ResultMonad = EitherT (HttpError ()) IO

getClient: IO (HttpClient ())
getClient = new_client certificate_ignore_check 1 1 False False

performRequest : HttpClient () -> ResultMonad (HttpResponse, Stream (Of Bits8) ResultMonad ())
performRequest client = 
  request client GET (url' "http://openbsd.org/70.html") [("Authorization", "Bearer foo")] ()
 
main : IO ()
main = do
    client <- getClient
    
    Right (result, stream) <- runEitherT $ performRequest client
      | Left e => printLn "Error"
    Right (content, _) <- runEitherT $ toList stream
      | Left e => printLn "Error"    

    printLn (status_code result)
    close client
    printLn "Done"
