module Main

import Network.HTTP
import Network.HTTP.URL
import Data.Nat 
import Control.Monad.Error.Either
import Control.Monad.Error.Interface

ResultMonad : Type -> Type
ResultMonad = EitherT (HttpError ()) IO

getClient: IO (HttpClient ())
getClient = new_client_default

performRequest : HttpClient () -> ResultMonad (HttpResponse, Stream (Of Bits8) ResultMonad ())
performRequest client = 
  request client GET (url' "http://openbsd.org/70.html") [] ()
 
main : IO ()
main = do
    client <- getClient
    result <- runEitherT $ performRequest client
    case result of
      Left err => printLn err
      Right (response, stream) =>
        printLn (status_code response)
