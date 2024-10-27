module Main

import Network.HTTP
import Network.HTTP.URL
import Data.Nat 
import Control.Monad.Error.Either
import Control.Monad.Error.Interface
import System

with_client : {e : _} -> IO (HttpClient e) -> (HttpClient e -> EitherT (HttpError e) IO a) -> EitherT (HttpError e) IO a
with_client client f = MkEitherT $ do
  c <- client
  Right ok <- runEitherT (f c)
  | Left err => close c *> pure (Left err)
  close c
  pure (Right ok)

map_error : Functor m => (e -> e') -> EitherT e m a -> EitherT e' m a
map_error f = bimapEitherT f id

test_redirect : EitherT String IO ()
test_redirect = map_error show $ with_client {e=()} new_client_default $ \client => do
  putStrLn "sending request"
  (response, content) <- request client GET (url' "http://openbsd.org/70.html") [] ()
  putStrLn "response header received"
  printLn response
  putStrLn "downloading response"
  content <- toList_ content
  printLn $ "\{show $ length content} bytes read"

-- run_test : String -> EitherT String IO () -> IO Bool
-- run_test name test = do
--   Right () <- runEitherT test
--   | Left err => putStrLn "\{name}: failed \{err}" $> False
--   putStrLn "\{name}: success" $> True

-- run : List (IO Bool) -> IO ()
-- run tests = do
--   results <- sequence tests
--   let [] = filter not results
--   | xs => putStrLn "\{show (length xs)} tests failed" *> exitFailure
--   putStrLn "all tests passed"

main : IO ()
main = do
  _ <- runEitherT test_redirect
  putStrLn "all tests passed"

