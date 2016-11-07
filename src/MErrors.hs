module MErrors where

import Instant.Abs
import Control.Monad.Error

data CustomError
  = IdentNotFoundError Ident
  | NotImplementedError String
  | OtherError String
  
instance Show CustomError where
  show (OtherError str) = str
  show (IdentNotFoundError ident) = (show ident) ++ " has been not found"
  show (NotImplementedError str) = str ++ " is not implemented"

instance Error CustomError where
  noMsg = OtherError "Unknown error"
  strMsg str = OtherError str