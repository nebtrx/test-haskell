{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Applicative
import           Data.Either
import           Data.Map            as Map
import           Data.Text
import qualified Data.Text.IO        as T

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
    deriving Show


userLogin :: IO (Either LoginError Text)
userLogin = do
    token <- getToken

    case token of
        Right domain ->
            case Map.lookup domain users of
            Just userpw -> do
                T.putStrLn "Enter password:"
                password <- T.getLine

                if userpw == password
                    then return token

                    else return (Left WrongPassword)
            Nothing -> return (Left NoSuchUser)
        left -> return left

getToken :: IO (Either LoginError Text)
getToken = do
    T.putStrLn "Enter email address:"
    email <- T.getLine
    return (getDomain email)


users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail
