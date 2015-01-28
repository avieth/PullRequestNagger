{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as B8

import Text.Blaze.Html5 hiding (command)
import Text.Blaze.Html5.Attributes hiding (id)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import System.Console.Haskeline

import Network.BSD (getHostName)
import Network.Mail.Mime
import Network.Mail.SMTP.SMTP
import Network.Mail.SMTP.SMTPParameters
import Network.Mail.SMTP.Types (Command(EHLO))
import Network.Mail.SMTP.Auth
import Network.Mail.SMTP.Send

import Github.Auth
import Github.PullRequests

data PullRequestNagger = PullRequestNagger {
    githubInfo :: GithubInfo
  , senderInfo :: SenderInfo
  , receiverInfo :: ReceiverInfo
  , specialMessage :: Maybe String
  }

data GithubInfo = GithubInfo {
    githubUsername :: String
  , githubPassword :: String
  , githubRepoOwner :: String
  , githubRepoName :: String
  }

data SenderInfo = SenderInfo {
    senderAddress :: String
  , senderUsername :: String
  , senderPassword :: String
  , senderHost :: String
  , senderPort :: Int
  }

data ReceiverInfo = ReceiverInfo {
    receiverAddress :: String
  }

data NagError = FailedToFetchPullRequests | FailedToSendMail

instance Show NagError where
  show FailedToFetchPullRequests = "Failed to fetch pull requests!"
  show FailedToSendMail = "Failed to send mail!"

data NagReport = NagOK | NothingToNagAbout | NagFailed NagError

nag :: PullRequestNagger -> IO NagReport
nag nagger = fmap transformOutput (runExceptT doNag)

  where

    doNag = do
      pullRequests <- getPullRequests nagger
      let numberOfPrs = length pullRequests
      if numberOfPrs == 0
      then return NothingToNagAbout
      else do
        liftIO $ putStrLn ("Got " ++ show numberOfPrs ++ " pull requests.")
        mail <- liftIO $ makeEmail nagger pullRequests
        sendMail nagger mail
        return NagOK

    transformOutput = either (NagFailed) id

printNagReport :: NagReport -> IO ()
printNagReport (NagOK) = putStrLn "Successfully nagged!"
printNagReport (NothingToNagAbout) = putStrLn "Nothing to nag about!"
printNagReport (NagFailed reason) = print reason

getPullRequests :: PullRequestNagger -> ExceptT NagError IO [PullRequest]
getPullRequests nagger = do
    result <- liftIO $ pullRequestsFor' (Just githubAuth) owner name
    case result of
      Left _ -> throwE FailedToFetchPullRequests
      Right pullRequests -> return $ filterPullRequests nagger pullRequests
  where
    githubAuth = GithubBasicAuth username password
    username = B8.pack $ githubUsername (githubInfo nagger)
    password = B8.pack $ githubPassword (githubInfo nagger)
    owner = githubRepoOwner (githubInfo nagger)
    name = githubRepoName (githubInfo nagger)

filterPullRequests :: PullRequestNagger -> [PullRequest] -> [PullRequest]
filterPullRequests nagger = filter (belongsToUser user)

  where

    belongsToUser :: String -> PullRequest -> Bool
    belongsToUser user = ((==) user) . githubOwnerLogin . pullRequestUser

    user :: String
    user = githubUsername (githubInfo nagger)

sendMail :: PullRequestNagger -> Mail -> ExceptT NagError IO ()
sendMail nagger mail = do
    smtpResult <- liftIO sendMail'
    case smtpResult of
      Left smtpError -> throwE FailedToSendMail
      Right () -> return ()
  
  where

    sendMail' = smtp smtpParameters $ do
      hostname <- liftIO $ getHostName
      command $ EHLO (B8.pack hostname)
      expectCode 250
      startTLS
      authLogin username password
      send mail

    smtpParameters = SMTPParameters {
        smtpHost = senderHost (senderInfo nagger)
      , smtpPort = fromIntegral $ senderPort (senderInfo nagger)
      , smtpVerbose = False
      }

    username = senderUsername (senderInfo nagger)

    password = senderPassword (senderInfo nagger)

makeEmail :: PullRequestNagger -> [PullRequest] -> IO Mail
makeEmail nagger pullRequests = simpleMail to from subject "" body []
  where
    to = Address (Just "The Lazy Developer") (T.pack $ receiverAddress (receiverInfo nagger))
    from = Address (Just "The Pull Request Nagger") (T.pack $ senderAddress (senderInfo nagger))
    subject = T.pack $ "You have " ++ show (length pullRequests) ++ " pull requests waiting for review"
    body = makeEmailBody nagger pullRequests

makeEmailBody :: PullRequestNagger -> [PullRequest] -> TL.Text
makeEmailBody nagger pullRequests = renderHtml $ do
  maybe (return ()) (header . string) (specialMessage nagger)
  mapM_ pullRequestLink pullRequests
  footer $ string "You've been nagged by the automatic Pull Request Nagger"

pullRequestLink :: PullRequest -> Markup
pullRequestLink pullRequest = do
    a (string title) ! href (fromString url)
    br
  where
    title = pullRequestTitle pullRequest
    url = pullRequestHtmlUrl pullRequest

makePullRequestNagger :: IO PullRequestNagger
makePullRequestNagger = runInputT defaultSettings $ do
  githubUsername <- getNonemptyString "Github username"
  githubPassword <- getSecretString "Github password"
  githubRepoOwner <- getNonemptyString "Github repo owner"
  githubRepoName <- getNonemptyString "Github repo name"
  emailSenderAddress <- getEmail "Sending email address"
  emailSenderUsername <- getStringDefault emailSenderAddress "Sending email username"
  emailSenderPassword <- getSecretString "Sending email password"
  emailSenderHost <- getNonemptyString "Sending SMTP host"
  emailSenderPort <- getInteger "Sending SMTP port"
  emailReceiver <- getEmail "Receiving email address"
  message <- getString "Special message"
  let github = GithubInfo githubUsername githubPassword githubRepoOwner githubRepoName
  let sender = SenderInfo emailSenderAddress emailSenderUsername emailSenderPassword emailSenderHost emailSenderPort
  let receiver = ReceiverInfo emailReceiver
  let specialMessage = if null message then Nothing else Just message
  return $ PullRequestNagger github sender receiver specialMessage

getString :: String -> InputT IO String
getString prompt = do
  maybeString <- getInputLine (prompt ++ " : ")
  case maybeString of
    Nothing -> getString prompt
    Just str -> return str

getNonemptyString :: String -> InputT IO String
getNonemptyString prompt = do
  maybeString <- getInputLine (prompt ++ " : ")
  case maybeString of
    Nothing -> getNonemptyString prompt
    Just "" -> getNonemptyString prompt
    Just str -> return str

getSecretString :: String -> InputT IO String
getSecretString prompt = do
  maybePassword <- getPassword (Just '*') (prompt ++ " : ")
  case maybePassword of
    Nothing -> getSecretString prompt
    Just str -> return str

getEmail :: String -> InputT IO String
getEmail prompt = do
    str <- getNonemptyString prompt
    if isEmail str
    then return str
    else getEmail prompt
  where
    -- We just test for an @ sign. Don't want to bring in any heavier machinery
    isEmail :: String -> Bool
    isEmail = elem '@'

getStringDefault :: String -> String -> InputT IO String
getStringDefault deflt prompt = do
  maybeString <- getInputLine (prompt ++ " (default " ++ deflt ++ ") : ")
  case maybeString of
    Nothing -> getStringDefault deflt prompt
    Just "" -> return deflt
    Just str -> return str

getInteger :: String -> InputT IO Int
getInteger prompt = do
  str <- getNonemptyString prompt
  case reads str of
    [(int,"")] -> return int
    _ -> getInteger prompt

main = makePullRequestNagger >>= nag >>= printNagReport
