{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Hashable

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.Text (Text, breakOn, pack, stripStart, unpack)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser hiding (text)
import Text.Read

-- | Make User hashable

instance Eq User where
  (==) :: User -> User -> Bool
  x == y = userId x == userId y

deriving instance Hashable UserId
deriving instance Read UserId

instance Hashable User where
  hashWithSalt :: Int -> User -> Int
  hashWithSalt salt = hashWithSalt salt . userId

type Item = Text

data Model = Model
  { debtorsMap    :: HashMap User (HashMap User Int),

    users         :: HashMap UserId User,

    shareDebtors  :: HashSet UserId, -- UserIds
    sharedAmount  :: Maybe (Int, User)
  }

data DebtevationQuery
  = ShareInKbQ ShareInKbQMessage
  deriving (Show, Read)

data ShareInKbQMessage
  = ShareInKbQMessageDone
  | ShareInKbQMessageCancel
  | ShareInKbQMessageUID UserId
  deriving (Show, Read)

callbackButton' :: Text -> DebtevationQuery -> InlineKeyboardButton
callbackButton' label query = callbackButton label (Text.pack (show query))

initialModel :: Model
initialModel =
  Model
    { debtorsMap = HashMap.empty,
      users = HashMap.empty,
      shareDebtors = HashSet.empty,
      sharedAmount = Nothing
    }

type Amount = Int

data Action
  = Help
  | NotAvailableCommand
  | Start User
  | Share User Amount
  | ShowCallback Text
  | Show User
  | FinishShare
  | CancelShare
  | ChangeDebtor UserId
  deriving (Show)

debtBot :: BotApp Model Action
debtBot =
  BotApp
    { botInitialModel = initialModel,
      botAction = updateToAction,
      botHandler = handleAction,
      botJobs = []
    }
  where
    updateToAction :: Update -> Model -> Maybe Action
    updateToAction update model
      | isCommand update "start" = do
          msg <- updateMessage update
          let user_ = messageFrom msg
          case user_ of
            Just user -> Just (Start user)
            _ -> Nothing

      | isCommand update "share" = do
          if not (isRegistered update model) then Just NotAvailableCommand
          else do
            msg <- updateMessage update
            let user_ = messageFrom msg
            let amount_ = textToInt $ removeCommand $ messageText msg

            case (user_, amount_) of
              (Just user, Just amount) -> Just (Share user amount)
              (_, _) -> Just (ShowCallback "Enter a non-zero number separated by a space, like this:\n/share 100")

      | isCommand update "show" = do
          if not (isRegistered update model) then Just NotAvailableCommand
          else do
            let msg = updateMessage update
            case msg of
              Nothing -> Just (ShowCallback "SHOW_DEBT_MSG_NOTHING_FAIL")
              Just _msg ->
                case messageFrom _msg of
                    Just user -> Just (Show user)
                    _ -> Just (ShowCallback "SHOW_DEBT_MSG_NOTHING_FAIL_2")

      | isCommand update "help" = Just Help

      | otherwise = do
        let msg = updateCallbackQuery update
        case msg of
          Nothing -> Just (ShowCallback "No such command =(\nWrite /help to see existing commands.")
          Just _msg -> case callbackQueryData _msg of
              Nothing -> Just (ShowCallback "No data in callback message")
              Just msgText -> case sharedAmount model of
                Just shareUser -> if userName (callbackQueryFrom _msg) == userName (snd shareUser)
                                    then case readMaybe (unpack msgText) of
                                      Just x -> handleDebtevationQuery x
                                      Nothing -> Just (ShowCallback "No data in callback message")
                                    else Nothing
                Nothing -> Just (ShowCallback "No data in callback message")
          where 
            handleDebtevationQuery :: DebtevationQuery -> Maybe Action
            handleDebtevationQuery (ShareInKbQ message) = case message of 
                ShareInKbQMessageDone -> Just FinishShare
                ShareInKbQMessageCancel -> Just CancelShare
                (ShareInKbQMessageUID _uid) -> Just (ChangeDebtor _uid)


    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Help -> model <# do
                  replyText $ pack ( "/help - No comments" ++ "\n"
                                  ++ "/start - Registration command" ++ "\n"
                                  ++ "/share <<num>> - Share money between registered users. Ex. /share 100" ++ "\n"
                                  ++ "/show - Display all your debtors and everyone you owe money to" ++ "\n")

      Start usr -> registerUser usr model <# do
        if HashMap.member (userId usr) (users model) then
            replyText alreadyRegisteredMessage
        else replyText startMessage

      Share usr amount -> shareAmount (amount, usr) model <# do
          reply
            (toReplyMessage "Select users to share debt with:")
              { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup (shareKeyboard model)) }

      NotAvailableCommand ->
        showDebts model <# do -- showDebts в Show text - норм?
          replyText needToRegisterMessage

      ShowCallback text ->
        showDebts model <# do -- showDebts в Show text - норм?
          replyText text

      Show user ->
        showDebts model <# do
          replyText $ createDebtsMessage user model

      FinishShare -> let newModel = addShares (HashSet.toList (shareDebtors model)) model in
          newModel {
            shareDebtors = HashSet.empty,
            sharedAmount  = Nothing
          } <# do
            editUpdateMessage
              (toEditMessage "Successfully splitted money =)")
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Nothing }

      CancelShare -> model {
        shareDebtors = HashSet.empty,
        sharedAmount  = Nothing
      } <# do
        editUpdateMessage
              (toEditMessage "Share cancelled...")
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Nothing }

      ChangeDebtor uid -> let newModel = model {
        shareDebtors = if HashSet.member uid (shareDebtors model) then
              HashSet.delete uid (shareDebtors model)
          else
              HashSet.insert uid (shareDebtors model)
      } in newModel <# do
          editUpdateMessage
              (toEditMessage "Select users to share debt with:")
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Just (SomeInlineKeyboardMarkup (shareKeyboard newModel)) }

    shareKeyboard model =
      InlineKeyboardMarkup
        (namesButtons : [[doneButton, cancelButton]])
      where
        _users :: [User]
        _users = HashMap.elems (users model)

        
        namesButtons = 
          [ callbackButton' (userToButtonName user) (ShareInKbQ (ShareInKbQMessageUID (userId user)))
          | user <- _users ]
        doneButton = callbackButton' "☑️ done" (ShareInKbQ ShareInKbQMessageDone)
        cancelButton = callbackButton' "✖️ cancel" (ShareInKbQ ShareInKbQMessageCancel)

        userToButtonName :: User -> Text
        userToButtonName user
            | selected  = "✅ " <> userName user
            | otherwise = "❌ " <> userName user
            where
                selected = HashSet.member (userId user) (shareDebtors model)

    startMessage =
      Text.unlines
        [ "You successfully registered =)"]
    alreadyRegisteredMessage =
      Text.unlines
        [ "You are already registered"]
    needToRegisterMessage =
      Text.unlines
        [ "Before using this command, please, register using /start"]


removeCommand :: Maybe Text -> Maybe Text
removeCommand (Just text_) = Just (stripStart $ snd (breakOn " " text_))
removeCommand Nothing = Nothing

userIdToInteger :: UserId -> Integer
userIdToInteger (UserId i) = i

userName :: User -> Text
userName user = firstName <> " " <> lastName
  where
    firstName = userFirstName user
    lastName = fromMaybe " " (userLastName user)

textToInt :: Maybe Text -> Maybe Int
textToInt (Just text_) = readMaybe (unpack text_)
textToInt Nothing = Nothing

shareAmount :: (Amount, User) -> Model -> Model
shareAmount x model =
  model
    { sharedAmount = Just x }

userIdToUserMapper :: Model -> UserId -> Maybe User
userIdToUserMapper model userID = HashMap.lookup userID (users model)

isCommand :: Update -> Text -> Bool
isCommand update cmd = isJust (parseUpdate (command cmd) update) || isJust (parseUpdate (command (cmd <> pack "@debtevation_bot")) update)

isRegistered :: Update -> Model -> Bool
isRegistered update model =
  case updateMessage update of
    Just msg ->
      case messageFrom msg of
        Just user -> HashMap.member (userId user) (users model)
          where
        _ -> False
    _ -> False

-- Увеличение долгов, выданных конкретным пользователем, конкретным пользователям
addShares :: [UserId] -> Model -> Model
addShares userIds model = case sharedAmount model of
  Nothing -> model
  Just (number, usr) ->
    model
      { debtorsMap = filteredResult
      , sharedAmount = Nothing
      }
    where
      decreasedMap = decreaseShares usersToShare (debtorsMap model)
      innerMap = HashMap.lookup usr decreasedMap
      averageDebt = (number + length userIds - 1) `div` length userIds
      usersToShare = mapMaybe (userIdToUserMapper model) userIds

      nonFilteredResult = HashMap.insert usr (increaseShares usr usersToShare averageDebt innerMap) decreasedMap
      filteredResult = removeZeros (HashMap.keys nonFilteredResult) nonFilteredResult

      -- Увеличение долгов у пользователей от известного пользователя (внутренний hashmap)
      increaseShares :: User -> [User] -> Int -> Maybe (HashMap User Int) -> HashMap User Int
      increaseShares _ [] _ debtors = fromMaybe HashMap.empty debtors
      increaseShares currUser (_user : _users) amount debtors
        | _user == currUser = increaseShares currUser _users amount debtors
        | otherwise = HashMap.insertWith (+) _user amount (increaseShares currUser _users amount debtors)

      decreaseShares :: [User] -> HashMap User (HashMap User Int) -> HashMap User (HashMap User Int)
      decreaseShares [] x = x
      decreaseShares (_user : _users) debtors =
        decreaseShares _users (HashMap.insert _user (increaseShares _user [usr] (-averageDebt) (Just innerDebtors)) debtors)
        where
          innerDebtors = HashMap.lookupDefault HashMap.empty _user debtors

      removeZeros :: [User] -> HashMap User (HashMap User Int) -> HashMap User (HashMap User Int)
      removeZeros [] x = x
      removeZeros (_user : _users) debtors =
        removeZeros _users (HashMap.insert _user filteredInnerDebtors debtors)
        where
          innerDebtors = HashMap.lookupDefault HashMap.empty _user debtors
          filteredInnerDebtors = HashMap.filter (/= 0) innerDebtors

registerUser :: User -> Model -> Model
registerUser user model
    | HashMap.member (userId user) (users model) = model
    | otherwise = model
        { debtorsMap = HashMap.insert user HashMap.empty (debtorsMap model)
        , users = HashMap.insert (userId user) user (users model) }

showDebts :: Model -> Model
showDebts model = model

createDebtsMessage :: User -> Model -> Text
createDebtsMessage user model
  | HashMap.member (userId user) (users model) = listOfDebtsToText user model
  | otherwise = "You are not registered =(\n Write /start"

listOfDebtsToText :: User -> Model -> Text
listOfDebtsToText user model = case HashMap.lookup user (debtorsMap model) of
  Nothing -> "Something went wrong. Probably you are not registered.\n Write /start"
  Just debtors -> if HashMap.null debtors
                  then "You have nor debts, nor loans"
                  else debtorsText
    where
      debtorsText = Text.intercalate "\n"
        [ pack "Your loans:", myLends, pack "\nYour debts:", myDebts ]
      myLends = Text.intercalate ""
        [ if amount > 0
          then userName debtor <> " - " <> Text.pack (show amount) <> " rub\n"
          else pack ""
        | (debtor, amount) <- HashMap.toList debtors
        ]
      myDebts = Text.intercalate ""
        [ if amount < 0
          then userName debtor <> " - " <> Text.pack (show (-amount)) <> " rub\n"
          else pack ""
        | (debtor, amount) <- HashMap.toList debtors
        ]

-- TODO:
--
-- Backlog ⌛
-- Не отвечать в лс 
--
-- Done
-- Запрещать нажимать кнопки не адресату
-- Обработка commandd с @debtevation_bot и без
-- В addShares вычитать у того на кого делим 
-- Регистрация 2 раза 
-- Запрещать все без регистрации
-- Удалять из мапы должников если 0 долг 
-- Разделиь я должен ..., мне должны ... -- Можно понимать по занку долга 
-- Написать хелп 

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId debtBot) env

main :: IO ()
main = do
  putStrLn "Started..."
  -- token <- Token . Text.pack <$> getLine
  run ""