{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

instance Hashable User where
  hashWithSalt :: Int -> User -> Int
  hashWithSalt salt user = salt + fromIntegral uid
    where
      uid = userIdToInteger (userId user)

type Item = Text

data Model = Model
  { debtorsMap    :: HashMap User (HashMap User Int),

    users         :: HashMap Integer User,

    shareDebtors  :: HashSet Integer, -- UserIds
    sharedAmount  :: Maybe(Int, User)
  }

data Status = UID Integer | Done | Cancel


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
  = Start User
  | Share User Amount
  | Show Text
  | ShowDebt User
  | FinishShare
  | CancelShare
  | ChangeDebtor Integer
  deriving (Show)

matchMessage :: Text -> Status
matchMessage message = case head (unpack message) of
  '1' -> maybe Cancel UID uid
  '2' -> Done
  '3' -> Cancel
  _ -> Cancel
  where
    uid = case textToInt (Just (pack (tail (unpack message)))) of
      Nothing -> Nothing
      Just num -> Just (toInteger num)

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
    updateToAction update _
      | isJust $ parseUpdate (command "start") update = do
          msg <- updateMessage update
          let user_ = messageFrom msg
          case user_ of
            Just user -> Just (Start user)
            _ -> Nothing

      | isJust $ parseUpdate (command "share") update = do
          msg <- updateMessage update
          let user_ = messageFrom msg
          let amount_ = textToInt $ removeCommand $ messageText msg

          case (user_, amount_) of
            (Just user, Just amount) -> Just (Share user amount)
            (_, _) -> Just (Show "Enter a non-zero number separated by a space, like this:\n/share 100")

      | isJust $ parseUpdate (command "show_debt") update = do
          let msg = updateMessage update
          case msg of
            Nothing -> Just (Show "SHOW_DEBT_MSG_NOTHING_FAIL")
            Just _msg ->
              case messageFrom _msg of
                  Just user -> Just (ShowDebt user)
                  _ -> Just (Show "SHOW_DEBT_NOTHING_FAIL_2")

      | otherwise = do
        let msg = updateCallbackQuery update
        case msg of
          Nothing -> Just (Show "No such command =( \nWrite /help to see existing commands.")
          Just _msg -> case callbackQueryData _msg of
              Nothing -> Just (Show "No data in callback message")
              Just msgText -> case matchMessage msgText of
                 Done -> Just FinishShare -- Call addShares
                 Cancel -> Just CancelShare -- Clear sharedAmount
                 UID _userId -> Just (ChangeDebtor _userId) -- Add new user to Set

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Start usr -> registerUser usr model <# do
          replyText startMessage

      Share usr amount -> shareAmount (amount, usr) model <# do
          reply
            (toReplyMessage "Select users to share debt with:")
              { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup (shareKeyboard model)) }

      Show text ->
        showDebts model <# do -- showDebts в Show text - норм?
          replyText text

      ShowDebt user ->
        showDebts model <# do
          replyText $ createDebtsMessage user model

      FinishShare -> let newModel = addShares (map UserId (HashSet.toList (shareDebtors model))) model in
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

        namesButtons = map (\user -> callbackButton (userToButtonName user) (userToTextUserId user)) _users
        doneButton = callbackButton "done" "2"
        cancelButton = callbackButton "cancel" "3"

        userToTextUserId :: User -> Text
        userToTextUserId user = pack ("1" ++ show uid)
            where
                uid = userIdToInteger (userId user)

        userToButtonName :: User -> Text
        userToButtonName user
            | selected  = "✅ " <> userName user
            | otherwise = "❌ " <> userName user
            where
                uid = userIdToInteger (userId user)
                selected = HashSet.member uid (shareDebtors model)

    startMessage =
      Text.unlines
        [ "You successfully registered =)"]


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
userIdToUserMapper model (UserId userID) = HashMap.lookup userID (users model)

-- -- Увеличение долгов, выданных конкретным пользователем, конкретным пользователям
addShares :: [UserId] -> Model -> Model
addShares userIds model = case sharedAmount model of
  Nothing -> model
  Just (number, usr) ->
    model
      { debtorsMap = HashMap.insert usr (increaseShares usr usersToShare averageDebt innerMap) decreasedMap
      , sharedAmount = Nothing
      }
    where
      decreasedMap = decreaseShares usersToShare (debtorsMap model)
      innerMap = HashMap.lookup usr decreasedMap
      averageDebt = number `div` length userIds
      usersToShare = mapMaybe (userIdToUserMapper model) userIds

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


registerUser :: User -> Model -> Model
registerUser user model
    | HashMap.member uid (users model) = model
    | otherwise = model
        { debtorsMap = HashMap.insert user HashMap.empty (debtorsMap model)
        , users = HashMap.insert uid user (users model) }
   where
    uid = userIdToInteger (userId user)

showDebts :: Model -> Model
showDebts model = model

createDebtsMessage :: User -> Model -> Text
createDebtsMessage user model
  | HashMap.member uid (users model) = listOfDebtsToText user model
  | otherwise = "You are not registered =(\n Write /start"
  where
    uid = userIdToInteger (userId user)

listOfDebtsToText :: User -> Model -> Text
listOfDebtsToText user model = case HashMap.lookup user (debtorsMap model) of
  Nothing -> "Something went wrong. Probably you are not registered.\n Write /start"
  Just debtors -> if debtorsText == ""
                  then "You have no debts"
                  else debtorsText
    where 
      debtorsText = Text.intercalate "\n"
        [ userName debtor <> " : " <> Text.pack (show amount)
        | (debtor, amount) <- HashMap.toList debtors
        ] 

-- TODO:
-- Регистрация 2 раза (Вагиф)
-- Не давать делать /show_debt без регистрации
-- Удалять из мапы должников если 0 долг
-- Разделиь я должен ..., мне должны ... -- Можно понимать по занку долга
-- В addShares вычитать у того на кого делим <<< done
--
-- Не отвечать в лс

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId debtBot) env

main :: IO ()
main = do
  putStrLn "Started..."
  -- token <- Token . Text.pack <$> getLine
  run ""