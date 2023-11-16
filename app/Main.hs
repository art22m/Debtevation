{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Main where

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text, breakOn, pack, stripStart, unpack)
import qualified Data.Text as Text
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser hiding (text)
import Text.Read

-- instance Eq User
-- instance Hashable User

type Item = Text

data Model = Model
  { debtorsMap    :: HashMap Integer (HashMap Integer Int),
    users         :: HashMap Integer User,
    shareDebtors  :: HashSet Integer, -- UserIds

    sharedAmount  :: Maybe(Int, Integer),
    paidAmount    :: Maybe(Int, Integer)
  }

data Status = UID Integer | Done | Cancel


initialModel :: Model
initialModel =
  Model
    { debtorsMap = HashMap.empty,
      users = HashMap.empty,
      shareDebtors = HashSet.empty,
      sharedAmount = Nothing,
      paidAmount = Nothing
    }

type Amount = Int

data Action
  = Start User
  | Share User Amount
  | Pay User Amount
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
            (_, _) -> Nothing

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
          Nothing -> Just (Show "No callback message")
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

      Share usr amount -> shareAmount (amount, userIdToInteger (userId usr)) model <# do
          reply
            (toReplyMessage "Select users to share debt with:")
              { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup (shareKeyboard model)) }

      Pay usr amount ->
        payAmount (amount, userIdToInteger (userId usr)) model <# do
          replyText "Successfully payed!"

      Show text ->
        showDebts model <# do
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
            | selected  = pack ("+ " ++ name)
            | otherwise = pack name
            where
                firstName = userFirstName user
                lastName = fromMaybe " " (userLastName user)
                name = unpack firstName ++ " " ++ unpack lastName

                uid = userIdToInteger (userId user)
                selected = HashSet.member uid (shareDebtors model)

    startMessage =
      Text.unlines
        [ "You successfully registered!"]


removeCommand :: Maybe Text -> Maybe Text
removeCommand (Just text_) = Just (stripStart $ snd (breakOn " " text_))
removeCommand Nothing = Nothing

userIdToInteger :: UserId -> Integer
userIdToInteger (UserId i) = i


textToInt :: Maybe Text -> Maybe Int
textToInt (Just text_) = readMaybe (unpack text_)
textToInt Nothing = Nothing

shareAmount :: (Amount, Integer) -> Model -> Model
shareAmount _amount model =
  model
    { sharedAmount = Just _amount }

payAmount :: (Amount, Integer) -> Model -> Model
payAmount _amount model =
  model
    { paidAmount = Just _amount
    }

-- -- Увеличение долгов, выданных конкретным пользователем, конкретным пользователям
addShares :: [UserId] -> Model -> Model
addShares userIds model = case sharedAmount model of
  Nothing -> model
  Just (number, integerInitId) ->
    model
      { debtorsMap = HashMap.insert integerInitId (increaseShares userIds (number `div` length userIds) innerHashMap) _hashMap
      , sharedAmount = Nothing
      }
    where
      _hashMap = debtorsMap model
      innerHashMap = HashMap.lookup integerInitId _hashMap

      -- Увеличение долгов у пользователей от известного пользователя (внутренний hashmap)
      increaseShares :: [UserId] -> Int -> Maybe (HashMap Integer Int) -> HashMap Integer Int
      increaseShares [] _ hashmap = Data.Maybe.fromMaybe HashMap.empty hashmap
      increaseShares (_userId : _userIds) _number hashmap =
        HashMap.insertWith (+) integerUserId _number (increaseShares (_userId : _userIds) _number hashmap)
          where
            integerUserId = userIdToInteger _userId

-- -- Увеличение долгов, выданных конкретным пользователем, конкретным пользователям
subtrShares :: UserId -> Model -> Model
subtrShares debtor model = case sharedAmount model of
  Nothing -> model
  Just (number, integerCredorId) ->
    model
      { debtorsMap = HashMap.insert integerCredorId subtrInnerMap _hashMap
      , paidAmount = Nothing
      }
    where
      _hashMap = debtorsMap model
      innerMap = HashMap.lookup integerCredorId _hashMap
      subtrInnerMap = decreaseShares debtor number innerMap

      -- Увеличение долгов у пользователей от известного пользователя (внутренний hashmap)
      decreaseShares :: UserId -> Int -> Maybe (HashMap Integer Int) -> HashMap Integer Int
      decreaseShares _userId _number _hashMap = case _hashMap of
          Nothing -> HashMap.fromList [(integerUserId, _number)]
            where
              integerUserId = userIdToInteger _userId
          Just hMap ->
            HashMap.insertWith subtructionFunc integerUserId _number hMap
            where
              subtructionFunc old new = if old > new then old-new else 0
              integerUserId = userIdToInteger _userId

registerUser :: User -> Model -> Model
registerUser user model
    | HashMap.member uid (users model) = model
    | otherwise = model
        { debtorsMap = HashMap.insert uid HashMap.empty (debtorsMap model)
        , users = HashMap.insert uid user (users model) }
   where
    uid = userIdToInteger (userId user)

showDebts :: Model -> Model
showDebts model = model


createDebtsMessage :: User -> Model -> Text
createDebtsMessage user model
  | HashMap.member uid (users model) = listOfDebtsToText user model
  | otherwise = "You are not login, please, write /start"
  where
    uid = userIdToInteger (userId user)

getInfoInside :: User -> Model -> Text
getInfoInside _ _ = ""

listOfDebtsToText :: User -> Model -> Text
listOfDebtsToText user model = 
  maybe "You are not register 1" go $
    HashMap.lookup uid (debtorsMap model)
    where
        uid = userIdToInteger (userId user)

        go :: HashMap Integer Int -> Text
        go debtors = Text.intercalate " | "
          [ name <> " : " <> Text.pack (show amount)
          | (debtorId, amount) <- HashMap.toList debtors
          , let mname = HashMap.lookup debtorId (users model)
          , let name = maybe (Text.pack (show debtorId)) userFirstName mname
          ]

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId debtBot) env

main :: IO ()
main = do
  putStrLn "Started..."
  -- token <- Token . Text.pack <$> getLine
  run ""