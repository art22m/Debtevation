{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Hashable

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe (isJust, fromMaybe, mapMaybe, fromJust)
import Data.Text (Text, breakOn, pack, stripStart, unpack)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser hiding (text)
import Text.Read

-- | Helpers

instance Eq User where
  (==) :: User -> User -> Bool
  x == y = userId x == userId y

deriving instance Hashable UserId
deriving instance Read UserId

instance Hashable User where
  hashWithSalt :: Int -> User -> Int
  hashWithSalt salt = hashWithSalt salt . userId

-- | Model

data Model = Model
  { debtorsMap    :: HashMap User (HashMap User Int),

    users         :: HashMap UserId User,

    acceptedDbors :: HashSet UserId, -- Пользователи подтвердившие долг    
    shareDebtors  :: HashSet UserId, -- Пользователи на которых распределяется долг
    sharedAmount  :: Maybe (Int, User, Maybe Text)
  }

initialModel :: Model
initialModel =
  Model
    { debtorsMap = HashMap.empty,
      users = HashMap.empty,
      acceptedDbors = HashSet.empty,
      shareDebtors = HashSet.empty,
      sharedAmount = Nothing
    }

-- | Inline Keyboards

data DebtevationQuery
  = ShareInKbQ ShareInKbQMessage
  | AcceptInKbQ AcceptInKbQMessage 
  deriving (Show, Read)

data AcceptInKbQMessage 
  = AcceptInKbQMessageUID UserId 
  | AcceptInKbQMessageCancel
  | AcceptInKbQMessageDone
  deriving (Show, Read) 

data ShareInKbQMessage
  = ShareInKbQMessageDone
  | ShareInKbQMessageCancel
  | ShareInKbQMessageUID UserId
  deriving (Show, Read)

callbackButton' :: Text -> DebtevationQuery -> InlineKeyboardButton
callbackButton' label query = callbackButton label (Text.pack (show query))

-- | Actions

data Action
  = Help
  | NotAvailableCommand
  | Start User
  | Share User Int (Maybe Text)
  | Optimize
  | ShowCallback Text
  | Show User
  | FinishShare
  | CancelShare
  | ChangeDebtor UserId
  | AcceptDebtAction UserId
  | FinishAccept
  deriving (Show)

-- | Commands and Actions Implementations

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
            let amount_ = textToInt $ getAmountOfShare $ messageText msg
            let desc_ = getDescriptionOfShare $ messageText msg
            case (user_, amount_) of
              (Just user, Just amount) -> Just (Share user amount desc_)
              (_, _) -> Just (ShowCallback "Enter a non-zero number separated by a space, like this:\n/share 100")

      | isCommand update "show" = do
          if not (isRegistered update model) then Just NotAvailableCommand
          else do
            let msg = updateMessage update
            case msg of
              Nothing -> Nothing 
              Just _msg ->
                case messageFrom _msg of
                  Just user -> Just (Show user)
                  _ -> Nothing 

      | isCommand update "optimize" = do
         if not (isRegistered update model)
          then Just NotAvailableCommand
          else Just Optimize

      | isCommand update "help" = Just Help

      | otherwise = do
        let msg = updateCallbackQuery update
        case msg of
          Nothing -> Just (ShowCallback "No such command =(\nWrite /help to see existing commands.")
          Just _msg -> case callbackQueryData _msg of
              Nothing -> Nothing 
              Just msgText -> case sharedAmount model of
                Just shareUser -> case readMaybe (unpack msgText) of
                                      Just x -> handleDebtevationQuery (callbackQueryFrom _msg) (getSecondElement shareUser) x
                                      Nothing -> Nothing 
                Nothing -> Nothing 
          where
            handleDebtevationQuery :: User -> User -> DebtevationQuery -> Maybe Action
            handleDebtevationQuery userClicked initiator (AcceptInKbQ message) = case message of
                (AcceptInKbQMessageUID _uid)
                  | _uid == userId userClicked -> Just (AcceptDebtAction _uid) 
                  | otherwise -> Nothing
                AcceptInKbQMessageCancel
                  | userName userClicked == userName initiator -> Just CancelShare
                  | otherwise -> Nothing
                AcceptInKbQMessageDone ->
                  if HashSet.isSubsetOf (acceptedDbors model) (shareDebtors model) &&
                      HashSet.isSubsetOf (shareDebtors model) (acceptedDbors model)
                    then Just FinishAccept
                    else Nothing
            handleDebtevationQuery user1 initiator (ShareInKbQ message)
              |  userName user1 == userName initiator = case message of
                ShareInKbQMessageDone -> Just FinishShare
                ShareInKbQMessageCancel -> Just CancelShare
                (ShareInKbQMessageUID _uid) -> Just (ChangeDebtor _uid)
              | otherwise = Nothing

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      Help -> model <# do
        replyText $ pack ( "/start - Register" ++ "\n"
                        ++ "/share <<num>> [description] - Shares money between registered users. Ex. /share 100 or /share 100 coca cola" ++ "\n"
                        ++ "/show - Displays all your debtors and everyone you owe money to" ++ "\n"
                        ++ "/optimize - Reduces number of transactions if it possible")

      Start usr -> registerUser usr model <# do
        if HashMap.member (userId usr) (users model) then
            replyText alreadyRegisteredMessage
        else replyText startMessage

      Share usr amount desc -> shareAmount (amount, usr, desc) model <# do
          reply
            (toReplyMessage (addDescription desc <> "Select users to share debt with:") )
              { replyMessageReplyMarkup = Just (SomeInlineKeyboardMarkup (shareKeyboard model)) }

      NotAvailableCommand ->
        model <# do 
          replyText needToRegisterMessage

      ShowCallback text ->
        model <# do
          replyText text

      Optimize -> optimizeDebts model <# do
        replyText $ pack "Sucessfully optimized"

      Show user ->
        model <# do
          replyText $ createDebtsMessage user model

      FinishShare ->
          model <# do
            editUpdateMessage
              (toEditMessage (getDescriptionFromModel model <> "Accept the debts:"))
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Just (SomeInlineKeyboardMarkup (showAcceptKbQ model)) }

      FinishAccept ->
        let newModel = addShares (HashSet.toList (shareDebtors model)) model in
          newModel {
            acceptedDbors = HashSet.empty,
            shareDebtors = HashSet.empty,
            sharedAmount  = Nothing
          } <# do
            editUpdateMessage
              (toEditMessage "Successfully accepted the debts =)")
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Nothing }

      CancelShare -> model {
        acceptedDbors = HashSet.empty,
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
              (toEditMessage (getDescriptionFromModel model <> "Select users to share debt with:"))
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Just (SomeInlineKeyboardMarkup (shareKeyboard newModel)) }

      AcceptDebtAction uid -> let newModel = model {
        acceptedDbors = if HashSet.member uid (acceptedDbors model) then
              acceptedDbors model
          else
              HashSet.insert uid (acceptedDbors model)
        } in newModel <# do
          editUpdateMessage
              (toEditMessage (getDescriptionFromModel model <> "Accept the debts:"))
              { Telegram.Bot.Simple.editMessageReplyMarkup  = Just (SomeInlineKeyboardMarkup (showAcceptKbQ newModel)) }

    -- Split Debt
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

    -- Accept Share
    showAcceptKbQ model = InlineKeyboardMarkup (namesButtons : [[doneButton, cancelButton]])
      where
        _users :: [User]
        _users = map toUser (HashSet.toList (shareDebtors model))

        toUser :: UserId -> User
        toUser _userId = fromJust (HashMap.lookup _userId (users model))

        namesButtons =
          [ callbackButton' (userToButtonName user) (AcceptInKbQ (AcceptInKbQMessageUID (userId user)))
          | user <- _users ]
        doneButton = callbackButton' "☑️ done" (AcceptInKbQ AcceptInKbQMessageDone)
        cancelButton = callbackButton' "✖️ cancel" (AcceptInKbQ AcceptInKbQMessageCancel)

        userToButtonName :: User -> Text
        userToButtonName user
            | selected  = "✅ " <> userName user
            | otherwise = "❌ " <> userName user
            where
                selected = HashSet.member (userId user) (acceptedDbors model)

    startMessage =
      Text.unlines
        [ "You successfully registered 🎉"]
    alreadyRegisteredMessage =
      Text.unlines
        [ "You are already registered 🤔️️️️️️"]
    needToRegisterMessage =
      Text.unlines
        [ "Before using this command, please, register using /start"]


-- | Helpers

getAmountOfShare :: Maybe Text -> Maybe Text
getAmountOfShare (Just text_) = Just (fst (breakOn " " (stripStart $ snd (breakOn " " text_))))
getAmountOfShare Nothing = Nothing

getDescriptionOfShare :: Maybe Text -> Maybe Text
getDescriptionOfShare (Just text_) = desc
  where
    desc_ = pack $ unwords $ drop 2 (words $ unpack text_)
    desc 
      | desc_ == "" = Nothing
      | otherwise = Just desc_
getDescriptionOfShare Nothing = Nothing

getDescriptionFromModel :: Model -> Text
getDescriptionFromModel model =
  case sharedAmount model of
    Just (_, _, text_) -> addDescription text_
    Nothing -> ""

userName :: User -> Text
userName user = firstName <> " " <> lastName
  where
    firstName = userFirstName user
    lastName = fromMaybe " " (userLastName user)

textToInt :: Maybe Text -> Maybe Int
textToInt (Just text_) = readMaybe (unpack text_)
textToInt Nothing = Nothing

shareAmount :: (Int, User, Maybe Text) -> Model -> Model
shareAmount x model =
  model
    { sharedAmount = Just x }

userIdToUserMapper :: Model -> UserId -> Maybe User
userIdToUserMapper model userID = HashMap.lookup userID (users model)

getSecondElement :: (a, b, Maybe c) -> b
getSecondElement (_, second, _) = second

addDescription :: Maybe Text -> Text
addDescription (Just text) = "\nDescription: " <> text <> "\n"
addDescription Nothing = ""

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
        
isCommand :: Update -> Text -> Bool
isCommand update cmd = 
  isJust (parseUpdate (command cmd) update) || 
  isJust (parseUpdate (command (cmd <> pack "@debtevation_bot")) update)

isRegistered :: Update -> Model -> Bool
isRegistered update model =
  case updateMessage update of
    Just msg ->
      case messageFrom msg of
        Just user -> HashMap.member (userId user) (users model)
          where
        _ -> False
    _ -> False

-- | Business Logic 

registerUser :: User -> Model -> Model
registerUser user model
    | HashMap.member (userId user) (users model) = model
    | otherwise = model
        { debtorsMap = HashMap.insert user HashMap.empty (debtorsMap model)
        , users = HashMap.insert (userId user) user (users model) }

addShares :: [UserId] -> Model -> Model
addShares userIds model = case sharedAmount model of
  Nothing -> model
  Just (number, usr, _) ->
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

-- https://www.win.tue.nl/~wstomv/publications/settling-debts-problems.pdf
optimizeDebts :: Model -> Model
optimizeDebts model = let newModel = model {
    debtorsMap = optimize _balance _emptyDebtors
  } in newModel
  where
    _debtors :: HashMap User (HashMap User Int)
    _debtors = debtorsMap model

    _emptyDebtors :: HashMap User (HashMap User Int) 
    _emptyDebtors = HashMap.fromList (map (, HashMap.empty) _users)

    _users :: [User]
    _users = HashMap.elems (users model)

    _balance :: HashMap User Int
    _balance = calculateBalance _users _debtors HashMap.empty

    calculateBalance :: [User] -> HashMap User (HashMap User Int) -> HashMap User Int -> HashMap User Int
    calculateBalance [] _ currBalances = currBalances
    calculateBalance (usr:usrs) debtors currBalances =
      calculateBalance usrs debtors (HashMap.insert usr balance currBalances)
      where
        balance = HashMap.foldr' (+) 0 (HashMap.lookupDefault HashMap.empty usr debtors)

    nonZeroBalanceExists :: HashMap User Int -> Bool
    nonZeroBalanceExists mp = HashMap.size (HashMap.filter (/= 0) mp) /= 0

    selectTwoUsers :: HashMap User Int -> ((User, Int), (User, Int))
    selectTwoUsers balances = (neg, pos)
      where
        neg = head $ HashMap.toList $ HashMap.filter (< 0) balances
        pos = head $ HashMap.toList $ HashMap.filter (> 0) balances

    optimize :: HashMap User Int -> HashMap User (HashMap User Int) -> HashMap User (HashMap User Int)
    optimize balance debtors
      | nonZeroBalanceExists balance = optimize newBalance newDebtors
      | otherwise = debtors
      where
        ((lusr, negval), (rusr, posval)) = selectTwoUsers balance
        m = min (-negval) posval

        internalLusr = HashMap.lookupDefault HashMap.empty lusr debtors
        currDebtLusr = HashMap.lookupDefault 0 rusr internalLusr
        internalLusrNew = HashMap.insert rusr (currDebtLusr - m) internalLusr

        internalRusr = HashMap.lookupDefault HashMap.empty rusr debtors
        currDebtRusr = HashMap.lookupDefault 0 lusr internalRusr
        internalRusrNew = HashMap.insert lusr (currDebtRusr + m) internalRusr

        newDebtors = HashMap.insert rusr internalRusrNew (HashMap.insert lusr internalLusrNew debtors)
        newBalance = HashMap.insert lusr (negval + m) (HashMap.insert rusr (posval - m) balance)

-- | Bot Run 

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId debtBot) env

main :: IO ()
main = do
  putStrLn "Starting...\nEnter token:"
  token <- Token . Text.pack <$> getLine
  run token
