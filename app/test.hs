import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Telegram.Bot.API
import Telegram.Bot.Simple


instance Eq User where 
    x == y = (userId x) == (userId y)

instance Hashable User where 
    hashWithSalt salt user = salt + (fromIntegral uid)
        where
            uid = userIdToInteger (userId user)


userIdToInteger :: UserId -> Integer
userIdToInteger (UserId i) = i

testF :: HashMap User (HashMap User Int)
testF = HashMap.empty

testF' :: HashSet User
testF' = HashSet.empty