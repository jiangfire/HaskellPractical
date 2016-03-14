import qualified Data.Map as Map 
phoneBook :: [(String, String)]
phoneBook =
	[("betty", "555-2938"),
	("bonnie", "452-2928"),
	("patsy", "493-2928"),
	("luculle", "205-2928")]
type PhoneBook = [(String, String)]
type PhoneNumber = String
type Name = String

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Main.Either String Code
lockerLookup lockerNumber lmap =
	case Map.lookup lockerNumber lmap of
		Nothing -> Main.Left $ "Locker Number" ++ show lockerNumber ++ "doesn't exists!"
		Just (state, code) -> if state /= Taken
			then Main.Right code
			else Main.Left $ "Locker " ++ show lockerNumber ++ " is Free"

lockers :: LockerMap
lockers = Map.fromList [(100, (Taken, "ZD392")), (101, (Free, "JAH3I"))]