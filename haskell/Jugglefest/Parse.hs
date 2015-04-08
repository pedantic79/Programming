import Data.List (intercalate)
import Data.Text (pack,unpack,split,Text)
import Text.Printf (printf)



type CircuitName = String
type JugglerName = String

data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
data Juggler = Juggler { _jName :: JugglerName
                       , _jSkill :: Skill
                       , _jPref :: [CircuitName]
                       }


instance Show (Skill) where
  show (Skill h e p) = printf "<H%d E:%d P:%d>" h e p

instance Show (Circuit) where
  show (Circuit cn sk) = cn ++ " " ++ show sk

instance Show (Juggler) where
  show (Juggler jn sk cns) = jn ++ " " ++ show sk ++ " " ++ showCNList cns
    where showCNList = intercalate ","



makeCircuit :: String -> Circuit
makeCircuit str
  | head txList == "C" = Circuit (txList !! 1) (makeSkillLst (drop 2 txList))
  | otherwise = error "Parse Error"
  where txList = map unpack . split (==' ') . pack $ str

makeSkillLst :: [String] -> Skill
makeSkillLst ss = make ints
  where ints = map ((read :: String -> Int) . drop 2) ss
        make (a:b:c:[]) = Skill a b c

makeSkill :: String -> Skill
makeSkill str = makeSkillLst ss
  where ss = map unpack . split (== ' ') . pack $ str


