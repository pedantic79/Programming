{-# LANGUAGE TemplateHaskell #-}
module Types
       ( Parser
       , Skill(..)
       , Circuit(..)
       , CircuitName
       , Juggler(..)
       , JugglerName
       , JugglerRaw(..)
       , FileLine
       , PDState(..)
       , ProcessData(..)
       , cName
       , circuits
       , size
       , lost
       , circMap
       , jName
       , jrPref
       , jrName
       , jrSkill
       , dotProduct
       , juggMap
       , toProcess
       , jCircDP
       ) where
import qualified Control.Lens as Lens
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as Parsec
import Text.Printf (printf)

type CircuitName = String
type JugglerName = String
type Parser = Parsec.Parsec String ()
type FileLine = Either.Either Circuit JugglerRaw
type PDState = St.State ProcessData
type CircuitDP = (CircuitName, Int)

data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
data JugglerRaw = JugglerRaw
                  { _jrName :: JugglerName
                  , _jrSkill :: Skill
                  , _jrPref :: [CircuitName]
                  }
data Juggler = Juggler
               { _jName :: JugglerName
               , _jSkill :: Skill
               , _jCircDP :: [CircuitDP]
               }

instance Show (Skill) where
  show (Skill h e p) = printf "<%d %d %d>" h e p

instance Show (Circuit) where
  show (Circuit cn sk) = cn ++ " " ++ show sk

instance Show (JugglerRaw) where
  show (JugglerRaw jn sk cns) = unwords [jn, show sk, show cns]

instance Show (Juggler) where
  show (Juggler jn _ dps) = jn ++ " " ++ unwords m
    where m = map (\(x,y) -> x ++ ":" ++ show y) dps

data ProcessData = ProcessData
                   { _circMap :: Map.Map CircuitName Circuit
                   , _juggMap :: Map.Map JugglerName Juggler
                   , _circuits :: Map.Map CircuitName [Juggler]
                   , _size :: Int
                   , _toProcess :: [Juggler]
                   , _lost :: [Juggler]
                   } deriving (Show)

-- Use TH calls to create our lenses
Lens.makeLenses ''Skill
Lens.makeLenses ''Circuit
Lens.makeLenses ''JugglerRaw
Lens.makeLenses ''Juggler
Lens.makeLenses ''ProcessData

instance Eq (Juggler) where
  (==) x y = getDP x == getDP y

instance Ord (Juggler) where
  (<=) x y = getDP x <= getDP y

getDP :: Juggler -> Int
getDP j = case _jCircDP j of
           [] -> -1
           ((_,p):_) -> p

-- Allow us to calculate dot products of anything that has skill
-- We use lenses so we need to wait until after the makeLenses
class DPCalc a where
  getSkill :: (DPCalc a) => a -> Skill
  dotProduct :: (DPCalc a, DPCalc b) => a -> b -> Int
  dotProduct u v = a*x + b*y + c*z
    where Skill a b c = getSkill u
          Skill x y z = getSkill v

instance DPCalc (JugglerRaw) where getSkill = _jrSkill
instance DPCalc (Circuit) where getSkill = _cSkill
instance DPCalc (Skill) where getSkill = id

