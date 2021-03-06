{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
       ( Circuit(..)
       , CircuitName(..)
       , FileLine
       , Juggler(..)
       , JugglerName(..)
       , JugglerRaw(..)
       , ProcessData(..)
       , PDState
       , Skill(..)
       , dotProduct
       ) where
import qualified Control.Monad.Identity as Id
import qualified Control.Monad.State as St
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Mon
import qualified Data.Semigroup as Sg

import Data.Monoid ((<>))

newtype CircuitName = CircuitName String deriving (Eq, Sg.Semigroup, Mon.Monoid, Ord)
newtype JugglerName = JugglerName String deriving (Eq, Ord)
type FileLine = Either.Either Circuit JugglerRaw
type PDState = St.StateT ProcessData Id.Identity
type CircuitDP = (CircuitName, Int)

data Skill = Skill
              { h :: Int
              , e :: Int
              , p :: Int
              }

data Circuit = Circuit
                { cName :: CircuitName
                , cSkill :: Skill
                }

data JugglerRaw = JugglerRaw
                  { jrName :: JugglerName
                  , jrSkill :: Skill
                  , jrPref :: [CircuitName]
                  }

data Juggler = Juggler
               { jName :: JugglerName
               , jSkill :: Skill
               , _jCircDP :: [CircuitDP]
               }

instance Show CircuitName where
  show (CircuitName s) = s

instance Show JugglerName where
  show (JugglerName s) = s

instance Show Skill where
  show (Skill h' e' p') = "<" <> str <> ">"
    where str = unwords . fmap show $ [h',e',p']

instance Show Circuit where
  show (Circuit cn sk) = unwords [show cn, show sk]

instance Show JugglerRaw where
  show (JugglerRaw jn sk cns) = unwords [show jn, show sk, show cns]

instance Show Juggler where
  show (Juggler jn _ dps) = unwords (show jn:m)
    where m = fmap (\(x,y) -> show x <> ":" <> show y) dps

data ProcessData = ProcessData
                   { _circMap   :: Map.Map CircuitName Circuit
                   , _juggMap   :: Map.Map JugglerName Juggler
                   , _circuits  :: Map.Map CircuitName [Juggler]
                   , _size      :: Int
                   , _toProcess :: [Juggler]
                   , _lost      :: [Juggler]
                   } deriving (Show)

instance Eq Juggler where
  (==) x y = getDP x == getDP y

instance Ord Juggler where
  (<=) x y = getDP x <= getDP y

getDP :: Juggler -> Maybe Int
getDP j = case _jCircDP j of
            []         -> Nothing
            ((_,dp):_) -> Just dp

-- Allow us to calculate dot products of anything that has skill
-- We use lenses so we need to wait until after the makeLenses
class DPCalc a where
  getSkill :: a -> Skill
  dotProduct :: (DPCalc b) => a -> b -> Int
  dotProduct u v = a*x + b*y + c*z
    where Skill a b c = getSkill u
          Skill x y z = getSkill v

instance DPCalc JugglerRaw where getSkill = jrSkill
instance DPCalc Circuit    where getSkill = cSkill
instance DPCalc Skill      where getSkill = id
