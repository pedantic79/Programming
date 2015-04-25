{-# LANGUAGE TemplateHaskell #-}
module Lens
       ( circuits
       , circMap
       , jCircDP
       , juggMap
       , lost
       , toProcess
       , size
       ) where
import qualified Control.Lens as Lens
import Types (Juggler,ProcessData)
   
-- Use TH calls to create our lenses
Lens.makeLenses ''Juggler
Lens.makeLenses ''ProcessData
         
