
> {-# LANGUAGE TemplateHaskell #-}
> import Control.Lens
> import Data.List (intercalate)
> import Text.Printf (printf)

> type CircuitName = String
> type JugglerName = String
> data Skill = Skill { _h :: Int, _e :: Int, _p :: Int }
> data Circuit = Circuit { _cName :: CircuitName, _cSkill :: Skill }
> data Juggler = Juggler { _jName :: JugglerName
>                        , _jSkill :: Skill
>                        , _jPref :: [CircuitName]
>                        }


> instance Show (Skill) where
>   show (Skill h e p) = printf "<H:%d E:%d P:%d>" h e p
> 
> instance Show (Circuit) where
>   show (Circuit cn sk) = cn ++ " " ++ show sk
> 
> instance Show (Juggler) where
>   show (Juggler jn sk cns) = jn ++ " " ++ show sk ++ " " ++ showCNList cns
>     where showCNList = intercalate ","
> 

We are using TemplateHaskell language extension to avoid writing all
the setters and getters. This will generate them automatically

> makeLenses ''Skill
> makeLenses ''Circuit
> makeLenses ''Juggler
>           

ghci> let s = Skill 3 2 1

Here are how to use some getters

ghci> view e s
2
ghci> s^.e
2


Here are examples of setters

ghci> set e 6 s
<H:3 E:6 P:1>

ghci> p .~ 10 $ s
<H:3 E:2 P:10>

ghci> s & h .~ 7
<H:7 E:2 P:1>


ghci> let j = Juggler "J234" (Skill 38 29 20) ["C2134", "C1241"]
ghci> let c = Circuit "C232" (Skill 10 82 13)


Here's how we can append a Circuit to the Juggler's list

ghci> j & jPref %~ ("C129":)
J234 <H:38 E:29 P:20> C129,C2134,C1241

ghci> over jPref ("C129") j
J234 <H:38 E:29 P:20> C129,C2134,C1241

We can apply a function to every jPref like this:
ghci> over (jPref.mapped) tail j
J234 <H:38 E:29 P:20> 2134,1241

ghci> j & jPref.mapped %~ tail
J234 <H:38 E:29 P:20> 2134,1241   
   
