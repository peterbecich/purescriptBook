module Chapter8.ReactExamples where

-- import Prelude

-- import Data.Maybe
-- import Data.List
-- import Data.Int (toNumber)

-- import Math

-- import Data.AddressBook (Address)

-- import Control.Monad.Eff
-- import Control.Monad.Eff.Random (random, RANDOM)
-- -- import Control.Monad.Eff.Console (logShow, CONSOLE)

-- import Control.Monad.Eff.Exception

-- -- import Control.Monad.Eff (Eff, forE)
-- import Control.Monad.ST (ST, newSTRef, readSTRef, modifySTRef, runST)

-- import React.DOM as D
-- import React.DOM.Props as P
-- import DOM.HTML

-- import React

-- newtype Person = Person
--   { firstName :: String
--   , lastName :: String
--   , address :: Address
--   }

-- formField
--   :: String
--   -> String
--   -> String
--   -> (String -> Person)
--   -> ReactElement
-- formField name hint value update =
--   D.div [ P.className "form-group" ]
--         [ D.label [ P.className "col-sm-2 control-label" ]
--                   [ D.text name ]
--         , D.div [ P.className "col-sm-3" ]
--                 [ D.input [ P._type "text"
--                           , P.className "form-control"
--                           , P.placeholder hint
--                           , P.value value
--                           , P.onChange (updateAppState ctx update)
--                           ] []
--                 ]
--         ]
