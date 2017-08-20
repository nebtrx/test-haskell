{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}


module Lib where

import           Data.List           (intercalate)
import           Language.Haskell.TH

emptyShow :: Name -> Q [Dec]
emptyShow name = [d|instance Show $(conT name) where show _ = "_empty_"|]

listFields :: Name -> Q [Dec]
listFields name = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name

    let names = map (\(name,_,_) -> name) fields

        showField :: Name -> Q Exp
        showField name = [|\x -> s ++ " = " ++ show ($(varE name) x)|] where
            s = nameBase name

        showFields :: Q Exp
        showFields = listE $ map showField names

    [d|instance Show $(conT name) where
        show x = intercalate ", " (map ($ x) $showFields)|]
