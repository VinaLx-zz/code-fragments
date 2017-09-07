module Grammar where

import           Data.Foldable (foldl')
import           Data.List     (intercalate)
import qualified Data.Map      as M
import qualified Data.Set      as S

data Symbol = T String | NT NonTerminal

data Production = P [Symbol] | Epsilon

newtype Rule = Rule [Production]

data NonTerminal = NonTerm {
    name :: String,
    rule :: Rule
}

data Grammar = Grammar {
    nonTerms    :: M.Map String NonTerminal,
    startSymbol :: NonTerminal
}

instance Show Symbol where
    show (T s)   = '"' : s ++ "\""
    show (NT nt) = name nt

instance Show Production where
    show Epsilon = "<epsilon>"
    show (P ts)  = unwords $ map show ts

instance Show NonTerminal where
    show (NonTerm n (Rule prods)) =
        let nl = length n
            padding = replicate (nl + 1) ' ' ++ "| "
            phead = head prods
            ptail = tail prods
         in n ++ " = " ++ show phead ++ "\n"
                 ++ intercalate "\n" (map ((padding ++) . show) ptail)
                 ++ "\n"

instance Show Grammar where
    show (Grammar nts start) =
        intercalate "\n" $ (:) (show start) $
            show . snd <$> ((/= name start) . fst) `filter` M.toList nts

fromStartSymbol :: NonTerminal -> Maybe Grammar
fromStartSymbol start = do
    (m, _) <- dfs M.empty (S.singleton $ name start) start
    return $ Grammar m start
    where dfs :: M.Map String NonTerminal -> S.Set String -> NonTerminal ->
                 Maybe (M.Map String NonTerminal, S.Set String)
          dfs m visited nt @ (NonTerm n (Rule ps)) = do
            (res, s) <- foldl' foldProd (Just (m, visited)) ps
            return (M.insert n nt res, s)
          foldProd :: Maybe (M.Map String NonTerminal, S.Set String) ->
                            Production ->
                            Maybe (M.Map String NonTerminal, S.Set String)
          foldProd Nothing _       = Nothing
          foldProd acc Epsilon     = acc
          foldProd acc (P symbols) = foldl' foldSymbol acc symbols
          foldSymbol :: Maybe (M.Map String NonTerminal, S.Set String) ->
                        Symbol ->
                        Maybe (M.Map String NonTerminal, S.Set String)
          foldSymbol Nothing _ = Nothing
          foldSymbol acc (T _) = acc
          foldSymbol acc @ (Just (m, visited)) (NT nt) =
            if name nt `S.member` visited
              then acc else dfs m (name nt `S.insert` visited) nt

lookupNT :: Grammar -> String -> Maybe NonTerminal
lookupNT = flip M.lookup . nonTerms

unsafeLookupNT :: Grammar -> String -> NonTerminal
unsafeLookupNT = (M.!) . nonTerms

nonTerminals :: Grammar -> [NonTerminal]
nonTerminals = map snd . M.toList . nonTerms

e :: NonTerminal
e = NonTerm "Expr" $ Rule
    [P [NT e, T "+", NT t], P [NT e]]


t = NonTerm "Term" $ Rule
    [P [NT t, T "*", NT f], P [NT f]]

f :: NonTerminal
f = NonTerm "Factor" $ Rule
    [P [T "(", NT e, T ")"], P [T "a"]]
