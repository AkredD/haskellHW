data Trie = Empty | Node Char [Trie] deriving (Show,Eq)
type Dictionary = [Trie]
-- showTrie :: Dictionary -> String
main' :: [[String]] 
main' = [
    showGoodTrie "" (addWord "a" (addWord "b" (addWord "c" []))),
    showGoodTrie "" (addWord "bitik" (addWord "site" (addWord "byte" (addWord "bite"
                    (addWord "bit" []))))),
    showGoodTrie "" (addWord "kruskall" [Node 'b' [Node 'i' [Node 't' [Empty,
                               Node 'e' [Empty]]],
                               Node 'y' [Node 't' [Node 'e' [Empty]]]],
                               Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]]),
    showGoodTrie "" (addWord "bit" [Node 'b' [Node 'i' [Node 't' [Empty,
                               Node 'e' [Empty]]],
                               Node 'y' [Node 't' [Node 'e' [Empty]]]],
                               Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]])]

main'' :: [Dictionary]
main'' = [
    (addWord "bit" [Node 'b' [Node 'i' [Node 't' [Empty,
                               Node 'e' [Empty]]],
           Node 'y' [Node 't' [Node 'e' [Empty]]]],
 Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]]),
    (addWord "kruskall" [Node 'b' [Node 'i' [Node 't' [Empty,
                               Node 'e' [Empty]]],
           Node 'y' [Node 't' [Node 'e' [Empty]]]],
 Node 's' [Node 'i' [Node 't' [Node 'e' [Empty]]]]])]
 
-- showTrie ((Node g listT):ys) ="[Node '" ++ g:[] ++ "' " ++ (showTrie(listT)) ++ showTrie(ys) ++ "]"
-- showTrie (_:ys) = "[Empty,"++showTrie(ys) ++ "]"
-- showTrie _ = ""


checkInTrie :: String -> Dictionary -> Bool
checkInTrie loc@(x:xs) trie@((Node g listT):ys) | x == g = checkInTrie xs listT
                                                | otherwise = checkInTrie loc ys
checkInTrie loc@(x:xs) (Empty:ys) = checkInTrie loc ys
checkInTrie (x:xs) _ = False
checkInTrie _ (Empty:ys) = True
checkInTrie _ _ = False
                                                  
deleteWord :: String -> Dictionary -> Dictionary
deleteWord loc dict | (checkInTrie loc dict) = snd $ deleteThisWord loc dict
                    | otherwise = dict

deleteThisSymbol ::(Bool, Dictionary) -> (Bool, Dictionary)
deleteThisSymbol (flag, trie@(Empty:ys)) = (flag, trie)
deleteThisSymbol (flag, trie@((Node g listT):ys)) | (flag && (ys == [])) = (False, trie)
                                                  | (not flag) = (False, trie)
                                                  | otherwise = (True, ys)                  
deleteThisWord :: String -> Dictionary -> (Bool, Dictionary)
deleteThisWord loc@(x:xs) ((Node g listT):ys) | x == g = (\(j,l) -> (j, (Node g l):ys)) $ deleteThisSymbol $ deleteThisWord xs listT
                                              | otherwise = (\(j,l) -> (j, (Node g listT):l)) $ deleteThisSymbol $ deleteThisWord loc ys
deleteThisWord loc@(x:xs) (Empty:ys) = deleteThisSymbol $ deleteThisWord loc ys
deleteThisWord (x:xs) _ = (False, [])
deleteThisWord _ trie@(Empty:ys) = (True, trie)



-- showGoodTrie :: String -> Dictionary -> [String]
showGoodTrie loc ((Node g listT):ys) = ((showGoodTrie (loc++g:[]) listT) ++ (showGoodTrie (loc) ys))
showGoodTrie loc (_:ys) = (loc:(showGoodTrie loc ys))
showGoodTrie loc _ = []

addWord :: String -> Dictionary -> Dictionary
addWord listD@(l:ls) ((Node g listT):ys)  | (l == g) = ((Node g (addWord ls listT)):ys)
                                          | otherwise = ((Node g listT):(addWord listD ys))
addWord listD (Empty:ys) = (Empty:(addWord listD ys))
addWord (l:ls) _ | ls == []  = (Node l [Empty]):[]
                 | otherwise = ((Node l (addWord ls [])):[])
addWord _ y = y                      