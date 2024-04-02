-- Imports ----------------------------------------------------------------------------------------
import Prelude hiding (lookup)
import Data.Maybe (isJust, fromMaybe)
import Debug.Trace (trace)
import Data.Typeable (typeOf)
import Data.List (elemIndices, sortBy)
import Data.Map (Map, fromList, lookup)
import Data.Set (Set, difference, fromList, intersection, member, singleton, toList, unions)

-- Utils ------------------------------------------------------------------------------------------
red :: String -> String
red string = "\x1b[31m" <> string <> "\x1b[0m"


validVariableChars :: [Char]
validVariableChars = ['a'..'z'] ++ ['a'..'z']

atEnd :: Int -> [a] -> Bool
atEnd index list = index == length list - 1

count :: [Int]
count = [0..]

enumerate :: [a] -> [(Int, a)]
enumerate = zip count 

denumerate :: [(Int, a)] -> [a]
denumerate = map snd

slice :: Int -> Int -> [a] -> [a]
slice start stop list = denumerate $ filter (\(i, _) -> (start <= i) && (i < stop)) (enumerate list)

splitAtIndex :: Int -> [a] -> Maybe ([a], a, [a])
splitAtIndex index list
  | (index < 0) || (index >= length list) = Nothing
  | otherwise = Just (slice 0 index list, list !! index, slice (index + 1) (length list) list)
  

-- Error Messages ---------------------------------------------------------------------------------
badTokenMessage :: Char -> String
badTokenMessage char = "Invalid Token: " <> [char]


bracesMismatch1, bracesMismatch2, impossibleFromMaybe, applyBetaSplitFailure :: String
bracesMismatch1 = "Braces do not match (not all opened braces were closed)"
bracesMismatch2 = "Braces do not match (there is a closed brace before an opened one)"
impossibleFromMaybe = "This error is impossible because there is a isJust clause before fromMaybe"
applyBetaSplitFailure = "`splitAtIndex` performed on branch or lambda stems failed"

-- Lexer ------------------------------------------------------------------------------------------
data Token =
    Sign
  | Dot
  | LBrace
  | RBrace
  | Letter !Char
  deriving (Eq, Show, Ord)


token :: Char -> Token
token char
  | char `elem` ['λ', '\\', 'ł'] = Sign
  | char == '.' = Dot
  | char `elem` ['(', '['] = LBrace
  | char `elem` [')', ']'] = RBrace
  | char `elem` validVariableChars = Letter char
  | otherwise = error $ red $ badTokenMessage char


braceReduce :: Int -> Token -> Int
braceReduce x token
  | token == LBrace = x + 1
  | token == RBrace = x - 1
  | otherwise = x


braceFoldl :: Int -> [Token] -> Int
braceFoldl val tokens
  | null tokens = val
  | val < 0 = error $ red bracesMismatch1
  | otherwise =
    let token = head tokens in
    let tail_tokens = tail tokens in
    braceFoldl (braceReduce val token) tail_tokens


checkMatchingBraces :: [Token] -> [Token]
checkMatchingBraces tokens
  | braceFoldl 0 tokens == 0 = tokens
  | otherwise = error $ red bracesMismatch2 


tokenise :: String -> [Token]
tokenise string = checkMatchingBraces $ map token string

-- Parser -----------------------------------------------------------------------------------------
data Node =
    Leaf {val :: !Token}
  | Branch {stems :: ![Node]}
  | Lambda {symbol :: !Token, definition :: ![Node]}
  deriving (Show)

data Stack = Stack {nodes :: ![Node], size :: Int} deriving (Show)


newStack :: [Node] -> Stack
newStack nodes = Stack {nodes = nodes, size = length nodes}


empty :: Stack -> Bool
empty stack = size stack == 0


push :: Node -> Stack -> Stack
push node stack = Stack {nodes = node : nodes stack, size = size stack + 1}


peak :: Stack -> Maybe Node
peak stack
  | empty stack = Nothing
  | otherwise = Just $ head $ nodes stack


pop :: Stack -> (Maybe Node, Stack)
pop stack
  | empty stack = (Nothing, stack)
  | otherwise =
    let stack_nodes = nodes stack in
    let stack_size = size stack in
    (Just $ head stack_nodes, Stack {nodes = tail stack_nodes, size = stack_size - 1})


-- `under` takes whatever is on top of the stack and makes it a child node of the one underneath
under :: Stack -> Stack
under stack = 
  let (maybe_node, popped_stack) = pop stack in
  if isJust maybe_node then
    let node = fromMaybe (error $ red impossibleFromMaybe) maybe_node in
    let (maybe_next, new_stack) = pop popped_stack in
      if isJust maybe_next then
        let next = fromMaybe (error $ red impossibleFromMaybe) maybe_next in
        push (Branch {stems = stems next ++ [node]}) new_stack
      else stack -- if there's nothing to push the top item under
  else stack -- if the stack is empty


treeStep :: Token -> Stack -> Stack
treeStep token stack
  | token == LBrace = push Branch {stems = []} stack
  | token == RBrace = under stack
  | otherwise = under $ push Leaf {val = token} stack


makeTreeRecur :: [Token] -> Stack -> Stack
makeTreeRecur tokens stack
  | null tokens = stack
  | otherwise =
    let token = head tokens in
    let tokens_tail = tail tokens in
    makeTreeRecur tokens_tail (treeStep token stack)

-- makeTree algorithm takes into account brackets as tokens are converted to nodes and pushed onto
-- a stack. If the bracket is open then a new node is pushed onto the stack. When the bracket is
-- closed the node on top becomes a child node of the node underneath it on the stack. Other tokens
-- are put underneath the top node immediately when consumed.

makeTree :: [Token] -> Node
makeTree tokens = head $ nodes $ makeTreeRecur tokens (newStack [Branch {stems = []}])


isBranch :: Node -> Bool
isBranch (Branch _) = True
isBranch _ = False


makeLambda :: [Node] -> [Node]
makeLambda nodes
  | isBranch $ head nodes = nodes
  | let head_token = val $ head nodes in
    head_token == Sign = [Lambda {symbol = val (nodes !! 1), definition = drop 3 nodes}]
  | otherwise = nodes


makeTreeLambdas :: Node -> Node
makeTreeLambdas (Leaf val) = Leaf {val}
makeTreeLambdas (Branch stems) = Branch {stems = map makeTreeLambdas (makeLambda stems)}
makeTreeLambdas (Lambda symbol definition) =
  Lambda {symbol, definition = map makeTreeLambdas (makeLambda definition)}


simplifyTree :: Node -> Node
simplifyTree (Leaf val) = Leaf {val}
simplifyTree (Lambda symbol definition) = Lambda {symbol, definition = map simplifyTree definition}
simplifyTree (Branch stems)
  | length stems == 1 = simplifyTree $ head stems
  | otherwise = Branch {stems = map simplifyTree stems} 


-- the main parse function that takes tokens, parses them into trees, forms lambda nodes and then
-- simplifies any unnecessary node nesting
parse :: [Token] -> Node
parse tokens = simplifyTree $ makeTreeLambdas $ makeTree tokens

-- Visualisation ----------------------------------------------------------------------------------
notateToken :: Token -> String
notateToken (Letter c) = c:""
notateToken token
  | token == Sign = "λ"
  | token == Dot = "."
  | token == LBrace = "("
  | token == RBrace = ")"


braceWrap :: String -> String
braceWrap string = "(" ++ string ++ ")"


-- converts an AST back to string for visualisation
notation :: Node -> String
notation = notationRecur 0


notationRecur :: Int -> Node -> String
notationRecur _ (Leaf val) = notateToken val

notationRecur i (Branch stems)
  | i == 0 = content
  | otherwise = braceWrap content
  where content = concatMap (notationRecur (i + 1)) stems

notationRecur i (Lambda symbol definition)
  | i == 0 = content
  | otherwise = braceWrap content
  where content = "λ" ++ notateToken symbol ++ "." ++ concatMap (notationRecur (i + 1)) definition

-- Evaluation -------------------------------------------------------------------------------------
isLambda :: Node -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False


-- if sub contain [(...) Lambda Node (...)] then return it's position,
findApplicationContext :: Node -> Maybe Int
findApplicationContext (Leaf _) = Nothing

findApplicationContext (Branch stems)
  | null searched_lambdas = Nothing -- branch nodes with no lambda expressions
  | head searched_lambdas `atEnd` stems = Nothing -- if found lambda has no args
  | otherwise = Just $ head searched_lambdas
  where searched_lambdas = elemIndices True (map isLambda stems)

findApplicationContext (Lambda _ definition)
  | null searched_lambdas = Nothing -- branch nodes with no lambda expressions
  | head searched_lambdas `atEnd` definition = Nothing -- if found lambda has no args
  | otherwise = Just $ head searched_lambdas
  where searched_lambdas = elemIndices True (map isLambda definition)

data Reduction = Reduction {
  before :: ![Node],
  lambda :: !Node,
  arg    :: !Node,
  after  :: ![Node]
}

lambdaWithArg :: ([Node], Node, [Node]) -> Reduction
lambdaWithArg (before, node, after) = Reduction{
  before,
  lambda=node,
  arg=head after,
  after = tail after
}

splitMaybePos :: Maybe Int -> [Node] -> Maybe ([Node], Node, [Node])
splitMaybePos maybe_lambda_pos = 
  splitAtIndex (fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos)

-- Alpha Conversion -------------------------------------------------------------------------------
-- applies and alpha conversion where all tokens within a node are replaced so that there are no
-- name clashes when applying beta reduction
alpha :: Map Token Token -> Node -> Node
alpha tok_map (Leaf val)
  | isJust maybe_new_val = Leaf{val = fromMaybe (error $ red impossibleFromMaybe) maybe_new_val}
  where maybe_new_val = lookup val tok_map

alpha tok_map (Branch stems) = Branch{stems = map (alpha tok_map) stems}

alpha tok_map (Lambda symbol definition)
  = Lambda{symbol, definition = map (alpha tok_map) definition}


-- gets all of the tokens from a node as candidate targets to be replaced
nodeTokens :: Node -> Set Token
nodeTokens (Leaf val) = singleton val
nodeTokens (Branch stems) = unions $ map nodeTokens stems
nodeTokens (Lambda _ definition) = unions $ map nodeTokens definition


-- gets all the tokens from a node that applies to a lambda. if itself is a lambda all members must
-- be acconted for
argTokens :: Node -> Set Token
argTokens (Leaf val) = singleton val
argTokens (Branch stems) = argTokens $ head stems
argTokens (Lambda symbol definition) = nodeTokens Lambda{symbol, definition} 


tokenMap :: Node -> Node -> Map Token Token
tokenMap lambda args =
  Data.Map.fromList $ zip (toList replacement_tokens) (sortBy token_comp (toList available_tokens))
  where lambda_tokens = nodeTokens lambda
        arg_tokens = argTokens args
        all_tokens = Data.Set.fromList $ map Letter validVariableChars
        replacement_tokens = lambda_tokens `intersection` arg_tokens
        available_tokens = (all_tokens `difference` lambda_tokens) `difference` replacement_tokens
        token_comp x y = compare (notateToken x) (notateToken y)


applyAlpha :: Node -> Node
applyAlpha (Leaf val) = Leaf{val}

-- TODO --
applyAlpha (Branch stems)
  | isJust maybe_lambda_pos = 
    let context = fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos in
    let split = splitMaybePos maybe_lambda_pos stems in
    let alpha_reduction = lambdaWithArg (fromMaybe (error $ red applyBetaSplitFailure) split) in
    undefined -- Define a function to apply alpha rule to reduction and return branch node
  where maybe_lambda_pos = findApplicationContext Branch{stems}

applyAlpha (Lambda symbol definition)
  | isJust maybe_lambda_pos = 
    let context = fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos in
    let split = splitMaybePos maybe_lambda_pos definition in
    let alpha_reduction = lambdaWithArg (fromMaybe (error $ red applyBetaSplitFailure) split) in
    undefined -- Define a function to apply alpha rule to reduction and return lambda node
  where maybe_lambda_pos = findApplicationContext Lambda{symbol, definition}

-- END TODO --

-- Beta Reduction ---------------------------------------------------------------------------------
-- given a lambda expression and a symbol, all matching symbols within the expression are replaced
-- with that value. beta _ (Branch stems) supports currying by evaluating the first node of a
-- branch node if there exist multiple stems in the branch
beta :: Node -> Node -> Node
beta (Lambda symbol definition) (Branch stems)
  | length stems == 1 = beta Lambda{symbol, definition} (head stems)
  | otherwise = Branch{stems = beta Lambda{symbol, definition} (head stems) : tail stems}

beta (Lambda symbol definition) replace
  | length new_stems == 1 = head new_stems
  | otherwise = Branch{stems = new_stems} 
  where new_stems = map (betaRecur symbol replace) definition


betaRecur :: Token -> Node -> Node -> Node
betaRecur find replace (Leaf val) = if val == find then replace else Leaf {val}

betaRecur find replace (Branch stems) =
  if length new_stems == 1 then head new_stems else Branch{stems = new_stems} 
  where new_stems = map (betaRecur find replace) stems

betaRecur find replace (Lambda symbol definition) =
  Lambda {symbol, definition = map (betaRecur find replace) definition}

betaReductionToBranch :: Reduction -> Node
betaReductionToBranch beta_reduction = Branch{
  stems =
       before beta_reduction
    ++ [beta (lambda beta_reduction) (arg beta_reduction)]
    ++ after beta_reduction
}

betaReductionToLambda :: Token -> Reduction -> Node 
betaReductionToLambda symbol beta_reduction = Lambda {
  symbol,
  definition =
       before beta_reduction
    ++ [beta (lambda beta_reduction) (arg beta_reduction)]
    ++ after beta_reduction
}


shouldApplyBeta :: Node -> Bool
shouldApplyBeta (Leaf _) = False

shouldApplyBeta (Branch stems)
  | isJust $ findApplicationContext Branch{stems} = True
  | otherwise = any shouldApplyBeta stems

shouldApplyBeta (Lambda symbol definition)
  | isJust $ findApplicationContext Lambda{symbol, definition} = True
  | otherwise = any shouldApplyBeta definition


applyBeta :: Node -> Node
applyBeta (Leaf val) = Leaf{val}

applyBeta (Branch stems)
  | isJust maybe_lambda_pos =
    let lambda_pos = fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos in
    let split = splitMaybePos maybe_lambda_pos stems in
    let beta_reduction = lambdaWithArg (fromMaybe (error $ red applyBetaSplitFailure) split) in
    betaReductionToBranch beta_reduction
  | otherwise = Branch{stems=map applyBeta stems}
  where maybe_lambda_pos = findApplicationContext Branch{stems}

applyBeta (Lambda symbol definition)
  | isJust maybe_lambda_pos =
    let lambda_pos = fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos in
    let split = splitMaybePos maybe_lambda_pos definition in
    let beta_reduction = lambdaWithArg (fromMaybe (error $ red applyBetaSplitFailure) split) in
    betaReductionToLambda symbol beta_reduction
  | otherwise = Lambda{symbol, definition = map applyBeta definition}
  where maybe_lambda_pos = findApplicationContext Lambda {symbol, definition}

-- Main -------------------------------------------------------------------------------------------
main :: IO()
main =

  let expression = stems $ parse $ tokenise "(\\x.z)z" in
  let lambda = expression !! 0 in
  let arg = expression !! 1 in
  putStrLn (show $ tokenMap lambda arg)
