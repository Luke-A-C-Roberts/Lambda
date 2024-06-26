module Lambda where

-- Imports ----------------------------------------------------------------------------------------

import Prelude hiding (lookup)

import Data.Maybe (isJust, fromMaybe)
import Data.Function (on)

import Data.List (elemIndices, sort, findIndices, intersperse, groupBy)
import Data.Map (Map, keys, lookup, fromList)
import Data.Set (Set, difference, fromList, empty, intersection, member, null, singleton, toList,
  union, unions)

import Debug.Trace (trace)

-- Utils ------------------------------------------------------------------------------------------

color :: Int -> String -> String
color val string = "\x1b[" <> show val <> "m" <> string <> "\x1b[0m"

red, lred, lyellow, lcyan :: String -> String
red     = color 31
lred    = color 91
lyellow = color 93
lcyan   = color 96


validVariableChars, validLambdaChars :: [Char]
validVariableChars = ['a'..'z']
validLambdaChars = ['λ', '\\', 'ł']


atEnd :: Int -> [a] -> Bool
atEnd index list = index == length list - 1

count :: [Int]
count = [0..]

enumerate :: [a] -> [(Int, a)]
enumerate = zip count 

denumerate :: [(Int, a)] -> [a]
denumerate = map snd


slice :: Int -> Int -> [a] -> [a]
slice start stop list =
  denumerate $ filter (\(i, _) -> (start <= i) && (i < stop)) (enumerate list)

-- given index n, before (0..n), item (n), after (n+1..)
splitAtIndex :: Int -> [a] -> Maybe ([a], a, [a])
splitAtIndex index list
  | (index < 0) || (index >= length list) = Nothing
  | otherwise = Just (slice 0 index list, list !! index, slice (index + 1) (length list) list)


infix 5 +:
(+:) :: [a] -> a -> [a]
l +: v = l ++ [v]

-- given a list of type a, map a function producing sets and then union all of the sets
unionMap :: Ord b => (a -> Set b) -> [a] -> Set b
unionMap func items = unions $ map func items

-- Error Messages ---------------------------------------------------------------------------------

badTokenMessage :: Char -> String
badTokenMessage char = "Invalid Token: " <> [char]

bracesMismatch1, bracesMismatch2, impossibleFromMaybe, applySplitFailure, alphaOutOfSymbols,
  infinateRecursion, unknownTokenError :: String
bracesMismatch1     = "Braces do not match (not all opened braces were closed)"
bracesMismatch2     = "Braces do not match (there is a closed brace before an opened one)"
impossibleFromMaybe = "This error is impossible because there is a isJust clause before fromMaybe"
applySplitFailure   = "`splitAtIndex` performed on branch or lambda stems failed"
alphaOutOfSymbols   = "all symbols were expended when trying to complete alpha conversion"
infinateRecursion   = "It is possible that the function being run is going on infinately"
unknownTokenError   = "A token of unknown type has been traversed during display"

alphaInnerError, tokenMapError, betaError :: Node -> String
alphaInnerError node = "When trying to apply `shouldApplyAlphaInner` the first argument expected "
                     <>"`Lambda`, got " <> show node <> " instead"

tokenMapError node = "When trying to apply `alphaTokenMap` the first argument expected `Lambda`, "
                     <> "got " <> show node <> " instead"

betaError node = "When trying to apply `beta` the first argument expected `Lambda`, got "
                     <> show node <> " instead"

invalidArg :: String -> String
invalidArg arg = "Invalid argument " <> arg <> ". use -h for help message" 

-- Preprocessor -----------------------------------------------------------------------------------

keepTogetherLambdaHeads :: String -> [String]
keepTogetherLambdaHeads = groupBy ((&&) `on` (`elem` validChars))
  where validChars = validLambdaChars ++ validVariableChars


desugarParamList :: String -> String
desugarParamList string =
  if   length string > 1 && head string `elem` validLambdaChars
  then 'λ' : joinStrings (intersperse ".λ" $ map (:[]) $ tail string)
  else string


joinStrings :: [String] -> String
joinStrings = foldl (<>) ""

preprocessInput :: String -> String
preprocessInput = joinStrings . map desugarParamList . keepTogetherLambdaHeads

-- Tokeniser --------------------------------------------------------------------------------------

-- tokens represent certain characters from the input that will later be parsed
data Token =
    Sign
  | Dot
  | LBrace
  | RBrace
  | EmptyToken
  | Letter !Char
  deriving (Eq, Show, Ord)


token :: Char -> Token
token char
  | char `elem` validLambdaChars = Sign
  | char == '.' = Dot
  | char `elem` ['(', '['] = LBrace
  | char `elem` [')', ']'] = RBrace
  | char `elem` validVariableChars = Letter char
  | char `elem` [' ', '\t'] = EmptyToken
  | otherwise = error $ red $ badTokenMessage char


isEmptyToken :: Token -> Bool
isEmptyToken EmptyToken = True
isEmptyToken _ = False

filterNonEmpty :: [Token] -> [Token]
filterNonEmpty = filter (not . isEmptyToken)


-- keeps track of how many braces are opened and closed
braceReduce :: Int -> Token -> Int
braceReduce nestedness _token
  | _token == LBrace = nestedness + 1
  | _token == RBrace = nestedness - 1
  | otherwise = nestedness


braceFoldl :: Int -> [Token] -> Int
braceFoldl val tokens
  | Prelude.null tokens = val
  | val < 0 = error $ red bracesMismatch1
  | otherwise =
    let _token = head tokens in
    let tail_tokens = tail tokens in
    braceFoldl (braceReduce val _token) tail_tokens


checkMatchingBraces :: [Token] -> [Token]
checkMatchingBraces tokens
  | braceFoldl 0 tokens == 0 = tokens
  | otherwise = error $ red bracesMismatch2 


tokenise :: String -> [Token]
tokenise string = checkMatchingBraces $ filterNonEmpty $ map token string

-- Parser -----------------------------------------------------------------------------------------

data Node =
    Leaf {val :: !Token} -- leaf nodes are variables in definitions and as arguments
  | Branch {stems :: ![Node]} -- branches contain more nodes
  | Lambda {symbol :: !Token, definition :: ![Node]} -- lambdas have one parameter and a definition
  deriving (Show)

isLeaf :: Node -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False 

isBranch :: Node -> Bool
isBranch (Branch _) = True
isBranch _ = False

isLambda :: Node -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False


-- stack structure for correctly nesting tree baced on braces
data Stack = Stack {nodes :: ![Node], size :: !Int} deriving (Show)

newStack :: [Node] -> Stack
newStack nodes = Stack {nodes = nodes, size = length nodes}

nullStack :: Stack -> Bool
nullStack stack = size stack == 0

push :: Node -> Stack -> Stack
push node stack = Stack {nodes = node : nodes stack, size = size stack + 1}

peak :: Stack -> Maybe Node
peak stack
  | nullStack stack = Nothing
  | otherwise = Just $ head $ nodes stack

pop :: Stack -> (Maybe Node, Stack)
pop stack
  | nullStack stack = (Nothing, stack)
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
        push (Branch {stems = stems next +: node}) new_stack
      else stack -- if there's nothing to push the top item under
  else stack -- if the stack is empty there's nothing to do


-- if the token is a left brace then add ontop, if its a right brace push it under the current node
-- if it is anything else then imediately push it under the node ontop
treeStep :: Token -> Stack -> Stack
treeStep _token stack
  | _token == LBrace = push Branch {stems = []} stack
  | _token == RBrace = under stack
  | otherwise = under $ push Leaf {val = _token} stack


-- recursively build the tree using the list of tokens placed onto the stack. consume a token every
-- recursion until the token list is exausted.
makeTreeRecur :: [Token] -> Stack -> Stack
makeTreeRecur tokens stack
  | Prelude.null tokens = stack
  | otherwise =
    let _token = head tokens in
    let tokens_tail = tail tokens in
    makeTreeRecur tokens_tail (treeStep _token stack)


makeTree :: [Token] -> Node
makeTree tokens = head $ nodes $ makeTreeRecur tokens (newStack [Branch {stems = []}])


makeLambda :: [Node] -> [Node]
makeLambda nodes
  | isBranch $ head nodes = nodes
  | let head_token = val $ head nodes in
    head_token == Sign = [Lambda {symbol = val (nodes !! 1), definition = drop 3 nodes}]
  | otherwise = nodes


-- recusively DFS the node tree. if there is a branch or definition in the form λ[a-z]\.[a-z]+ then
-- convert to a lambda node.
makeTreeLambdas :: Node -> Node
makeTreeLambdas (Leaf val) = Leaf {val}
makeTreeLambdas (Branch stems) = Branch {stems = map makeTreeLambdas (makeLambda stems)}
makeTreeLambdas (Lambda symbol definition) =
  Lambda {symbol, definition = map makeTreeLambdas (makeLambda definition)}


-- inforces left associtivity i.e. abc -> (ab)c
associativitySize :: Int
associativitySize = 3

makeLeftAssociative :: Node -> Node
makeLeftAssociative (Leaf val) = Leaf{val}

makeLeftAssociative (Branch stems)
  | length stems < associativitySize = Branch{stems = map makeLeftAssociative stems}
  | otherwise = Branch{stems = [
      makeLeftAssociative $ Branch{stems = init stems},
      makeLeftAssociative $ last stems
    ]}

makeLeftAssociative (Lambda symbol definition)
  | length definition < associativitySize = Lambda{symbol, definition = map makeLeftAssociative definition}
  | otherwise = Lambda{symbol, definition = [
      makeLeftAssociative $ Branch{stems = init definition},
      makeLeftAssociative $ last definition
    ]}


-- make lambda and make tree can cause unnecessary branch node nesting so simplifyTree recursively
-- DFS traverses the tree and retracts the unnecessary single item branch nodes.
simplifyTree :: Node -> Node
simplifyTree (Leaf val) = Leaf {val}
simplifyTree (Lambda symbol definition) = Lambda {symbol, definition = map simplifyTree definition}
simplifyTree (Branch stems)
  | length stems == 1 = simplifyTree $ head stems
  | otherwise = Branch {stems = map simplifyTree stems} 


regulariseTree :: Node -> Node
regulariseTree = simplifyTree . makeLeftAssociative


-- the main parse function that takes tokens, parses them into trees, forms lambda nodes and then
-- simplifies any unnecessary node nesting
parse :: [Token] -> Node
parse tokens = regulariseTree $ makeTreeLambdas $ makeTree tokens

-- Visualisation ----------------------------------------------------------------------------------

notateToken :: Token -> String
notateToken (Letter c) = c:""
notateToken _token
  | _token == Sign   = "λ"
  | _token == Dot    = "."
  | _token == LBrace = "("
  | _token == RBrace = ")"

notateToken _ = undefined


braceWrap :: String -> String
braceWrap string = "(" ++ string ++ ")"


-- converts an AST back to string for visualisation
notation :: Node -> String
notation = notationRecur 0 0


notationRecur :: Int -> Int -> Node -> String
notationRecur _ _ (Leaf val) = notateToken val

notationRecur depth pos (Branch stems)
  | depth == 0 || pos == 0 = content -- if pos is zero then there it is in the form (ab)c -> "abc"
  | otherwise = braceWrap content
  where content = concatMap (uncurry $ notationRecur (depth + 1)) (enumerate stems)

notationRecur depth _ (Lambda symbol definition)
  | depth == 0 = content
  | otherwise = braceWrap content
  where content = "λ" ++ notateToken symbol ++ "."
                  ++ concatMap (uncurry $ notationRecur (depth + 1)) (enumerate definition)


showAST :: Int -> Node -> String
showAST depth (Leaf val) = replicate depth ' ' <> "+ " <> notateToken val +: '\n'

showAST depth (Branch stems) = start <> end
  where start = replicate depth ' ' <> "+ ()" +: '\n'
        end   = foldl (<>) "" (map (showAST (depth + 1)) stems)

showAST depth (Lambda symbol definition) = start <> end
  where start = replicate depth ' ' <> "+ λ" <> notateToken symbol +: '\n'
        end   = foldl (<>) "" (map (showAST (depth + 1)) definition)

-- Evaluation -------------------------------------------------------------------------------------

-- if node contains [(...) Lambda Node (...)] then return it's position,
findApplicationContext :: Node -> Maybe Int
findApplicationContext (Leaf _) = Nothing

findApplicationContext (Branch stems)
  | Prelude.null searched_lambdas = Nothing -- branch nodes with no lambda expressions
  | head searched_lambdas `atEnd` stems = Nothing -- if found lambda has no args
  | otherwise = Just $ head searched_lambdas
  where searched_lambdas = elemIndices True (map isLambda stems)

findApplicationContext (Lambda _ definition)
  | Prelude.null searched_lambdas = Nothing -- branch nodes with no lambda expressions
  | head searched_lambdas `atEnd` definition = Nothing -- if found lambda has no args
  | otherwise = Just $ head searched_lambdas
  where searched_lambdas = elemIndices True (map isLambda definition)


-- a datatype that holds a context where either the alpha or beta rule can apply and the
-- surrounding context
data Reduction = Reduction {
  before :: ![Node], -- everything before the to be reduced lambda
  lambda :: !Node, -- the lambda which will be reduced
  arg    :: !Node, -- the argument of the lambda
  after  :: ![Node] -- everything after in the array of nodes
} deriving (Show)

lambdaWithArg :: ([Node], Node, [Node]) -> Reduction
lambdaWithArg (before, node, after) = Reduction{
  before,
  lambda=node,
  arg=head after,
  after = tail after
}

-- only really exists because some lines were to many characters
splitMaybePos :: Maybe Int -> [Node] -> Maybe ([Node], Node, [Node])
splitMaybePos maybe_lambda_pos = 
  splitAtIndex (fromMaybe (error $ red impossibleFromMaybe) maybe_lambda_pos)

-- Alpha Conversion -------------------------------------------------------------------------------

alpha :: Map Token Token -> Node -> Node
alpha token_map (Leaf val)
  | val `elem` keys token_map
    = Leaf{val = fromMaybe (error $ red impossibleFromMaybe) $ lookup val token_map}
  | otherwise = Leaf{val}

alpha token_map (Branch stems) = Branch{stems = map (alpha token_map) stems}

alpha token_map (Lambda symbol definition)
  | symbol `elem` keys token_map
    = Lambda{
      symbol = fromMaybe (error $ red impossibleFromMaybe) $ lookup symbol token_map,
      definition = map (alpha token_map) definition
    }
  | otherwise = Lambda{symbol, definition = map (alpha token_map) definition}

variables :: Node -> Set Token
variables (Leaf val) = singleton val
variables (Branch stems) = unions $ map variables stems
variables (Lambda _ definition) = unions $ map variables definition


boundVariables :: Node -> Set Token
boundVariables (Leaf _) = empty
boundVariables (Branch stems) = unionMap boundVariables stems
boundVariables (Lambda symbol definition) =
  singleton symbol `union` unionMap boundVariables definition


freeVariables :: Node -> Set Token
freeVariables node = variables node `difference` boundVariables node


validVariables :: Set Token -> Set Token -> Set Token
validVariables target_tokens lambda_tokens = 
  Data.Set.fromList (map Letter validVariableChars)
  `difference` target_tokens
  `difference` lambda_tokens


-- given variables that need to be replaced and variables within a lambda that are forbidden from
-- being used, return a map of targets and replacements 
newVariables :: Int -> Set Token -> Set Token -> Map Token Token
newVariables i target_tokens lambda_tokens = Data.Map.fromList $ take i
  $ zip (sort $ toList target_tokens) (sort $ toList $ validVariables target_tokens lambda_tokens)


-- Finds if there is at least one position in the tree that contains an application context. of it
-- does then determine if the lambda and argument at that position should undergo alpha conversion
shouldApplyAlpha :: Node -> Bool
shouldApplyAlpha (Leaf _) = False

shouldApplyAlpha (Branch stems)
  | isJust maybe_context =
    let context = fromMaybe (error $ red impossibleFromMaybe) maybe_context in
    let lambda = stems !! context in
    let arg = stems !! (context + 1) in
    shouldApplyAlphaInner lambda arg
  | otherwise = any shouldApplyAlpha stems
  where maybe_context = findApplicationContext Branch{stems}

shouldApplyAlpha (Lambda symbol definition)
  | isJust maybe_context =
    let context = fromMaybe (error $ red impossibleFromMaybe) maybe_context in
    let lambda = definition !! context in
    let arg = definition !! (context + 1) in
    shouldApplyAlphaInner lambda arg
  | otherwise = any shouldApplyAlpha definition
  where maybe_context = findApplicationContext Lambda{symbol, definition}


filterLeaves :: [Node] -> [Node]
filterLeaves = filter isLeaf


-- determines if a lambda and an argument should undergo alpha conversion. It occurs if one or more
-- arguments are the same as any bound variables within a nested function. The outer function
-- doesn't need to be searched since it will get replaced in beta reduction in the next step.
shouldApplyAlphaInner :: Node -> Node -> Bool
shouldApplyAlphaInner (Lambda _ definition) (Leaf val)
  = val `member` boundVariables Branch{stems = definition}

shouldApplyAlphaInner (Lambda symbol definition) (Branch stems) =
  not $ Data.Set.null (arg_variables `intersection` bound_variables)
  where symbol_set = singleton symbol
        arg_variables = variables Branch {stems}
        bound_variables =
          boundVariables Lambda {symbol, definition} `difference` symbol_set

shouldApplyAlphaInner (Lambda symbol1 definition1) (Lambda symbol2 definition2) =
  not $ Data.Set.null (arg_variables `intersection` bound_variables)
  where symbol_set = singleton symbol1
        arg_variables = variables Lambda {
          symbol = symbol2,
          definition = definition2
        }
        bound_variables =
          boundVariables Lambda {
            symbol = symbol1,
            definition = definition1
          } `difference` symbol_set

shouldApplyAlphaInner node _ = error $ red $ alphaInnerError node

alphaTokenMap :: Node -> Node -> Maybe (Map Token Token)
alphaTokenMap (Lambda _ definition) (Leaf val)
  | val `member` bound_variables = Just $ newVariables 1 (singleton val) varaibles
  | otherwise = Nothing
  where bound_variables = boundVariables Branch{stems = definition}
        varaibles = variables Branch{stems = definition}

alphaTokenMap (Lambda _ definition) (Branch stems)
  | not $ Data.Set.null substitution_variables =
    Just $ newVariables (length substitution_variables) substitution_variables all_variables
  | otherwise = Nothing
  where arg_variables = variables Branch {stems}-- Data.Set.fromList $ map val $ filterLeaves stems
        bound_variables = boundVariables Branch{stems = definition}
        all_variables = variables Branch{stems = definition}
        substitution_variables = arg_variables `intersection` bound_variables

alphaTokenMap (Lambda symbol1 definition1) (Lambda symbol2 definition2)
  | not $ Data.Set.null substitution_variables =
    Just $ newVariables (length substitution_variables) substitution_variables all_variables
  | otherwise = Nothing
  where arg_variables = variables Lambda{
          symbol = symbol2,
          definition = definition2
        }--Data.Set.fromList $ map val $ filterLeaves definition2
        bound_variables = boundVariables Lambda {
          symbol = symbol1,
          definition = definition1
        }
        all_variables = variables Lambda{
          symbol = symbol1,
          definition = definition1
        }
        substitution_variables = arg_variables `intersection` bound_variables

alphaTokenMap node _ = error $ red $ tokenMapError node

alphaReductionToBranch :: Reduction -> Node
alphaReductionToBranch reduction 
  | isJust maybe_alpha_token_map = Branch {
    stems = before reduction
        ++ [
          alpha (fromMaybe (error $ red impossibleFromMaybe) maybe_alpha_token_map)
          (lambda reduction),
          arg reduction
        ]
        ++ after reduction
  }
  | otherwise = Branch {
    stems = before reduction
      ++ [lambda reduction, arg reduction]
      ++ after reduction
  }
  where maybe_alpha_token_map = alphaTokenMap (lambda reduction) (arg reduction)

alphaReductionToLambda :: Token -> Reduction -> Node
alphaReductionToLambda symbol reduction 
  | isJust maybe_alpha_token_map = Lambda {
    symbol,
    definition = before reduction
      ++ [
        alpha (fromMaybe (error $ red impossibleFromMaybe) maybe_alpha_token_map)
        (lambda reduction),
        arg reduction
      ]
      ++ after reduction
  }
  | otherwise = Lambda {
    symbol,
    definition = before reduction
      ++ [lambda reduction, arg reduction]
      ++ after reduction
  }
  where maybe_alpha_token_map = alphaTokenMap (lambda reduction) (arg reduction)


applyAlpha :: Node -> Node
applyAlpha (Leaf val) = Leaf{val}

applyAlpha (Branch stems)
  | isJust maybe_context =
    let split = splitMaybePos maybe_context stems in
    let alpha_reduction = lambdaWithArg (fromMaybe (error $ red applySplitFailure) split) in
    alphaReductionToBranch alpha_reduction
    
  | otherwise = Branch{stems = map applyAlpha stems}
  where maybe_context = findApplicationContext Branch{stems}

applyAlpha (Lambda symbol definition)
  | isJust maybe_context =
    let split = splitMaybePos maybe_context definition in
    let alpha_reduction = lambdaWithArg (fromMaybe (error $ red applySplitFailure) split) in
    alphaReductionToBranch alpha_reduction
    
  | otherwise = Lambda{symbol, definition = map applyAlpha definition}
  where maybe_context = findApplicationContext Lambda{symbol, definition}

-- Beta Reduction ---------------------------------------------------------------------------------

-- given a lambda expression and a symbol, all matching symbols within the expression are replaced
-- with that value. beta _ (Branch stems) supports currying by evaluating the first node of a
-- branch node if there exist multiple stems in the branch
beta :: Node -> Node -> Node
beta (Lambda symbol definition) replace
  | length new_stems == 1 = head new_stems
  | otherwise = Branch{stems = new_stems} 
  where new_stems = map (betaRecur symbol replace) definition

beta node _ = error $ red $ betaError node

betaRecur :: Token -> Node -> Node -> Node
betaRecur find replace (Leaf val) = if val == find then replace else Leaf {val}

betaRecur find replace (Branch stems) =
  Branch {stems = map (betaRecur find replace) stems}

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
applyBeta (Leaf val) = Leaf{val} -- applying beta reduction does nothing to a free varible

applyBeta (Branch stems)
  | not $ Prelude.null child_reduction_positions =
    let child_reduction_pos = head child_reduction_positions in
    let maybe_split = splitAtIndex child_reduction_pos stems in
    let (hd, node, tl) = fromMaybe (error $ red impossibleFromMaybe) maybe_split in
    Branch {stems = hd ++ [applyBeta node] ++ tl}

  | isJust maybe_lambda_pos =
    let split = splitMaybePos maybe_lambda_pos stems in
    let beta_reduction = lambdaWithArg (fromMaybe (error $ red applySplitFailure) split) in
    betaReductionToBranch beta_reduction

  | otherwise = Branch{stems}
  where child_reduction_positions = findIndices shouldApplyBeta stems
        maybe_lambda_pos = findApplicationContext Branch{stems}

applyBeta (Lambda symbol definition)
  | not $ Prelude.null child_reduction_positions =
    let child_reduction_pos = head child_reduction_positions in
    let maybe_split = splitAtIndex child_reduction_pos definition in
    let (hd, node, tl) = fromMaybe (error $ red impossibleFromMaybe) maybe_split in
    Lambda {symbol, definition = hd ++ [applyBeta node] ++ tl}

  | isJust maybe_lambda_pos =
    let split = splitMaybePos maybe_lambda_pos definition in
    let beta_reduction = lambdaWithArg (fromMaybe (error $ red applySplitFailure) split) in
    betaReductionToLambda symbol beta_reduction

  | otherwise = Lambda{symbol, definition}
  where child_reduction_positions = findIndices shouldApplyBeta definition
        maybe_lambda_pos = findApplicationContext Lambda {symbol, definition}

-- Evaluation -------------------------------------------------------------------------------------

maxIteration, tabLength :: Int
maxIteration = 1000
tabLength = 6

evalRecur :: TraceType -> Int -> Node -> Node
evalRecur NoTrace _ _ | trace "" False = undefined

evalRecur TraceSteps iteration node
  | trace (lyellow (show iteration) <> space <> symbol <> " " <> notation node) False = undefined
  where symbol | shouldApplyAlpha node = lred "α"
               | shouldApplyBeta  node = lcyan "β"
               | otherwise = " "
        space =  replicate (tabLength - length (show iteration)) ' '

evalRecur TraceAST iteration node
  | trace ((show iteration +: '\n') <> showAST 0 node) False = undefined


evalRecur trace_type iteration node
  | iteration > maxIteration = error $ red infinateRecursion
  | to_eval_alpha = newEvalRecur $ applyAlpha node
  | to_eval_beta && not to_eval_alpha = newEvalRecur $ regulariseTree $ applyBeta node
  | otherwise = node
  where newEvalRecur = evalRecur trace_type (iteration + 1) 
        to_eval_alpha = shouldApplyAlpha node
        to_eval_beta  = shouldApplyBeta node

eval :: TraceType -> String -> String
eval trace_type = notation . evalRecur trace_type 0 . parse . tokenise . preprocessInput

-- Prompt -----------------------------------------------------------------------------------------

renotate :: String -> String
renotate = notation . parse . tokenise

data TraceType =
    NoTrace
  | TraceAST
  | TraceSteps
