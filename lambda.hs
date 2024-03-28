-- Imports ----------------------------------------------------------------------------------------
import Data.Maybe (isJust, fromMaybe)
import Debug.Trace (trace)
import Data.Typeable (typeOf)

-- Utils ------------------------------------------------------------------------------------------
red :: String -> String
red string = "\x1b[31m" <> string <> "\x1b[0m"

-- Error Messages ---------------------------------------------------------------------------------
badTokenMessage :: Char -> String
badTokenMessage char = "Invalid Token: " <> [char]

bracesMismatch1, bracesMismatch2, impossibleFromMaybe :: String
bracesMismatch1 = "Braces do not match (not all opened braces were closed)"
bracesMismatch2 = "Braces do not match (there is a closed brace before an opened one)"
impossibleFromMaybe = "This error is impossible because there is a isJust clause before fromMaybe"

-- Lexer ------------------------------------------------------------------------------------------
data Token =
    Sign
  | Dot
  | LBrace
  | RBrace
  | Letter !Char
  deriving (Eq, Show)

token :: Char -> Token
token char
  | char `elem` ['λ', '\\', 'ł'] = Sign
  | char == '.' = Dot
  | char `elem` ['(', '['] = LBrace
  | char `elem` [')', ']'] = RBrace
  | char `elem` ['a'..'z'] ++ ['A'..'Z'] = Letter char
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
makeTreeLambdas (Leaf val) = Leaf {val = val}
makeTreeLambdas (Branch stems) = Branch {stems = map makeTreeLambdas (makeLambda stems)}
makeTreeLambdas (Lambda symbol definition) =
  Lambda {symbol, definition = map makeTreeLambdas (makeLambda definition)}

parse :: [Token] -> Node
parse tokens = makeTreeLambdas $ makeTree tokens

-- Main -------------------------------------------------------------------------------------------
main :: IO()
main =
  let input = "(łx.ły.x)ab" in
  print (parse $ tokenise input)
