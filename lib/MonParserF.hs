-- author : Eiko chan
{-# LANGUAGE RankNTypes #-}
module MonParserF where

import Control.Monad
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Either(lefts, rights)
import Data.Monoid (Monoid(..))

class (Functor f) => Parsable f where -- parsable functors would include [] and Tree
  node :: f a -> Maybe a
  childs :: f a -> [f a]

instance Parsable [] where
  node [] = Nothing
  node (x:xs) = Just x
  childs [] = []
  childs (x:xs) = [xs]

data ParserF b a = CreateParserF { runParserF :: forall f. Parsable f => f b -> [(a, f b)] }

data Tree a = EmptyTree | Node a [Tree a] deriving Show

instance Functor Tree where
  fmap fab EmptyTree = EmptyTree
  fmap fab (Node a subTrees) = Node (fab a) (fmap (fmap fab) subTrees)

instance Parsable Tree where
  node EmptyTree = Nothing
  node (Node x _) = Just x
  childs EmptyTree = []
  childs (Node x []) = [EmptyTree]
  childs (Node x y) = y

flatten :: Tree a -> [a]
flatten EmptyTree = []
flatten (Node a children) = a : concatMap flatten children
--newtype Parser a = CreateParser { run :: String -> [(a, String)] }

--mRunParserF :: Parsable f => (ParserF b a) -> f b -> Maybe a
mRunParserF parser = (fmap fst) . listToMaybe . (runParserF parser)

instance Functor (ParserF b) where
  fmap f p = CreateParserF (\str -> [(f v, res) | (v, res) <- runParserF p str])

-- The functor Parser is a monad, with unit (natural transform):
unit :: a -> ParserF b a 
unit v = CreateParserF $ \x -> [(v, x)]
-- the unit (eta), returns a Parser with a given value without doing anything

-- and composition mu: Parser (Parser a) \to Parser a (another natural transform).
-- then the binding operator >>= is defined to be mu . Parser, i.e. 
-- p1 >>= f = mu (fmap_Parser f) p1
--          = mu (Parser a -> Parser Parser b) p1 
--          = Parser b
-- In haskell, in order to make a monad, first of all you need to make it a functor.
-- then you must make it applicative. This is just a requirement in haskell, it does not mean that any monad induces applicative structure.
instance Applicative (ParserF b) where
  pure = unit
  pf <*> p = CreateParserF (\str -> [(f v, res2) | (f, res) <- runParserF pf str, (v, res2) <- runParserF p res] )

instance Monad (ParserF b) where
  return = pure
  p1 >>= f = CreateParserF $ \str -> concat [runParserF (f v) res |(v, res)<- runParserF p1 str]

cup :: ParserF a b -> ParserF a b -> ParserF a b
(CreateParserF p1) `cup` (CreateParserF p2) = CreateParserF $ \str -> (p1 str) ++ (p2 str)

instance Semigroup (ParserF b a) where
  (<>) = cup

instance Monoid (ParserF b a) where
  mempty = zero

zero :: ParserF a b
zero = CreateParserF (\inp -> [])
-- Always fail to parse anything

restriction :: Bool -> ParserF a ()
restriction True = unit ()
restriction False = zero

item :: ParserF b b
item = CreateParserF fitem
  where
    fitem inp = case node inp of
      Nothing -> []
      Just xb -> [(xb, child) | child <- childs inp]
-- Always success (as long as input is nonempty), parse the first char.

satisfy :: (b -> Bool) -> ParserF b b
satisfy f = do
  xb <- item
  if f xb then return xb else zero

just c = satisfy ( == c)

itemIn list = satisfy (`elem` list)
itemsIn list = many $ satisfy (`elem` list)
items0In list = many0 $ satisfy (`elem` list)

digit = itemIn ['0'..'9'] 

lower = itemIn ['a'..'z'] 

upper = itemIn ['A'..'Z'] 

letter = lower <> upper

space = just ' '

string :: String -> ParserF Char String
string "" = unit ""
string (x:xs) = do { just x; string xs; return (x:xs) }

tryMaybe :: ParserF b a -> ParserF b (Maybe a)
tryMaybe (CreateParserF p) = CreateParserF $ \str -> let r = p str in
  case r of
    [] -> [(Nothing, str)]
    _ -> [ (Just x, xs) | (x, xs) <- r]

try0 :: ParserF b [a] -> ParserF b [a]
try0 (CreateParserF p) = CreateParserF $ \str -> let r = p str in
  case r of
    [] -> [([], str)]
    _ -> r 

tryBool :: ParserF b a -> ParserF b Bool
tryBool parser = do
  m <- tryMaybe parser
  case m of
    Nothing -> return False
    Just _ -> return True

end :: ParserF b ()
end = do
  hasItem <- tryBool item
  case hasItem of
    True -> zero
    False -> return ()

many :: ParserF b a -> ParserF b [a]
many p = do
  r <- p
  rs <- try0 $ many p
  return (r:rs)

many0 p = try0 $ many p

spaceOrEnter = mconcat $ fmap string ["\r\n", "\r", "\n", " "]
commandSeparator = many spaceOrEnter

commandSeparator2 = just '~' `eitherParse` (just ',' `eitherParse` (just '，' `eitherParse` many (mconcat $ fmap string ["\r\n", "\r", "\n", " "])))

spaces0 = many0 space
spaces = many space

identifier = many (letter <> digit)

headCommand cmd = do
  spaces0
  just ':' <> just '：'
  str <- string cmd
  return str        

eitherParse :: (Show a) => ParserF t a -> ParserF t b -> ParserF t (Either a b)
eitherParse f g = do
  rf <- tryMaybe f
  case (rf) of
    Nothing -> do
      rg <- g
      return $ Right rg
    Just x -> return $ Left x

htmlCode :: String -> Char -> ParserF Char Char
htmlCode code char = do
  string code
  return char

htmlCodes = mconcat
  [htmlCode "&amp;" '&'
  ,htmlCode "&#91;" '['
  ,htmlCode "&#93;" ']'
  ,htmlCode "&#44;" ','
  ]

htmlDecode :: ParserF Char String
htmlDecode = many $ htmlCodes <> item

