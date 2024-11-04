module MonParser where
import Data.Maybe (listToMaybe)

newtype Parser b a = CreateParser { runParser :: [b] -> [(a, [b])] }
mRunParser parser = (fmap fst) . listToMaybe . (runParser parser)

instance Functor (Parser b) where
  fmap f p = CreateParser $ \bs -> [(f v, res) | (v, res) <- runParser p bs]

instance Applicative (Parser b) where
  pure a = CreateParser $ \bs -> [(a, bs)]
  pf <*> p = CreateParser $ \bs -> [(f v, res2) 
    | (f, res) <- runParser pf bs
    , (v, res2) <- runParser p res
    ]

unit :: a -> Parser b a
unit = pure

cup :: Parser b a -> Parser b a -> Parser b a
(CreateParser p1) `cup` (CreateParser p2) = CreateParser $ \bs -> (p1 bs) ++ (p2 bs)

instance Monad (Parser b) where
  return = pure
  p1 >>= f = CreateParser $ \bs -> concat [runParser (f v) res | (v, res) <- runParser p1 bs]

instance Semigroup (Parser b a) where
  (<>) = cup

zero :: Parser b a
zero = CreateParser $ \_ -> []

instance Monoid (Parser b a) where
  mempty = zero

--instance Alternative (Parser b) where
--  empty = zero
--  (<|>) = cup

item :: Parser b b
item = CreateParser fitem
  where
    fitem [] = []
    fitem (b:bs) = [(b, bs)]

satisfy :: (b -> Bool) -> Parser b b
satisfy f = do
  b <- item
  if f b then return b else zero

just :: Eq b => b -> Parser b b
just c = satisfy ( == c)

string :: Eq b => [b] -> Parser b [b]
string [] = unit []
string (x:xs) = do
  c <- just x
  cs <- string xs
  return (c:cs)

itemIn :: Eq b => [b] -> Parser b b
itemIn bs = item >>= \b -> if b `elem` bs then return b else zero

itemNotIn :: Eq b => [b] -> Parser b b
itemNotIn bs = item >>= \b -> if b `elem` bs then zero else return b

tryMaybe :: Parser b a -> Parser b (Maybe a)
tryMaybe (CreateParser p) = CreateParser $ \str -> let r = p str in
  case r of
    [] -> [(Nothing, str)]
    _ -> [ (Just x, xs) | (x, xs) <- r]

try0 :: Parser b [a] -> Parser b [a]
try0 (CreateParser p) = CreateParser $ \str -> let r = p str in
  case r of
    [] -> [([], str)]
    _ -> r 

tryBool :: Parser b a -> Parser b Bool
tryBool parser = do
  m <- tryMaybe parser
  case m of
    Nothing -> return False
    Just _ -> return True

end :: Parser b ()
end = do
  hasItem <- tryBool item
  case hasItem of
    True -> zero
    alse -> return ()

many :: Parser b a -> Parser b [a]
many p = do
  r <- p
  rs <- try0 $ many p
  return (r:rs)

many0 p = try0 $ many p
