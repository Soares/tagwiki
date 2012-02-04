module Control.DateTime.Calculation
    ( Calculation(..)
    , when
    ) where
import Control.Applicative hiding ( (<|>) )
import Control.DateTime.Absolute
import Internal
import Text.Fragment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.TagWiki
import Text.Point
import Text.Pinpoint
import Text.Printf
import qualified Text.Symbols as Y

data Calculation
    = Exactly Expression
    | AbsRange Expression Expression
    | RelRange Expression Modification
    deriving Show

data Expression
    = Mod Term Modification
    | Simple Term
    deriving Show

-- TODO: test
-- {Kyte -51 @ /1/2}
-- {Kyte -51/1/2 @ /3/4}
data Term
    = Abs Absolute
    | PP Pinpoint
    deriving Show

instance Parseable Expression where
    parser = try (Mod <$> parser <*> parser)
         <|> (Simple <$> parser)
         <?> "a date calculation expression"

instance Parseable Term where
    parser = try (Abs <$> parser)
         <|> (PP <$> parser)
         <?> "a date calculation term"

instance Parseable Calculation where
    parser = try exact <|> absRange <|> relRange <?> "date calculation" where
        obrace = char '{' >> anyWhite >> return ()
        cbrace = anyWhite >> char '}' >> return ()
        comma = operator Y.comma
        exact = between obrace cbrace
            (Exactly <$> parser)
        absRange = between obrace cbrace
            (AbsRange <$> parser <*> (comma *> parser))
        relRange = between obrace cbrace
            (RelRange <$> parser <*> (comma *> parser))

instance Fragment Calculation where
    resolve (Exactly x) = show <$> moment x
    resolve (RelRange x y) = printf "%s → %s" <$>
        (show <$> moment x) <*> (show . endMoment y <$> moment x)
    resolve (AbsRange x y) = printf "%s → %s" <$>
        (show <$> moment x) <*> (show <$> moment y)

instance Momented Calculation where
    when End (RelRange _ Done) = pure Present
    when End (AbsRange _ y) = moment y
    when End (RelRange x y) = endMoment y <$> moment x
    when _ (AbsRange x _) = moment x
    when _ (RelRange x _) = moment x
    when _ (Exactly x) = moment x

moment :: (Internal i) => Expression -> i Absolute
moment (Simple (Abs a)) = pure a
moment (Simple (PP pp)) = pinpoint pp
moment (Mod t m) = flip apply m <$> moment (Simple t)

endMoment :: Modification -> Absolute -> Absolute
endMoment = flip apply
