module HelpFunctions
where

import Control.Monad (void)
import Text.Parsec.String (Parser)
import Text.Parsec (char, many, oneOf)


-- ======= Hjälpfunktioner =============
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \t"

-- ======= Hjälpfunktioner =============
