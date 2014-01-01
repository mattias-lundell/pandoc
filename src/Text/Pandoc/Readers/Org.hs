{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances #-}
-- RelaxedPolyRec needed for inlinesBetween on GHC < 7
{-
  Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Mattias Lundell <mattias@lundell.com>
   Stability   : alpha
   Portability : portable

Conversion of org text to 'Pandoc' document.
-}
{-
TODO:
_ correctly handle tables within tables
_ parse templates?
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML ( htmlTag, isBlockTag, isCommentTag )
import Text.Pandoc.XML ( fromEntities )
import Text.Pandoc.Parsing hiding ( nested )
import Text.Pandoc.Walk ( walk )
import Text.Pandoc.Shared ( stripTrailingNewlines, safeRead, stringify, trim )
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import Data.List (intersperse, intercalate, isPrefixOf )
import Text.HTML.TagSoup
import Data.Sequence (viewl, ViewL(..), (<|))
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)

-- | Read mediawiki from an input string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
           -> String     -- ^ String to parse (assuming @'\n'@ line endings)
           -> Pandoc
readOrg opts s =
  case runParser parseOrg OrgState { oOptions         = opts
                                   , oMaxNestingLevel = 4
                                   , oNextLinkNumber  = 1
                                   , oCategoryLinks   = []
                                   , oHeaderMap       = M.empty
                                   , oIdentifierList  = []
                                   }
       "source" (s ++ "\n") of
    Left err'    -> error $ "\nError:\n" ++ show err'
    Right result -> result

data OrgState = OrgState { oOptions         :: ReaderOptions
                         , oMaxNestingLevel :: Int
                         , oNextLinkNumber  :: Int
                         , oCategoryLinks   :: [Inlines]
                         , oHeaderMap       :: M.Map Inlines String
                         , oIdentifierList  :: [String]
                     }

type OrgParser = Parser [Char] OrgState

instance HasReaderOptions OrgParser where
  askReaderOption f = (f . oOptions) `fmap` getState

instance HasHeaderMap OrgParser where
  getHeaderMap      = fmap oHeaderMap getState
  putHeaderMap hm   = updateState $ \st -> st { oHeaderMap = hm }

instance HasIdentifierList OrgParser where
  getIdentifierList   = fmap oIdentifierList getState
  putIdentifierList l = updateState $ \st -> st { oIdentifierList = l }

spaceChars :: [Char]
spaceChars = " \n\t"

parseOrg :: OrgParser Pandoc
parseOrg = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc $ bs

block :: OrgParser Blocks
block =  mempty <$ skipMany1 blankline
     <|> header
     <|> para

guardColumnOne :: OrgParser ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

header :: OrgParser Blocks
header = try $ do
  guardColumnOne
  level <- many1 (char '*') >>= return . length
  many1 (char ' ')
  title <- trimInlines . mconcat <$> manyTill inline newline
  attr <- registerHeader nullAttr title
  return $ B.headerWith attr level title

para :: OrgParser Blocks
para = do
  contents <- trimInlines . mconcat <$> many1 inline
  if F.all (==Space) contents
    then return mempty
    else return $ B.para contents

inline :: OrgParser Inlines
inline =  whitespace
      <|> str

str :: OrgParser Inlines
str = B.str <$> many1 (noneOf $ spaceChars)

whitespace :: OrgParser Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> endline)

endline :: OrgParser ()
endline = () <$ try (newline <*
                     notFollowedBy spaceChar <*
                     notFollowedBy newline)
