{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str file = anagramsWithDict . toDictSet . lines <$> readFile file
  where
    noCaseLst      = map NoCaseString
    noCaseVariants = noCaseLst $ permutations str
    toDictSet      = S.fromList . hlist . noCaseLst
    anagramsWithDict dictSet = map ncString 
                               $ filter (`S.member` dictSet) noCaseVariants

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString