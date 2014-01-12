{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS -fwarn-unused-imports #-}

-- | Perl-style pattern matching.  Examples:
--
-- > let match = "^(.*)_(\\d).txt$"
-- > let source = "file_1.txt"
-- > source =~+ match !! 1         => ("file",(0,4))
-- > source =~- match !! 2         => "1"
--
-- > let q :: LBS = "wif kwof ..wif,, wif,  8fwif"
-- > let p :: SBS = "\\Sw.f"
-- > let x :: LBS = replaceRegexAll q p (\ ([(a,_)] :: [(LBS, (MatchOffset, MatchLength))]) -> Just $ "@" <> a <> "@")
-- > x         => "wif @kwof@ .@.wif@,, wif,  8@fwif@"
-- > q =~++ p  => [[("kwof",(4,4))],[(".wif",(10,4))],[("fwif",(24,4))]]
--
-- (For simplicity, source data is always LBS and pattern is always
-- SBS.  Checkout package string-conversions.)
module Text.Regex.Easy
  ( module Text.Regex.PCRE
  , (=~+)
  , (=~++)
  , replaceRegex
  , replaceRegexAll
  , (=~-)
  , (=~#)
  )
where

import Data.Array as AR
import Data.Function
import Data.List as List
import Data.Monoid
import Data.String.Conversions
import Prelude hiding ((++))
import Text.Regex.PCRE

import qualified Data.ByteString.Lazy as LBS


(=~+) :: forall source match .
         (RegexMaker Regex CompOption ExecOption match,
          RegexContext Regex source
               (AllTextSubmatches (Array Int) (source, (MatchOffset, MatchLength))),
          source ~ LBS,
          match ~ SBS)
       => source -> match -> [(source, (MatchOffset, MatchLength))]
(=~+) source match = elems (getAllTextSubmatches (source =~ match) :: MatchText source)


(=~++) :: forall source match .
         (RegexMaker Regex CompOption ExecOption match,
          RegexContext Regex source
               (AllTextSubmatches (Array Int) (source, (MatchOffset, MatchLength))),
          source ~ LBS,
          match ~ SBS)
       => source -> match -> [[(source, (MatchOffset, MatchLength))]]
(=~++) source match = case source =~+ match of
                        [] -> []
                        x@((_, (holeStart, holeEnd)):_) -> x : map (shift (holeStart + holeEnd))
                                                                   (LBS.drop (fromIntegral $ holeStart + holeEnd) source =~++ match)
  where
    shift :: Int -> [(source, (MatchOffset, MatchLength))] -> [(source, (MatchOffset, MatchLength))]
    shift o' = map (\ (s, (o, l)) -> (s, (o + o', l)))


-- | replace first match with result of a function of the match.
-- source must be LBS.  (FIXME: at least use string-conversions to relax
-- this constraint?)
replaceRegex :: forall source match .
         (RegexMaker Regex CompOption ExecOption match,
          RegexContext Regex source
               (AllTextSubmatches (Array Int) (source, (MatchOffset, MatchLength))),
          source ~ LBS,
          match ~ SBS)
       => source -> match -> ([(source, (MatchOffset, MatchLength))] -> Maybe source) -> source
replaceRegex source match trans = case source =~+ match of
      m@((_, (offset, length)):_) -> let before = LBS.take (fromIntegral offset) source
                                         after = LBS.drop (fromIntegral $ offset + length) source
                                     in case trans m of
                                          Just m' -> before <> m' <> after
                                          Nothing -> source


-- | replace first match with result of a function of the match.
-- source must be LBS.  (FIXME: at least use string-conversions to relax
-- this constraint?)
replaceRegexAll :: forall source match .
         (RegexMaker Regex CompOption ExecOption match,
          RegexContext Regex source
               (AllTextSubmatches (Array Int) (source, (MatchOffset, MatchLength))),
          source ~ LBS,
          match ~ SBS)
       => source -> match -> ([(source, (MatchOffset, MatchLength))] -> Maybe source) -> source
replaceRegexAll source match trans = case source =~+ match of
      [] -> source
      m@((_, (offset, length)):_) -> case trans m of
                                        Just m' -> let before = LBS.take (fromIntegral offset) source
                                                       after  = LBS.drop (fromIntegral $ offset + length) source
                                                   in before <> m' <> replaceRegexAll after match trans
                                        Nothing -> let before = LBS.take (fromIntegral $ offset + length) source
                                                       after  = LBS.drop (fromIntegral $ offset + length) source
                                                   in before <> replaceRegexAll after match trans


-- | Convenience function for '(=~+)' that chops rarely needed
-- offsets and lengths off the result.
(=~-) :: (RegexMaker Regex CompOption ExecOption match,
          RegexLike Regex source,
          source ~ LBS,
          match ~ SBS)
       => source -> match -> [source]
(=~-) source match = map fst $ source =~+ match


-- | Convenience function for '(=~+)' with match result 'Bool'.
(=~#) :: (RegexMaker Regex CompOption ExecOption match,
          RegexLike Regex source,
          source ~ LBS,
          match ~ SBS)
       => source -> match -> Bool
(=~#) source match = not . null $ source =~+ match
