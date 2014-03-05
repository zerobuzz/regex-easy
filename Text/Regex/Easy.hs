{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE NoImplicitPrelude                        #-}
{-# LANGUAGE OverloadedStrings                        #-}
{-# LANGUAGE ScopedTypeVariables                      #-}

{-# OPTIONS -fwarn-unused-imports #-}

-- | This module provides perl-style pattern matching.  It is intended
-- for use with minimal Haskell knowledge, so it moves away from the
-- complex regex-* type signatures for the sake of clarity, and always
-- uses the same string types for source text and patterns.  See
-- 'tests' in source code for a few examples.
module Text.Regex.Easy
  ( module Text.Regex.PCRE
  , Match, Source
  , (=~+)
  , (=~-)
  , (=~#)
  , (=~++)
  , replaceRegex
  , replaceRegexAll
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


-- | Rudimentary tests.  Read the source as a form of documentation.
tests :: Bool
tests = and $
    (("file_1.txt" =~+ "^(.*)_(\\d).txt$") ==
     [ ( "file_1.txt" , ( 0 , 10 ) )
     , ( "file" , ( 0 , 4 ) )
     , ( "1" , ( 5 , 1 ) )
     ]) :

    (("file_1.txt" =~- "^(.*)_(\\d).txt$") ==
     ["file_1.txt", "file", "1"]) :

    ("file_1.txt" =~# "^(.*)_(\\d).txt$") :

    (let q :: LBS = "wif kwof ..wif,, wif,  8fwif"
         p :: SBS = "\\Sw.f"
     in ((q =~+ p) ==
         [ ( "kwof" , (  4 , 4 ) ) ]) &&
        ((q =~++ p) ==
         [ [ ( "kwof" , (  4 , 4 ) ) ]
         , [ ( ".wif" , ( 10 , 4 ) ) ]
         , [ ( "fwif" , ( 24 , 4 ) ) ]
         ])) :

    (let q :: LBS = "wif kwof ..wif,, wif,  8fwif"
         p :: SBS = "\\Sw.f"
         f ([(a,_)] :: [(LBS, (MatchOffset, MatchLength))]) = Just $ "@" <> a <> "@"
     in (replaceRegex q p f == "wif @kwof@ ..wif,, wif,  8fwif") &&
        (replaceRegexAll q p f == "wif @kwof@ .@.wif@,, wif,  8@fwif@")) :

    []


type Match = SBS
type Source = LBS


-- | Convenience wrapper around '(=~)', that trades flexibility off
-- for compactness.
(=~+) :: Source -> Match -> [(Source, (MatchOffset, MatchLength))]
(=~+) source match = elems (getAllTextSubmatches (source =~ match) :: MatchText Source)


-- | Convenience wrapper for '(=~+)' that chops rarely needed offsets
-- and lengths off the result.
(=~-) :: Source -> Match -> [Source]
(=~-) source match = map fst $ source =~+ match


-- | Convenience function for '(=~+)' with match result 'Bool'.
(=~#) :: Source -> Match -> Bool
(=~#) source match = not . null $ source =~+ match


-- | Like '(=~+)', but find all matches, not just the first one.
(=~++) :: Source -> Match -> [[(Source, (MatchOffset, MatchLength))]]
(=~++) source match = case source =~+ match of
                        [] -> []
                        x@((_, (holeStart, holeEnd)):_) -> x : map (shift (holeStart + holeEnd))
                                                                   (LBS.drop (fromIntegral $ holeStart + holeEnd) source =~++ match)
  where
    shift :: Int -> [(Source, (MatchOffset, MatchLength))] -> [(Source, (MatchOffset, MatchLength))]
    shift o' = map (\ (s, (o, l)) -> (s, (o + o', l)))


-- | Replace first match with result of a function of the match.
replaceRegex :: Source -> Match -> ([(Source, (MatchOffset, MatchLength))] -> Maybe Source) -> Source
replaceRegex source match trans = case source =~+ match of
      m@((_, (offset, length)):_) -> let before = LBS.take (fromIntegral offset) source
                                         after = LBS.drop (fromIntegral $ offset + length) source
                                     in case trans m of
                                          Just m' -> before <> m' <> after
                                          Nothing -> source


-- | Replace all matches with result of a function of the match.
replaceRegexAll :: Source -> Match -> ([(Source, (MatchOffset, MatchLength))] -> Maybe Source) -> Source
replaceRegexAll source match trans = case source =~+ match of
      [] -> source
      m@((_, (offset, length)):_) -> case trans m of
                                        Just m' -> let before = LBS.take (fromIntegral offset) source
                                                       after  = LBS.drop (fromIntegral $ offset + length) source
                                                   in before <> m' <> replaceRegexAll after match trans
                                        Nothing -> let before = LBS.take (fromIntegral $ offset + length) source
                                                       after  = LBS.drop (fromIntegral $ offset + length) source
                                                   in before <> replaceRegexAll after match trans
