module Lib where
    ( someFunc
    ) where

import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Set as Set

import Text.Regex.TDFA

import Control.Applicative
import Data.List (foldl')

newtype Parser t = Parser { doParse :: String -> Maybe (t, String) }

instance Functor Parser where
  fmap f parser = Parser { doParse = \p -> do
                             (res, restOfString) <- doParse parser p
                             return (f res, restOfString) }

instance Applicative Parser where
  pure x = Parser { doParse = \p -> Just (x, p) }
  pf <*> p = Parser { doParse = \input -> do
                        (f, rs) <- doParse pf input
                        (v, rss) <- doParse p rs
                        return (f v, rss) }

instance Monad Parser where
  p >>= f = Parser { doParse = \input -> do
                       (v, rs) <- doParse p input
                       doParse (f v) rs }

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((:) <$> p <*> zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

endOfInput :: Parser ()
endOfInput = Parser p where
  p [] = Just ((), "")
  p _ = Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f where
  f [] = Nothing
  f (x:xs)
    | p x = Just (x, xs)
    | otherwise = Nothing


data Rule = SingleChar Char | SubRules [[Int]] deriving Show

createRuleMap :: String -> IntMap.IntMap Rule
createRuleMap = IntMap.fromList . map f . lines where
  f :: String -> (Int, Rule)
  f s = (ruleNum, rule) where
    ruleNum = read $ takeWhile isDigit s
    isSingleChar :: Bool
    isSingleChar = s =~ "\"[a-z]\""
    singleCharIndex = case List.elemIndex '"' s of (Just i) -> i + 1
    isMultipleLists = any (== '|') s
    pipeCharIndex = case List.elemIndex '|' s of (Just i) -> i
    singleListMatches :: [String]
    singleListMatches = getAllTextMatches (s =~ "[0-9]+")
    firstListMatches :: [String]
    firstListMatches = getAllTextMatches ((take pipeCharIndex s) =~ "[0-9]+")
    secondListMatches :: [String]
    secondListMatches = getAllTextMatches ((drop (pipeCharIndex + 1) s) =~ "[0-9]+")
    singleListNums = (drop 1 . map read) singleListMatches
    firstListNums = (drop 1 . map read) firstListMatches
    secondListNums = map read secondListMatches
    rule = if isSingleChar
      then SingleChar (s !! singleCharIndex)
      else if not isMultipleLists
      then SubRules [singleListNums]
      else SubRules [firstListNums, secondListNums]

-- Part 1

evaluateRule :: Int -> IntMap.IntMap Rule -> [String]
evaluateRule n ruleMap = case (IntMap.!) ruleMap n of
  SingleChar char -> [[char]]
  SubRules subRules -> do
    subRule <- subRules
    let concatRuleParts :: [[String]] -> [String]
        concatRuleParts [] = [""]
        concatRuleParts (x:xs) = do
          x' <- x
          map (x' ++) (concatRuleParts xs)
    concatRuleParts $ map (flip evaluateRule ruleMap) subRule

-- Part 2
-- TODO-thinking about it
-- 0: 8 11
-- 8: 42 | 42 8
-- 11: 42 31 | 42 11 31

-- oneOrMore (match rule 42) followed by possibly nested brackets of rule 42 and rule 31

startsWith :: String -> Parser ()
startsWith [] = pure ()
startsWith (c:cs) = do
  satisfy (== c)
  startsWith cs

nTimes :: Int -> Parser a -> Parser [a]
nTimes 1 p = (:[]) <$> p
nTimes n p = (:) <$> p <*> nTimes (n-1) p

failingParser :: Parser a
failingParser = Parser $ const Nothing

matchRuleParser :: Int -> IntMap.IntMap Rule -> Parser ()
matchRuleParser n ruleMap = do
  let possibleStartingStrings = evaluateRule n ruleMap
  foldr (\s p -> p <|> startsWith s) failingParser possibleStartingStrings

nTimesRule11Parser :: Int -> IntMap.IntMap Rule -> Parser ()
nTimesRule11Parser 1 ruleMap = matchRuleParser 42 ruleMap >> matchRuleParser 31 ruleMap
nTimesRule11Parser n ruleMap = do
  matchRuleParser 42 ruleMap
  nTimesRule11Parser (n-1) ruleMap
  matchRuleParser 31 ruleMap

diagonalFrom :: Int -> [(Int, Int)]
diagonalFrom 1 = [(1, 1)]
diagonalFrom n = go n where
  go 1 = [(1, n)]
  go m = (m, n - m + 1):(go (m-1))

diagonalPairs :: [(Int, Int)]
diagonalPairs = concat $ map diagonalFrom [1..]
-- diagonalPairs = [(1, 1), (1, 2), (2, 1), (3, 1), (2, 2), (1, 3), (1, 4)]

loopRuleParser :: IntMap.IntMap Rule -> Parser ()
loopRuleParser ruleMap = do
  oneOrMore (matchRuleParser 42 ruleMap)
  let rule11Parser :: Parser ()
      rule11Parser = do
        (matchRuleParser 42 ruleMap >> matchRuleParser 31 ruleMap) <|> (matchRuleParser 42 ruleMap >> rule11Parser >> matchRuleParser 31 ruleMap)
  rule11Parser
  endOfInput

blah :: [(Int, Int)] -> IntMap.IntMap Rule -> Parser ()
blah [(n, m)] ruleMap = nTimes n (matchRuleParser 42 ruleMap) >> nTimesRule11Parser m ruleMap >> endOfInput
blah ((n, m):ps) ruleMap = do
  (nTimes n (matchRuleParser 42 ruleMap) >> nTimesRule11Parser m ruleMap >> endOfInput) <|> blah ps ruleMap

-- Inputs

rulesInputExample :: String
rulesInputExample = "0: 4 1 5\n\
\1: 2 3 | 3 2\n\
\2: 4 4 | 5 5\n\
\3: 4 5 | 5 4\n\
\4: \"a\"\n\
\5: \"b\""

messagesInputExample :: String
messagesInputExample = "ababbb\n\
\bababa\n\
\abbbab\n\
\aaabbb\n\
\aaaabbb"

rulesInputExample2 :: String
rulesInputExample2 = "42: 9 14 | 10 1\n\
\9: 14 27 | 1 26\n\
\10: 23 14 | 28 1\n\
\1: \"a\"\n\
\11: 42 31\n\
\5: 1 14 | 15 1\n\
\19: 14 1 | 14 14\n\
\12: 24 14 | 19 1\n\
\16: 15 1 | 14 14\n\
\31: 14 17 | 1 13\n\
\6: 14 14 | 1 14\n\
\2: 1 24 | 14 4\n\
\0: 8 11\n\
\13: 14 3 | 1 12\n\
\15: 1 | 14\n\
\17: 14 2 | 1 7\n\
\23: 25 1 | 22 14\n\
\28: 16 1\n\
\4: 1 1\n\
\20: 14 14 | 1 15\n\
\3: 5 14 | 16 1\n\
\27: 1 6 | 14 18\n\
\14: \"b\"\n\
\21: 14 1 | 1 14\n\
\25: 1 1 | 1 14\n\
\22: 14 14\n\
\8: 42\n\
\26: 14 22 | 1 20\n\
\18: 15 15\n\
\7: 14 5 | 1 21\n\
\24: 14 1"

messagesInputExample2 :: String
messagesInputExample2 = "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
\bbabbbbaabaabba\n\
\babbbbaabbbbbabbbbbbaabaaabaaa\n\
\aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
\bbbbbbbaaaabbbbaaabbabaaa\n\
\bbbababbbbaaaaaaaabbababaaababaabab\n\
\ababaaaaaabaaab\n\
\ababaaaaabbbaba\n\
\baabbaaaabbaaaababbaababb\n\
\abbbbabbbbaaaababbbbbbaaaababb\n\
\aaaaabbaabaaaaababaa\n\
\aaaabbaaaabbaaa\n\
\aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
\babaaabbbaaabaababbaabababaaab\n\
\aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

rulesInput :: String
rulesInput = "94: 118 64 | 22 34\n\
\21: 16 64 | 49 34\n\
\70: 58 34 | 106 64\n\
\100: 58 64 | 56 34\n\
\24: 29 64 | 128 34\n\
\63: 107 64 | 106 34\n\
\10: 64 64 | 34 64\n\
\1: 40 34 | 58 64\n\
\119: 56 34 | 103 64\n\
\131: 56 64 | 9 34\n\
\34: \"b\"\n\
\65: 64 58 | 34 107\n\
\0: 8 11\n\
\68: 64 43 | 34 50\n\
\48: 107 64 | 96 34\n\
\38: 80 64 | 74 34\n\
\128: 73 64 | 19 34\n\
\6: 62 34 | 18 64\n\
\95: 111 34 | 61 64\n\
\74: 5 64 | 65 34\n\
\82: 91 64 | 79 34\n\
\37: 34 107 | 64 106\n\
\35: 122 34 | 23 64\n\
\106: 34 34 | 30 64\n\
\118: 9 34 | 114 64\n\
\4: 107 34 | 106 64\n\
\102: 96 64 | 107 34\n\
\31: 75 34 | 108 64\n\
\99: 34 10 | 64 114\n\
\90: 64 34 | 34 34\n\
\112: 64 10 | 34 3\n\
\15: 114 64 | 88 34\n\
\11: 42 31\n\
\49: 32 34 | 120 64\n\
\83: 56 64 | 96 34\n\
\29: 34 78 | 64 131\n\
\84: 107 64 | 56 34\n\
\25: 40 64 | 107 34\n\
\33: 26 34 | 24 64\n\
\122: 114 34 | 103 64\n\
\69: 56 34 | 96 64\n\
\5: 103 34 | 9 64\n\
\17: 59 34 | 70 64\n\
\123: 40 64 | 90 34\n\
\114: 34 64\n\
\121: 114 34 | 3 64\n\
\32: 34 9 | 64 114\n\
\127: 4 64 | 84 34\n\
\125: 71 34 | 121 64\n\
\116: 64 1 | 34 118\n\
\105: 34 73 | 64 37\n\
\56: 64 34\n\
\110: 105 64 | 54 34\n\
\79: 64 9 | 34 107\n\
\42: 6 64 | 68 34\n\
\53: 2 64 | 17 34\n\
\61: 64 114 | 34 88\n\
\8: 42\n\
\19: 103 34 | 40 64\n\
\9: 30 34 | 34 64\n\
\107: 34 64 | 64 30\n\
\115: 56 64 | 130 34\n\
\77: 56 34 | 9 64\n\
\46: 27 64 | 67 34\n\
\59: 88 34\n\
\22: 34 3 | 64 106\n\
\76: 69 34 | 28 64\n\
\129: 36 64 | 113 34\n\
\124: 44 64 | 39 34\n\
\103: 64 64\n\
\91: 34 40\n\
\67: 88 64 | 114 34\n\
\130: 34 64 | 64 34\n\
\12: 76 64 | 125 34\n\
\81: 64 96 | 34 106\n\
\13: 64 51 | 34 41\n\
\30: 64 | 34\n\
\20: 34 106 | 64 107\n\
\43: 89 34 | 129 64\n\
\71: 34 114 | 64 3\n\
\66: 48 64 | 81 34\n\
\3: 64 64 | 64 34\n\
\87: 64 52 | 34 14\n\
\96: 30 34 | 64 64\n\
\62: 38 34 | 12 64\n\
\88: 34 34\n\
\75: 126 34 | 85 64\n\
\55: 34 109 | 64 67\n\
\45: 56 64\n\
\104: 64 112 | 34 5\n\
\39: 64 77 | 34 100\n\
\86: 34 114 | 64 88\n\
\26: 64 47 | 34 94\n\
\101: 34 127 | 64 35\n\
\41: 81 34 | 83 64\n\
\40: 30 30\n\
\2: 115 64 | 71 34\n\
\64: \"a\"\n\
\108: 64 33 | 34 92\n\
\126: 64 7 | 34 13\n\
\117: 45 64 | 99 34\n\
\54: 15 34 | 63 64\n\
\51: 123 34 | 98 64\n\
\14: 34 88 | 64 56\n\
\98: 58 64 | 114 34\n\
\78: 34 9 | 64 10\n\
\7: 34 60 | 64 104\n\
\97: 46 64 | 82 34\n\
\57: 87 34 | 117 64\n\
\60: 34 120 | 64 4\n\
\58: 64 64 | 34 34\n\
\27: 64 40 | 34 56\n\
\89: 95 64 | 116 34\n\
\109: 34 90\n\
\44: 64 25 | 34 72\n\
\50: 64 124 | 34 101\n\
\47: 98 64 | 20 34\n\
\16: 34 70 | 64 69\n\
\85: 64 110 | 34 57\n\
\36: 119 34 | 91 64\n\
\28: 56 64 | 58 34\n\
\80: 91 64 | 72 34\n\
\111: 64 56 | 34 103\n\
\93: 66 64 | 55 34\n\
\92: 53 64 | 93 34\n\
\23: 64 3 | 34 40\n\
\52: 64 88 | 34 10\n\
\120: 96 64 | 114 34\n\
\73: 40 34\n\
\72: 34 106 | 64 88\n\
\18: 97 34 | 21 64\n\
\113: 102 34 | 86 64"

messagesInput :: String
messagesInput = "aaabaabbabaaaabbaabaababbaaaabbb\n\
\bbabaaabbbbababbbaabbaba\n\
\baaaabbaaaaabbabbabaaaab\n\
\aabaaaaabaabaabbbababbba\n\
\abbbaababbaaaababaaabababbaaabababbbabbaaaaabbbabbaabbab\n\
\abbbbbbbbbaaababaaababaa\n\
\ababbbbbbbabbbabaaaaaaaaaaaabbbbbbaaabbaabbbabab\n\
\abaabbbababbaaababbbabab\n\
\aaaabababbaabaaaabaabbaa\n\
\bbaaaaabbbababbababbbbbaabbbabab\n\
\bbabaababbabbbaaabaaabba\n\
\bbaaabbabaaabbaabbbaaababaaababbbbaabaaababababb\n\
\bbbbaabbabaababababaabbbbbbbabbabbbaaabbaabaabbaabbbbbabababbbbbbabbbaab\n\
\aabbbbaaaabaaaaaaabbabaa\n\
\aaaabababababbabbabababb\n\
\babbabaababbbbabbbbababa\n\
\aabbbbbbaababbbbbabbbbbaababbaba\n\
\baaabbabaaaabaaaabbbabaa\n\
\bbaaaaabbbabbbaabababbba\n\
\abbaabbbbaabbabbaabbabaa\n\
\bbbbaaaabbababbaabbabbabbaabbabbbbbbababbabbbbbb\n\
\ababaabababaaabbaaaaaaaaaaaababa\n\
\abbaaaaaaaabbbbaabaabbaa\n\
\bbabababababababaaababbb\n\
\bbabbbaaababbbababbaabaaaaaaaabb\n\
\aaabbbbaaaaabaaabbbaaaabbaabaaabbbbabaaa\n\
\bbbabbaabbabbbabbbbabbba\n\
\abbbbbbaababbaaabaaaababaaaabaabbbbababaabbbaaaaabaaabbb\n\
\abbaaaabbbabababbaabaabbbaabbbaabbbbabaa\n\
\aaaabbbbabbaabaabbbabbababbaaaaabaaababb\n\
\bbaaaabbbbababbbbbbaabbababbbaaaaabbabbaabbabaaabbaababa\n\
\abaabaaaabbbbbbaababbbaaaabbabbbbbababaabbabaabbabababbabbbbaaba\n\
\bababbababbbaabbaabbbabb\n\
\aaabbababbaabaaabaabbaaa\n\
\babaaabbbaaaababbabbaaababbababb\n\
\babbaaaabbbabaaaabaabbbbaaabaabb\n\
\aabaaababaabaaaabaaaabbaaababbabaabababababbabbb\n\
\bababababababbbbbabbbbbb\n\
\baabbbaaaaabbbbaabbabbababbababa\n\
\abbbbbaaaaabbabbbababbbbbaaabbabbabaabbb\n\
\bbbabbbbaabbbababbabbaaabbbbaaabbbbbaaaa\n\
\ababbbabbbaaaaaaaaabbabbbababbabbaaabaababbbbabbaababbaabbbabaaaabbbabbb\n\
\aaabaababababababbbaabaa\n\
\ababbabbbabaabbabbaaaaabaaaabaabbaaabababbbbaabbbbaaababababbaaa\n\
\aaaabaabbbaaaaaabbaabbab\n\
\bbbaaaabaabbbababaaaaabb\n\
\aababbabbaabbaabbabbabba\n\
\aabbaaaaaabbbababbaaabbb\n\
\abaaaaabbaabaabbbaababbaababbbbbbbbbbbba\n\
\abbbbbaaaaabbbbbabbbbbaabbaaabaaaaaaaabb\n\
\bbabbbbaaaabbbbbaababbba\n\
\abbbaababaabababbbbbabbabaaababbbbbbbbaa\n\
\aaaaabbbaaabbabaabbabbabbbababababbbaaaaaaaaaababbbbaaba\n\
\abbbabbabaababaaabbbabababbaaaba\n\
\baaabbbbabbbbabbbbabbbbbaababaab\n\
\aaaaaaaaababbbabbaabbaabbbabbbbb\n\
\bbbaaabbbbbabbaaaaaabaaaaaaaabaa\n\
\bbababbbbaaaabbbabaabaaabbbaabababaaaaabbbaababbbaabbaaaaaaabaab\n\
\abbbaabbbbbbaaaaaabbabaa\n\
\aaabaabaaabbbbabaabbbabaaabaaabbbbbbbbbaaaabaaab\n\
\bbbbabaaaabaabababababbb\n\
\aabbbbabbbbabbaaababaabb\n\
\bbabbbaababaabbabbbaabbababbbbaa\n\
\abbabaababbbabaaaaaaabba\n\
\abaabbbabbbababbabbaababbbabaaaa\n\
\aababbbaabbaababbbbabaaabbbaabbbaababbaa\n\
\bbbaababaaabbbbaaabbbabababbabbbabaaaabb\n\
\abbbbbaaaaaaaaabaaabababbabbaaaabbbbbbbbabaaabba\n\
\ababbbaabbabaaababbbaaabaaabaabaabaabaab\n\
\bbabbaaaabbbabbabaaababa\n\
\babaaabbaabbaaaabbabbabb\n\
\abaaaabaababbbaabbbbbbbb\n\
\bbbaaaabababbaaabaaaaaba\n\
\aabbbaabbaaababbbbbbbaaaabbaaaba\n\
\bbaabaaaaababaaaaaaabbaabbaababbabbaabaabaaaaaaabaaababbababbaaaaaaababbbaabaaababbabbaa\n\
\babaaabbababbbabbaaaabbb\n\
\aaabbabaaabbbabaababbbba\n\
\bbaaaaaaaaaababaabbbaaaa\n\
\baabaaaabbababbaabbbbbbbabababaa\n\
\aaabbabbbabbabaaabaabbbaaaaababa\n\
\aabaaababbbbaaaaaaaabbaa\n\
\aaaaaaaababbbbaabbaabbbabaaabbaababbbabababbaaba\n\
\bbbaabbaaabbbbaaaababaaa\n\
\baabaaabaababbabaaaabaaa\n\
\bbbaabbabbabbaabaabbaabbabaabbbabbbabbaabbaabbbbbaaaabbb\n\
\abbaaaabbaaaabbabaababbb\n\
\bbbbbaaaabaababaaabaabaaaaaaabbbbbbaabbbaaabbbaa\n\
\babbbbbabbbababbaabbbbbbaaaabbbbbbaaabaa\n\
\aabaabaabaabbbaaaabbaaab\n\
\abbbabbabbbaaabbaaaaaaabbbbbbbbaaaabababaaababaababbbbbbbbaababa\n\
\baababbabbaaababbbbaaabbababbbab\n\
\bbaaaababbbbbaaaabaababaaababaaaababababbbbbbabbaaabbbabbaaaabbabbbbabab\n\
\bbbaaababbbbababaaababba\n\
\aabaaabbbaabbaabbbbbabbaaaaabaaaabaaabba\n\
\abbbbbaababbbaabbbbaabaa\n\
\bbbaabababaabaaabaabbaba\n\
\bbabbaabaaabbbabbbbbabaa\n\
\baabbaabbaaababbabbaaabb\n\
\aabbbbabaabbbaabbabaabbb\n\
\abbaabbbaabbbabaabaababbabbababa\n\
\bbabaaabbbbaaaabbabaabaaaabbaaaaaabaabaaaaabbaaa\n\
\abbabbbbbbaaaaaabaabbbab\n\
\baaabbaababbbabbabbbbaab\n\
\abaabababbbbaaabbaababaa\n\
\bbbaaaaaaaaabaabbaaaaaababbbabbabaabaabbaaabaaab\n\
\baaaaabbaabbabbbabbaaababaaabbabbbaaabbbbbabbabbbababaab\n\
\bbababaabbababbbaaabaaaa\n\
\abaaaabaaabbbbabaabbbabb\n\
\aaaabbbaabbbbabaabbabbbaaabbaaabbbbbbbab\n\
\bbaabababaaaaababbbbababaaaaaabbbabbbaababbaabaaaaababaaaaaabaab\n\
\ababaaabaaaabaababbbaabbabbaabaaaabbbbbbababbbaaababbbba\n\
\bbabbaabbbbbaaabbaababaa\n\
\aaaabbabbaabaaaaaabbaaab\n\
\bbbaaaaabbbbaaabaabbbaababbbbaaa\n\
\aabaabaabaaaaaabaaababaa\n\
\aaabbbbbaaaaabbbababbabbbaaababa\n\
\abbbaabbababbbaababaabaaababbbaaaabbbbbbbbbbbaabbbbbaabaaaababab\n\
\bbabbaabbaaaaaaaaaaaaaaaabaaabba\n\
\baaaaaaaababbbabaabbaaaaababbbabaabbbbbbabaaaaabbaaaabbbabbabbbaaabbaabbaabababb\n\
\bbbaababbaaabbaaabbabaab\n\
\bbaabbaabbbaaabbbabbbbaabaababbaabbabbbbbbabbbbbbbbbbbaaaabaabba\n\
\bbaaabbabbabbababaaaaaaaabbaaaabbbbbaaba\n\
\babbbbababbabbabbbbaaaaabbbabbabbbbbabbbbbabbbbbbabbabbb\n\
\abaababaaaaabaababaaabab\n\
\bbabbaaabbabbaababbabbaa\n\
\babbabbabbbbaaabbbabababababaaabaaabababbaabbaab\n\
\baabababbbbbaaabaaaaabbbabbbbbbabaaaaaba\n\
\ababbaaabaaabbaababbbbbb\n\
\bababaaabbbbaabbabbbbbbbaabbabba\n\
\bbababaabaaababbbaaaaaabaababbbaaaabbbaaabbaabba\n\
\bbbabbaaaabaaabbabaababbabbbbaab\n\
\bbabaabbbbbaaababbbabbaaaabbbbbbbabbbbbbabaaaaaabbbbbbabbbbabbbababaabaa\n\
\bbabbbbabaaabbaaaabbbbaabaabbaabbbbaabaa\n\
\ababbbaaaabaaabaaabbbaabaaaaaabaaaabbaaa\n\
\ababbbaabbabbbabbbaaabbabbbbbbaa\n\
\aabbababbabbabaaaaaababb\n\
\bababbaabaabababaabbbababaabbabbaabaabaaabababab\n\
\aababbbabbabbaabbaabbbbaaabaabaaaababbbaaaabbbbbbbbbbbaabbbbabbbbbbabbbabbbbbbba\n\
\bababbababaababbaababaab\n\
\babbbaaaabbaaaababbababb\n\
\abbabbbbabaabababaababbb\n\
\bbabbabaaabbababaabababb\n\
\babaabaaaababbbbaabaaababaaaabbaaaaaabab\n\
\abbabbababaabababaaaabbaabbbabbabbabbbbababbaaba\n\
\bbabaaabbaabbbabbbaaabaababaaaaa\n\
\abbbbabaabaababbababbabbabbaaaaabbbabbbb\n\
\baaabbbaaaabbbbaabaaaaabbbbaaabaabbabbbababbaaaa\n\
\aabbbbabbaabbbaaaabbaaaaabaabababbaaabaa\n\
\abbabaababaaabbaaabababbababbaab\n\
\aabbbaabbaaabbbbbbaaababababaaababbaaaaababaaaaabaabbbabbabbaabb\n\
\baabababbbabababbabaabbaaabaabbbabbbbaba\n\
\baabbbbabbaaabbaabaabbab\n\
\bbbaaaabbbbbabababaabbaa\n\
\abaababababbbaaababbbaba\n\
\bbaaaaabbababbabbbaaaaababaabbbabaababaaabbbbaab\n\
\bbabaabaaaabbbbbbabbbbababaaaaabaaababbaaabbbaaaabababaa\n\
\bbaaaaabbaababbaababbbaaaabababb\n\
\babaaabbbbbbaaaaaaabbabb\n\
\aababbbabbbababbbabbabaabaaabbaabaabbaba\n\
\baaabaababbbaaabaabbababaaaababaababbbbbaaaabaaababbabbb\n\
\bbbabbbbbbababbbbaabababbbabaaabababbaabbbaabbbb\n\
\bbbbabbaaaaabaababaaaabaaabababbabbaaabb\n\
\aaabbbbabaabbbaabbbaababaaaabaaaabbbbbbabbbabbbabbababbbabbabbbbaabbbaaa\n\
\aaabbbbaaaaaabbbabbaaaaabbbaabbb\n\
\aababbabaaabbbbbbababaaabaaaababbbaabaab\n\
\bbbabbabababbbbbbabbbbababaabaab\n\
\abbaabaabbaaaabbabbbbbbbaaaaabaabaabababbaababaa\n\
\bbbaabbaaaaabbabaaabbbabbabaaaaa\n\
\ababaaabbabaaabbabbbbbbbaaaababaabaaabbb\n\
\aabaaaabbbbabbbbbabaabaaabababbbabbbaaaabbaaabbababbaabbabaaabab\n\
\aabaaabbbabbbbbaaaaaabaaabbbbbbababaabbb\n\
\babbbbababbaaaabbabababb\n\
\aaabbabaaababbbabaabaaaaaabababaaabbaaab\n\
\babaaabababababbabbaabaaababbabaabaaaabaababababbabaaabbabbabbab\n\
\bbbbaaabaaabaabaababbbaabbaabaaaababaabbaaaabbba\n\
\baaabbbbbbabaababbbbbbab\n\
\abbaaaabbbabaabaaaababab\n\
\aaaabbabababbbbbaabbbabb\n\
\abaaaabaababaabaaaaaaaba\n\
\babbbaaabaaababaabababbaabaaaabb\n\
\aaabbabbaaaabaaabbbaaabbbabbbbbbabbabaab\n\
\aabaaaaabaaabaabaabbaaab\n\
\baabaabbababaabababbaaaa\n\
\abbabbbbababbbaabbabaaababbaaaabaabaabab\n\
\abbaabbbbabbbbbbabbabbbbbababbbabbbaabaaabbbbbabbbabaabbabbabaaa\n\
\baabbabbabaabaaabaababbb\n\
\bbabaaabbaabaabbbbbbbaaababababababaaaaa\n\
\bababbbbbbaabaaabaaaabbaaababbbbbbbabbbaaaabaabb\n\
\aabaaaaabbaaaaababbbabbb\n\
\bababbbbabbbaababababbababbaaabaabbaaaba\n\
\abbaaaaaabbaabaabaabaaababbaabbbbbaababb\n\
\bbbbabababaaabaaaaaaabbbaababaab\n\
\aabbbabbabbbbabaabbabbbbbbabbbbbbaabbbabbaaaabaaaabbaabaaaaaabbb\n\
\babaaabbababbababaaabbaaaaabbbaabbbbbbaa\n\
\ababaaabaaabbbabbbabbbababbbabab\n\
\aaabbbababbabbbbaabaabab\n\
\bababababbabbaabbbbbabbabbbabbbbaabbaaaa\n\
\abbbabbaaababbababbbbbaaaaababababbaaabb\n\
\ababaabaaaaabbbbbbbbabaa\n\
\bbbababbbababbaabbaaaabbaabaabbb\n\
\ababbaabababbababababbba\n\
\bbbbaabbbaababbabbaaabaa\n\
\aabaaaaaaaaaaaaaaaabbabaaaababba\n\
\aabbbbababaabbbababbabbb\n\
\bbbaaaabaaaaabbbbbabbbbabaaabbbbbbbaababbabaabbb\n\
\aababbbbbaabbbbbbbaabbbb\n\
\aababaaabbbbababbabbbabbbabbabbabbbbabaabbbabaaaaaaabbababbaabab\n\
\bbabbaaabbabbababbbabaaa\n\
\abbbbbbbbbaabaaaabaaabba\n\
\baaaababbbbabbabbbaabaaababaaaaabbbaabbb\n\
\babaabaabaaabbbababaaabbbbbaabaa\n\
\abaababbbabaabbaabbbbaab\n\
\aaabaabbbaaaabbaaaaaaabaaaaabababaabbaabaabbbabbababbbaa\n\
\abaaabaaaaabbbbabaabbbba\n\
\bbbaabbabababbbbbabababb\n\
\bbbbaaabbaaabbbbbbaabaaaabaaaaaa\n\
\bbababbbbbababbabbbabbabbaabbbaaaaaabbbbbbbabbababababba\n\
\abbaabbbbaaaaaaababbbbbaabaabbbbaaabaaab\n\
\aaabaababaabaaababbabbba\n\
\abbababababbbbbbbbbaabbaaabaabbbbbbbbbbbbaaabababbbbaababbbaaababaabaaaabbaababaababaaba\n\
\aaaabbbaaabbbbabbbabbaabbabbbbaabaabbababaabababaabbabbbabbabbaaaabbbaba\n\
\baabbaabbbbaaaabbbbaaaaaaaaabbabbaabbbbaabbaaababbbbaaba\n\
\bbaabbbabbaaaaabaabbbaabaabbbbbbabaabaaabbaaabaaabbbabbb\n\
\baaaaaabbabbabaabbbabbabbbbaaabaabbbbaaa\n\
\babbbbabaabaaabaabbbbbbbbbaabaab\n\
\bababbabbbaabbaabababbaabaaaabbaaaaabbaa\n\
\bbbbabbabaabaabbaabbbbababaababbababaaababaabaabbaababaa\n\
\bbabababbaaababbbabaabaa\n\
\ababbbabbaabbabbabbbbaab\n\
\aabbbbaabaabbabbabaabbaaaaaaabba\n\
\ababaaabaaaaababbbabaabb\n\
\bbaabbaaaabbbababbbbbabb\n\
\aabaaabbaabbabbbabababab\n\
\bbbaababbbabbababbbbbaababbbbbaabaaababa\n\
\abbbaaabbaaabbbaaaabbbaa\n\
\aabaaabbbaaaabbaabbbaabbaaaaababbabaaabbabbbbaabbaabbaab\n\
\baabaaaabbabaabaaaabbabbaabbababbabbbbbaaaababbaaaababbb\n\
\baabbabbbaaabbbaabbaabaabbaaaaaaaaaaaabaaabbbaaa\n\
\bbabbbbabbabbaababbbbbbbaabbbababbabaaaaaaabaaaa\n\
\aabaaababbbabbababbbbaba\n\
\abaababbaabaaabaabbbaaaa\n\
\babbbbbabbbbabbabbaabbabbbaaabaaabaabbbbabbbaabbbabaabbbbbabbaabaaaabbabbaabbabaabbbbaabbbabbaab\n\
\baabbaabaabaabaaabaabbbbaaaabaabbaababbbababaabbabbabbbb\n\
\aababbababbbbbaaaaaabababbbbbbaabbbbbbbb\n\
\aababababaaaaaabbbabaaaabbbabbabaaaaaaabbbbbbbaa\n\
\babaabbaaaaaabaabbbbaabbbbbbabbbbbbababa\n\
\aaabbbbabbabaabaababaabababaaabbabbbaabbaaabaaab\n\
\abbabbabbabbbbbaaaabaaab\n\
\abbbaaaabbaaaabbaabaaaabbabbaabbbabaababbbbbbbabaabbbabbababaaabbaabbbabbbbabbbbabbabababaaaabaa\n\
\abbbabaaaaabbabbabbabbbbbbabaaabbabbaabbababbabaaaaababaaaababbababababb\n\
\aabbbaaabbabbbbbabbaaababbaabbab\n\
\aaaabbbaaababaaabababaaaabbabbabaaaababbbbbbbabbbabaabbaabababbb\n\
\aabbbbaaaaabbabaaababbabaabbbbabbbabaaba\n\
\abbabbbbaababbbbabbbbaaa\n\
\aaabbabaaaabaababaabbbaababbabbb\n\
\aabaabaaabbbabbaababbaaabbbbabbb\n\
\bababbaaabaaabaaaaababbb\n\
\aabbbbaabbbaaaaabbaabbaabbabbbaaaabbabab\n\
\aabbabbbaabaaabaabbaababbbbbabaa\n\
\bbaabaaaaaabbbbbbbabbaababbaabbbbaaaaaaabbbaabbbbaaabababababbba\n\
\aaaaaaabbabbabbabbbbbaaaababbbbbaabbbbbababbbaaa\n\
\aababbbaabbbaaabbabaaaba\n\
\aabbbbbbaaababbabaaabbabbabbbbabbbbaaababbbababbabbbabbbabaababaaaababbabbbbabbbaaabbaab\n\
\bbbababbbaaabbababbabbbb\n\
\bbbbbababbbbaabaabaabbaababbbbbb\n\
\bbababbabbaaaaabbbaabbbababbabbb\n\
\abaaababaabbbbbaabbabbaaaabaabbbbbbbbbbabaabaaababbaabba\n\
\baabababbbbbbaababbaaaab\n\
\baaababbbabbbaaabbaaababaabaabba\n\
\bbabbaabababbbabababaabb\n\
\bbbbabbabaabbbbaabaabbbabaaabaababaaababbababbbababababb\n\
\bbabbbaaabbaabaaaababaababbbabababaabbaaabababbbaabbbabb\n\
\bbaabaaabaaabbaabbabbabb\n\
\baaabbbabbbbabbabaaaabbabbbbabbaaabbaaabbbbbbbabbabbbabaaabababaaabbaabb\n\
\baabbaabaaaabaabbaaababbbabababb\n\
\abbaababbbaaaabbbabbabbbabbababbbabbbbbbbbbabababbbbbbbbaaaababbbabbabaabbbbabaabbbbbaab\n\
\abbaababbabaabbabaababbabaababbbaabbbbabaabababaaaabbbaaaababaab\n\
\baaaabbaabbabbbbbabbaaaa\n\
\bbbabaabbbbbaabbbaaabbba\n\
\aaaabbbbaabaaabbbaaaaaba\n\
\bbbabbbbbbaaaaaabbbaababbaabaaaa\n\
\abbaaaaabbbababbbbaabaab\n\
\abbabbbbbabbbaababababab\n\
\baaabaaaaaabbabbbbbbbaabaababbbabababbbabbbbbbbaabbbbaababbbbabbbbbbabba\n\
\bbabababababbbbbabbaabababbaababbaabbabbbbbaabaabbbbabaabaaabaaa\n\
\ababbaabbbbbbbaaaaabbbaaabaaabab\n\
\bababbbbbbbabbaababaabbb\n\
\aaabaabaabbaabaaabbbbbbbabaaaababbaaaaba\n\
\aabbaaaaaaaabbbbabbbabaa\n\
\babbbbbabaaababbbbaaaaabbaaabaab\n\
\baabaaaababaabbaabbababb\n\
\bbbabbbbbabbbbbaaaaabbba\n\
\bbababaaaabbaaaaaaaabbaa\n\
\bbbaaabbbabbbabbaabbaabb\n\
\bbbaabbaaababbbabaaababa\n\
\aababbbbabbabbbbabaaaaabbbbaabbbaaaabbbaababaaaa\n\
\bbababbbbabbbbbaaabbbaaa\n\
\abbbabbaabbaaaaaabbbbbab\n\
\aaababbababaabbbbabbaabbabbbabbb\n\
\bbabbbaaaaabababaabbbbbabaabbaaaaabaabba\n\
\ababaabababababaaabbabba\n\
\babbbbbabaababbabaabaaba\n\
\abbbbbaabbababbaaababaaa\n\
\babaabbabbbababbbabababb\n\
\bbbaaabbbbaaaabaaabbabaabbbbbabbbbaabbababbaabba\n\
\ababbbabbbaabbbaabaabbbaaaabbbbbaababaab\n\
\abaaaaabaababbabababbaaaaabbbbbaaaababbaaaababbbbbaaabaabbbbaaba\n\
\baaaabbababababaabbbaababbbabbaabbbaaabaabaababbbbbabaabbbaabbab\n\
\aabbaaaaabbbbbaabaabbaba\n\
\ababbaaaaaabbbbbaababbabbbbabbaaaabaaaaaabaabaaaabbababa\n\
\bababbabbbbaababbaaaabbb\n\
\bbbbabbaaababbbabaababbb\n\
\bbbbababaaaabbaabbaabaabbbbaabbaabbbbbbabbbbbaaabbbaabba\n\
\aababababbbbbbaaaabaaabbbabbbaaaaaaababbabaabbbabbbabbaaaaaababaabbbaabaabbbabbbbbbbabbb\n\
\bbaabbaabbbbabbaaaaababaaaabbabaaaabbaabbaabaabaaabbaaab\n\
\aabbbaabbbbbaabbbbabbbaabbbaaababaabbbbb\n\
\bbabbbaabaaabbabbbbaaaababaabbab\n\
\bbbaaabbabbaabbbbabababaaababbbaabbbbaaa\n\
\bbababaaabaaaaabbbbaaabaaabbbaaababaaabaaabbaabababababaaabbbaaa\n\
\ababbbaaabbbaaabbabbaaba\n\
\baabbbbabaabbaabbaabbabbbbababaabbabbabbabaabbaa\n\
\aabbaaaababaaabbbabbabaabaaabbbbbaabbbabbbbbabbb\n\
\baababbaaabbaaaaabaaabaaaaaabbabbabbaaba\n\
\bbbaababbbbbbaaabbaabaaabbabaabaaababaaaaabbaaab\n\
\babaabbabbbaabbaaaababba\n\
\bbbaaaabaabbbbbbbbbbabaa\n\
\ababbbabbaabbaabaaabbbabaaababaa\n\
\abbbbbaabbbaaabaabaabbbaaaabbbababbbabbbbaabbaaaabbbbbab\n\
\bbbbaaaabbaaaabbaabbababbabbabba\n\
\baabababbabbbbababaabababbbababa\n\
\aaabbaaaaabbbbbabbaabbaaabaaaababbbababbbabaabbaaaaaaaba\n\
\baaaaaaaaabbaaaaaaabaaab\n\
\bababbbbaaaabbabbababbbbbbabaaababbbbaaa\n\
\babbbbbababbbbaaabbbbbbbaaabbababbbabaaa\n\
\baaababaaabbbaabbbbaaabbbbabbabaaababaaaaabbbbbb\n\
\abbbbbaabbabbabababbabbb\n\
\ababbbabbbabbaabbaabbaaa\n\
\aaaabaaaaaaabbababaabbab\n\
\bbbbbaabbaaabbbbaaabaababaaabbbabbaaaaababbaaaaababaabab\n\
\abbabbbbbababaaabaabbbaaabbbbbab\n\
\abbaaaababababaabaaaababaabbaabbabbabaabaabbaabbbaaaabbaaaabaaaa\n\
\baaababbaaaabaabbaaaabbababaabbbbabbaaaa\n\
\aaabaabaabaabbaaababbaababbabbbaabbbbbaaabaaaaabbbbababa\n\
\abbbbaaaaaaabbaaabababaaaabaabaabaabbaaaabaaaabaabaabbabbbaabaab\n\
\bbabbbaaaabaaabbbababbabbbaabaab\n\
\babababaaaabaabaaabaabab\n\
\baaabaabbbaaabbabbaabbbabaabbabbbbbaabbabbbbabaa\n\
\ababaaabbbababbabbbabbabbbbbbbbababababb\n\
\ababbabbbbaaabaaaaababaa\n\
\baaaababaabbbbbbaaababaa\n\
\abbaaaababaabababbabaabaabbabaaa\n\
\babbabababbaaabaaaaababaababaaabbaaaabbaababbbaaaabbbbaaaabaabbaaaababab\n\
\babaaabbbaaaaaabaabbaaab\n\
\abbaaaabaaaabaaabaabaaba\n\
\bbabbbabbaaaababababbbbabbaaaaabaabababaabbaaabaaababaaabbbbbbaa\n\
\baaabaabbbbbbaaabaaabbabaaabbbbabbaabababbbbaaba\n\
\abbbaabbabaabababbbbabaa\n\
\bbabbbaabbabbaabbabaaaaa\n\
\bbaaaaababbbaaabbbbbaabbabababbb\n\
\aaaaabbbaabaaaaaabbaaaba\n\
\bbaaaaaaaaabbabbbaaababa\n\
\abaabaabababbababbaabbbbababaaaa\n\
\bbbaabbabbabbaabaabaaabbbabaaaab\n\
\bbabbaabbabbabbabbbaaabbbbbaabbaaabbbaab\n\
\abbaaabaaabaababbababaab\n\
\baaabbbabbabababbbbaabbabbbaabababbbbaba\n\
\abbabbbbaaabbabbababbbba\n\
\aabaabaabbabbbbababbaabb\n\
\aababbbabababbabababbaaa\n\
\abababbaaabbababaabababaababaabbaabaaabbbbaaabbbabababbaabbaaaaa\n\
\baaaabbabaaabbabbbaaabbaaaaababaabaababbabaabaab\n\
\baabbaaaabbaababaabbaabbbaabaaaabaaaabaabababbbaabbbbbbaaababbbaaabbbaababbaaaaaabababab\n\
\baaabbaaaaabbbabababaaababbbbbab\n\
\abaabbbababaabaabaaaababbbaaaaabbabaaaabbabaaaabbaababaabaaaabbb\n\
\ababbaaababaabaaaabaabbb\n\
\aaaabaabababaaaaaaabbabbaaababbbababaabbbaaaabaaaaaabbaaabababbbaaaabbaa\n\
\abaabbabbabbbaabbbbbbbbbabaabbbbabbaabbaabababaa\n\
\aababbabbbabaababababaababbaabbabbaaabaababbabbb\n\
\aaabaababbbabaabababaaabbbbbababbababbbababaaabbbaababbabbbababbaaaababb\n\
\aaabaabaabbaabbbbbbbbaabaaaaaababbbababa\n\
\baabbabbbbaaaaababaabaab\n\
\baaaaaabbaaabaabbababbaaababbbbbaabbabbbbabbbabb\n\
\ababbbbbbaaaaaabbbabbaabbbbababa\n\
\bbbabababbbabbabbbabbabababbbabaababbaaabbaababbbbabbaab\n\
\abbaaaaaaabbbaabaaaaabbbbbbbbbbaaabbabaa\n\
\babbbaaaaabaaabbbbabbaab\n\
\bbabbababaababbabbbabaab\n\
\aabbaaaabaabbabbbbabaabb\n\
\abbbabbabaabaaabaaaaabba\n\
\babbbabbbaabbabbabbabaaa\n\
\abbbbbbabaaababbababbaba\n\
\aaaabbaababbababbbbbabbbbbbbabababaabbbabbaaabaaaaaaabba\n\
\babaabaabbaaababaabbbbbbbbaabaaaababbaaaabaaaaaaabbabaaa\n\
\bbbbaabbaabaaabbbababaaa\n\
\baabababaaabbabbabaaaabaaaaabaaabaaaaabb\n\
\aaaabababaababbabaaaabbb\n\
\abaababbbbaabbaaaaabbbbabbbabbbaabbababb\n\
\bbabbaabaaaababaaabaaababbabbaabaaaaaaabbaababbbbbbbbbba\n\
\abaaaaabaaaabbbbababaaaa\n\
\bbbaaaababbbaaababbbbaaa\n\
\abaabaaaaabbabaaababaaaaaabababa"

someFunc :: IO ()
someFunc = do
  let messages = lines messagesInput
      ruleMap = createRuleMap rulesInput
      validStrings = Set.fromList $ evaluateRule 0 ruleMap
      matchingMessages = filter (flip Set.member validStrings) messages
  -- putStrLn $ show ruleMap
  putStrLn $ "Number of matching messages: " ++ show (length matchingMessages)

  let results2 = (length . filter (/= Nothing) . map (doParse (blah (take 5000 diagonalPairs) ruleMap))) messages
  putStrLn $ "Number of matching messages with looping rules: " ++ show results2
