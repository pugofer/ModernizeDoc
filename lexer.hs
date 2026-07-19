{-# LANGUAGE NPlusKPatterns #-}
module Main where
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import System.Environment (getArgs)
import System.IO          (stderr, hPutStrLn)

------------------------------------------------------------
-- Ontology 0 : Input Lines
-- Read from a file typically
------------------------------------------------------------

type Line = String

------------------------------------------------------------
-- Ontology 1 : Classified source lines
------------------------------------------------------------

data PreLexeme
     = PHead   Int String      -- level, heading
     | PCmd    String String   -- CmdName CmdRest; igonred
     | PLine   Int String      -- indent, text
     | PBull   Int String      -- indent, bullet text
     | PBlank
     deriving (Eq, Show)

------------------------------------------------------------
-- Ontology 2 : Lexemes
-- Output of the Layout Elaborator
--
-- Layout has been resolved by:
--   - replacing indentation with explicit structural tokens;
--   - removing ignorable lines.
--
-- Note: PreLexeme -> [Lexeme]

-- A PreLexeme may:
--     - disappear,
--     - elaborate into one Lexeme,
--     - elaborate into several Lexemes.
------------------------------------------------------------

data Lexeme
    = LHead Int String

    | LLine String
    | LBlank

    | LBullBeg
    | LBullEnd

    | LVerbBeg
    | LVerbEnd
    deriving (Eq, Show)    

------------------------------------------------------------
-- Private to the Layout Elaborator.
-- Elements stored on the layout stack.
------------------------------------------------------------

data LayoutKind -- 'C' stands for the context on stack
    = CBull
    | CVerb
    deriving (Eq, Show)

type LayoutFrame = (LayoutKind, Int)

closeFrame :: LayoutFrame -> Lexeme
closeFrame (CBull, _) = LBullEnd
closeFrame (CVerb, _) = LVerbEnd

closeUntil :: Int -> [LayoutFrame] -> ([Lexeme], [LayoutFrame])
-- pop stack & emit close tags until ii >= si
closeUntil _ [] = ([], [])
closeUntil ii stk@(f@(_, si):fs)
  | ii >= si  = ([], stk)
  | ii <  si  = (out ++ [closeFrame f], fs')
           where (out, fs') = closeUntil ii fs

flushStk :: [LayoutFrame] -> [Lexeme]
flushStk fs = [closeFrame f | f <- fs]

closeAll :: [LayoutFrame] -> ([Lexeme], [LayoutFrame])
closeAll stk = (flushStk stk, [])

-- Transformer 1
classifyAll :: [Line] -> [PreLexeme]

-- Transformer 2
elaborate :: [PreLexeme] -> [Lexeme]

-- Lexer = Transformer 1 + 2
lexer :: [Line] -> [Lexeme]
lexer = elaborate . classifyAll


classify :: Line -> PreLexeme
-- bad order!!
classify l
  | all isSpace l      = PBlank
  | "." `isPrefixOf` l = parseCmd (drop 1 l)
  | " " `isPrefixOf` l = spaceCase l
  | otherwise          = PLine 0 l


spaceCase :: Line -> PreLexeme
spaceCase l
  | "       o  " `isPrefixOf` l = PBull 10 (drop 10 l)
  | "  o  "      `isPrefixOf` l = PBull 5  (drop 5  l)
  | otherwise                   = PLine (length sp) txt
  where
    (sp, txt) = span (== ' ') l

parseCmd :: String -> PreLexeme
parseCmd s =
  case cmd of
    "ST" -> handleSect rest
    "co" -> PCmd cmd rest
    _    -> PCmd cmd rest
--    _    -> error ("Unhandled cmd: " ++ cmd)
  where 
    (cmd, rest0) = break isSpace s
    rest         = dropWhile isSpace rest0

handleSect :: String -> PreLexeme
handleSect s = PHead level rest
   where  
     (levstr, rest0) = break isSpace s
     rest            = dropWhile isSpace rest0

     -- Replaces dots with spaces, then uses 'words' to extract non-empty segments
     level  = length (words [if c == '.' then ' ' else c | c <- levstr])


classifyAll = map classify

elaborate pls = elabLoop [] pls

-- Algorithm
-- Invariant:
--   stk is the current layout stack.
--   pls is the remaining unprocessed PreLexemes.
--
--   stk = []
--   while pls:
--       pl, pls = pls[0], pls[1:]
--
--       out, stk = step(pl, stk)
--       emit(out)
--
  --   emit(flushStk(stk))

elabLoop :: [LayoutFrame] -> [PreLexeme] -> [Lexeme]
elabLoop stk [] = flushStk stk
elabLoop stk (pl:pls) = out ++ elabLoop stk' pls
  where (out, stk') = step pl stk

-- Elaborate one PreLexeme; May emit zero or more Lexemes.
step :: PreLexeme -> [LayoutFrame] -> ([Lexeme], [LayoutFrame])  

step (PHead lev hd ) stk = (flushStk stk ++ [LHead lev hd], [])
step (PCmd cmd args) stk = ([], stk)
step (PBlank)        stk = ([LBlank], stk)
step (PLine i t)     stk = stepLine t i stk
step (PBull i t)     stk = stepBull t i stk

-- When an input indent and a contextual stack-top indent are compared
-- the result can be one of 4 cases
data IndentRelation = CRSibl | CRChild | CRClose

cmpInd :: Int -> Int -> IndentRelation
-- Relation between incoming-indent and stack-indent
cmpInd ii si
 | ii >  si = CRChild
 | ii == si = CRSibl
 | ii <  si = CRClose

-- stepLine and stepBull
-- Elaborate a line with its indent in stack context 

stepLine :: String -> Int -> [LayoutFrame] -> ([Lexeme], [LayoutFrame])

-- a flush-left line in outermost context
stepLine t 0   [] =  ([LLine t], [])
-- an indented line in outermost context
stepLine t ii@(i+1) [] = ([LVerbBeg, LLine t], [(CVerb, ii)])
-- flush-left line with pending context
-- wonder if this is more case analysis than required
stepLine t 0 stk  = (ls ++ [LLine t], [])
  where (ls, []) = closeAll stk
      
-- an indented line in Verb context
stepLine t ii@(i+1) stk@((CVerb, si) : fs) =
  case cmpInd ii si of
    CRSibl  -> ([LLine t], stk)
    CRChild -> ([LVerbBeg, LLine t], (CVerb, ii) : stk)
    CRClose -> let (ls, stk') = closeUntil ii stk in (ls ++ [LLine t], stk')
-- an indented line in bullet context
stepLine t ii@(i+1) stk@((CBull, si) : fs) =
  case cmpInd ii si of
    CRSibl  -> ([LLine t], stk)
    CRChild -> ([LVerbBeg, LLine t], (CVerb, ii) : stk)
    CRClose -> let (ls, stk') = closeUntil ii stk in (ls ++ [LLine t], stk')


stepBull :: String -> Int -> [LayoutFrame] -> ([Lexeme], [LayoutFrame])
-- a flush-left bullet -- impossible
stepBull t 0 stk =  error "FlushLeft Bullet"
-- a top level bullet ie no context
stepBull t ii@(i+1) [] = ([LBullBeg, LLine t], [(CBull, ii)])
      
-- a nested bullet
stepBull t ii stk@((CBull, si) : fs) =
  case cmpInd ii si of
    CRSibl  -> ([LBullEnd, LBullBeg, LLine t] , stk)
    CRChild -> ([LVerbBeg, LLine t], (CVerb,ii) : stk)
    CRClose -> let (ls, stk') = closeUntil ii stk in (ls ++ [LLine t], stk')

-- bullet inside verb block not allowed?
stepBull t ii stk@((CVerb, si) : fs) = error "Bullet inside Verb"


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "Error: No input file"
    [infile] -> do 
      contents <- readFile infile
--       let result = process $ map classify $ lines contents
      let result = lexer (lines contents)
      mapM_ print result
