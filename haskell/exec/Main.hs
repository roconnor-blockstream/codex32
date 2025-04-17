module Main where

import Control.Applicative ((<**>), (<|>), many)
import Control.Arrow ((+++))
import Control.Monad (unless)
import Data.List (find,intercalate, nub)
import Data.Monoid ((<>))
import Data.Text (pack)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Types as Opt
import qualified Prettyprinter as PP
import qualified Prettyprinter.Util as PP
import qualified System.Exit as Sys

import Codex32
import Codex32.Error
import Codex32.Word5
import Codex32.Polynomial

data AdvancedOptions = AdvancedOptions { optLength :: Int
                                       , optOverrideLength :: Bool
                                       , optErasures :: [Int]
                                       , optSpec :: Spec
                                       , optResidue :: Poly Word5
                                       }

data Command = Simple String
             | Advanced AdvancedOptions

residueReader :: Opt.ReadM (Spec, [Word5])
residueReader = do
  result <- Opt.eitherReader parse
  let len = length result
  spec <- maybe (failLength len) return $ find (\spec -> len == specDegree spec) specs
  return (spec, result)
 where
  parse = (errMsg +++ reverse) . fromString
  errMsg c = "Illegal Bech32 character " ++ show c ++ "."
  failLength len = fail $ "Residue length must be " ++ intercalate " or " (show <$> validDegrees) ++ "."
  validDegrees = specDegree <$> specs
  specs = [codex32Spec, codex32LongSpec]

codex32SimpleParser :: Opt.Parser Command
codex32SimpleParser = Simple <$> Opt.strArgument (Opt.metavar "CODEX32_STRING" <> Opt.help "Codex32 string to correct")

codex32AdvancedParser :: Opt.Parser Command
codex32AdvancedParser = Advanced <$>
  (mkAdvancedOptions <$> Opt.option Opt.auto (Opt.long "len" <> Opt.metavar metaLength <> Opt.help "Total length of codex32 string")
                     <*> Opt.switch (Opt.long overrideOptName <> Opt.hidden)
                     <*> many (Opt.option Opt.auto (Opt.short 'e' <> Opt.metavar "ERASURE_LOCATION" <> Opt.help "Location of unreadable character (can be repeated)"))
                     <*> Opt.argument residueReader (Opt.metavar metaResidue <> Opt.help "Residue from worksheet"))
 where
  mkAdvancedOptions l ol e (s, r) = AdvancedOptions l ol e s r

metaLength = "LENGTH"
overrideOptName = "override_length"
metaResidue = "RESIDUE"

codex32CorrectParser :: Opt.ParserInfo Command
codex32CorrectParser = Opt.info ((codex32SimpleParser <|> codex32AdvancedParser) <**> Opt.helper) (Opt.progDescDoc desc)
 where
  pretty = PP.reflow . pack
  desc = Just $ pretty "Error correct codex32 strings."
             <> PP.line <> PP.line
             <> pretty "Pass an 'ms1' string on the command line to try and output an error-corrected version of that string."
             <> PP.line <> PP.line
             <> pretty "Alternatively, if you are verifying your checksum using the worksheet from the Codex32 booklet, you can pass '--len 48' followed by the 13 character residue you computed on your worksheet."
             <> PP.space <> PP.space
             <> pretty "If you have unreadable characters in your string, use your best guess and add '-e <location>' to let the tool know which box contained your unreadable character."
             <> PP.space <> PP.space
             <> pretty "If error correction is possible, the program will output instructions on which character locations contain errors and what values to add, using the booklet's addition wheel, in order to correct the error."
             <> PP.space <> PP.space
             <> pretty "With this method, the computer can help you correct your codex32 string without even needing to know what the string is."

codex32CorrectOptions :: Opt.Parser Command
codex32CorrectOptions = Opt.subparser (Opt.command "correct" codex32CorrectParser)

codex32Options :: Opt.ParserInfo Command
codex32Options = Opt.info (codex32CorrectOptions <**> Opt.helper) mempty

codex32Prefs :: Opt.ParserPrefs
codex32Prefs = Opt.prefs Opt.showHelpOnEmpty

formatCorrections :: Int -> [(Int, Word5)] -> String
formatCorrections _ [] = "No errors found.  Residue is correct."
formatCorrections len corrections = unlines (header : (fmt <$> corrections))
 where
  header = show len ++ " errors found.  Make the following corrections."
  fmt (ix, delta) = "Add " ++ show (toChar delta) ++ " to position " ++ show (len - ix) ++ "."

main :: IO ()
main = Opt.customExecParser codex32Prefs codex32Options >>= run

run :: Command -> IO a
run (Simple codex32Str) =
  case (correctCodex32String codex32Str) of
    Nothing -> putStrLn "Failed to error correct string" >> Sys.exitFailure
    Just str -> putStrLn str >> Sys.exitSuccess
run (Advanced options) | 13 < length erasureIxs = failWith "No more than 13 -e options are allowed."
                       | not (optOverrideLength options) && bitsize `notElem` [128, 256, 512] = failWith $ "Unusual bitsize found.  Override with --" ++ overrideOptName ++ "."
                       | 5 <= padding = failWith $ "Invalid " ++ metaLength ++ "."
                       | len < 48 = failWith $ metaLength ++ " too short."
                       | 127 < len = failWith $ metaLength ++ " too long."
                       | specDataLength spec < 6 + payloadLength = failWith $ metaLength ++ " too long for " ++ show (length residue) ++ " character " ++ metaResidue ++ "."
                       | 15 == length residue && len < 99 = failWith $ metaLength ++ " too short for " ++ show (length residue) ++ " character " ++ metaResidue ++ "."
                       | otherwise = format result
 where
  erasureIxs = nub (optErasures options)
  residue = optResidue options
  len = optLength options
  spec = optSpec options
  dataLength = len - length (specPrefix spec) - 1
  payloadLength = dataLength - 6 - length (specTarget spec)
  (bytesize, padding) = (payloadLength * 5) `divMod` 8
  bitsize = bytesize * 8
  failWith str = Opt.handleParseResult . Opt.Failure $ Opt.parserFailure codex32Prefs codex32Options (Opt.ErrorMsg str) [Opt.Context "correct" codex32CorrectParser]
  result = errorCorrections (optSpec options) erasureIxs residue
  format Nothing = putStrLn "Too many errors.  Unable to correct." >> Sys.exitFailure
  format (Just corrections) = putStr (formatCorrections len corrections) >> Sys.exitSuccess
