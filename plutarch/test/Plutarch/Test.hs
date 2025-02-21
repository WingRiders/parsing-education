module Plutarch.Test ((#@?=), psucceeds, pfails) where

import Data.Text (
    Text,
    unpack,
 )
import Plutarch.Evaluate
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Util (reflow)
import Test.Tasty.HUnit
import Plutarch.Prelude
import Plutarch.Script
import Plutarch.Internal.Term (compile, Config (..), LogLevel (..), TracingMode (..))
import Plutarch.Internal.Other (printScript)

comp :: HasCallStack => ClosedTerm a -> Script
comp t = either (error . unpack) id $ compile (Tracing LogInfo DoTracing) t

-- | Like `shouldBe` but for Plutarch terms
pshouldBe :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
pshouldBe x y = do
    p1 <- eval (comp x)
    p2 <- eval (comp y)
    printScript p1 @?= printScript p2
  where
    eval :: Script -> IO Script
    eval s = case evalScript s of
        (Left e, _, traces) -> assertFailure (prettyLogsWithError "Term evaluation failed:" (show e) traces)
        (Right x', _, _) -> pure x'

prettyLogsWithError :: Pretty a => Doc ann -> a -> [Text] -> String
prettyLogsWithError msg e = prettyLogs (msg <> line <> indent 2 (pretty e))

prettyLogs :: Doc ann -> [Text] -> String
prettyLogs msg traces =
    renderString
        . layoutPretty (LayoutOptions (AvailablePerLine 110 1))
        . unAnnotate
        $ msg
            <> line
            <> "Traces:"
            <> line
            <> indent 2 (align . vsep . punctuate line . map reflow $ traces)

-- | Like `@?=` but for Plutarch terms
(#@?=) :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = pshouldBe

infix 4 #@?=

-- | Asserts the term evaluates successfully without failing
psucceeds :: HasCallStack => ClosedTerm a -> Assertion
psucceeds = psucceedsTraceInputs []

psucceedsTraceInputs :: HasCallStack => [Text] -> ClosedTerm a -> Assertion
psucceedsTraceInputs inputs p = case evalScript $ comp p of
    (Left e, _, traces) -> assertFailure (prettyLogsWithError "Term evaluation failed:" (show e) (inputs <> traces))
    (Right _, _, _) -> pure ()

-- | Asserts the term evaluates without success
pfails :: HasCallStack => ClosedTerm a -> Assertion
pfails = pfailsTraceInputs []

pfailsTraceInputs :: HasCallStack => [Text] -> ClosedTerm a -> Assertion
pfailsTraceInputs inputs p = do
    case evalScript $ comp p of
        (Left _, _, _) -> pure ()
        (Right _, _, traces) -> assertFailure (prettyLogs "Term evaluation succeeded:" (inputs <> traces))
