{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens            ((^.))
import qualified Language.LSP.Types.Lens as L
import           System.FilePath
import qualified Ide.Plugin.ImportCompletions as ImportCompletions
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

importCompletionsPlugin :: PluginDescriptor IdeState
importCompletionsPlugin = ImportCompletions.descriptor "ImportCompletions"

tests :: TestTree
tests =
  testGroup "pragmas"
  [ completionTests ]

completionTests :: TestTree
completionTests =
  testGroup "completions"
  [ testCase "completes pragmas" $ runSessionWithServer importCompletionsPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 4) (Position 0 34)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "LANGUAGE") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "LANGUAGE"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "LANGUAGE ${1:extension} #-}"
        item ^. L.detail @?= Just "{-# LANGUAGE #-}"

  , testCase "completes pragmas with existing closing bracket" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      let te = TextEdit (Range (Position 0 4) (Position 0 33)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "LANGUAGE") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "LANGUAGE"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "LANGUAGE ${1:extension} #-"
        item ^. L.detail @?= Just "{-# LANGUAGE #-}"

  , testCase "completes options pragma" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 4) (Position 0 34)) "OPTIONS"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "OPTIONS_GHC") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "OPTIONS_GHC"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "OPTIONS_GHC -${1:option} #-}"

  , testCase "completes ghc options pragma values" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      let te = TextEdit (Range (Position 0 0) (Position 0 0)) "{-# OPTIONS_GHC -Wno-red  #-}\n"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 24)
      let item = head $ filter ((== "Wno-redundant-constraints") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "Wno-redundant-constraints"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Nothing
        item ^. L.insertText @?= Nothing

  , testCase "completes language extensions" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 24) (Position 0 31)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 24)
      let item = head $ filter ((== "OverloadedStrings") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "OverloadedStrings"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes the Strict language extension" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 13) (Position 0 31)) "Str"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 16)
      let item = head $ filter ((== "Strict") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "Strict"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes No- language extensions" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 13) (Position 0 31)) "NoOverload"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 23)
      let item = head $ filter ((== "NoOverloadedStrings") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "NoOverloadedStrings"
        item ^. L.kind @?= Just CiKeyword
  ]

goldenWithPragmas :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithPragmas title path = goldenWithHaskellDoc pragmasPlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
