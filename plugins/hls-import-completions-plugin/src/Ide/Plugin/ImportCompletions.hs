{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.ImportCompletions
  ( descriptor
  ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               hiding (use, List)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Char                  (isSpace)
import qualified Data.HashMap.Strict        as H
import           Data.List.Extra            (nubOrdOn)
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Development.IDE            as D
import           Development.IDE.GHC.Compat
import           Ide.Types
import qualified Language.LSP.Server        as LSP
import qualified Language.LSP.Types         as J
import qualified Language.LSP.Types.Lens    as J
import qualified Language.LSP.VFS           as VFS
import qualified Text.Fuzzy                 as Fuzzy
import Development.IDE.Plugin.Completions.Types 
import Ide.Plugin.Config

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { 
    pluginHandlers = mkPluginHandler J.STextDocumentCompletion completion
  }



-- TODO - add boolean check to only filter on
completion :: IdeState
    -> PluginId
    -> J.CompletionParams
    -> LSP.LspM Config (Either J.ResponseError (J.ResponseResult J.TextDocumentCompletion))
completion _ide plId complParams = do
    let (J.TextDocumentIdentifier uri) = complParams ^. J.textDocument
        position = complParams ^. J.position
        fp = uriToFilePath' uri
    contents <- LSP.getVirtualFile $ toNormalizedUri uri
    liftIO $ fmap (Right . J.InL) $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) -> do
            sess <- runAction "ImportCompletions" _ide $ use GhcSession (toNormalizedFilePath' _path)
            mPreFix <- VFS.getCompletionPrefix position cnts
            isImportPrefix <- isImportModulePreFix <$> VFS.getCompletionPrefix position cnts
            case (sess, isImportPrefix, mPreFix) of
              (Just sess, True, Just preFix) -> do
                let VFS.PosPrefixInfo { fullLine, prefixModule, prefixText } = preFix
                    enteredQual = if T.null prefixModule then "" else prefixModule <> "."
                    fullPrefix  = enteredQual <> prefixText
                mModules <- envVisibleModuleNames sess
                let importModules =  maybe [] (map moduleNameString) mModules
                return $ J.List $ filtListWith mkCompl (map T.pack importModules) fullPrefix enteredQual
              _ -> return $ J.List []
        _ -> return $ J.List []

filtListWith :: (T.Text -> J.CompletionItem) -> [T.Text] -> T.Text -> T.Text -> [J.CompletionItem]
filtListWith f list fullPrefix enteredQual =
  [ f label
  | label <- Fuzzy.simpleFilter fullPrefix $ list
  , enteredQual `T.isPrefixOf` label
  ]

isImportModulePreFix :: Maybe VFS.PosPrefixInfo -> Bool
isImportModulePreFix (Just pfix) = 
  "import " `T.isPrefixOf` VFS.fullLine pfix
isImportModulePreFix Nothing = False

mkCompl :: T.Text -> J.CompletionItem
mkCompl label =
  J.CompletionItem label (Just J.CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing
