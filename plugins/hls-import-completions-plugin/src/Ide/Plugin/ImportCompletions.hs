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
import           Data.List
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
            case sess of
              (Just sess) -> do
                hello <- envVisibleModuleNames sess
                let strs =  maybe [] (map moduleNameString) hello
                return $ J.List $ map mkCompl strs
              _ -> return $ J.List []
        _ -> return $ J.List []


mkCompl :: String -> J.CompletionItem
mkCompl label =
  J.CompletionItem (T.pack label) (Just J.CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing
