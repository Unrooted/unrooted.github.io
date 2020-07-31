{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Traversable
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Hakyll

main :: IO ()
main = do
  setLocaleEncoding utf8
  hakyll do
  
    match "*.md" do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= finalizeHtml datedCtx
  
    match "posts/*" do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"  datedCtx
        >>= finalizeHtml datedCtx
  
    create ["archive.html"] do
      route idRoute
      compile do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          archiveCtx =
            listField "posts" datedCtx (return posts) <>
            constField "title" "Archives"      <>
            defaultContext
  
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= finalizeHtml archiveCtx
  
  
    match "index.html" do
      route idRoute
      compile do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          indexCtx =
            listField "posts" datedCtx (return posts) <>
            constField "title" "Home"        <>
            defaultContext
  
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= finalizeHtml indexCtx
  
    match "homebrew/**/*.md" do
      route $ setExtension "html"
      compile do
        metadata <- getMetadata =<< getUnderlying
        let system = fold $ lookupString "brew-system" metadata
            license = fold $ lookupString "brew-license" metadata
            string = systemLicenseString system license
            brewCtx = constField "systemandlicense" string <> datedCtx

        pandocCompiler
          >>= loadAndApplyTemplate "templates/homebrew.html" brewCtx
          >>= finalizeHtml brewCtx
    
    match "homebrew/index.html" do
      route $ setExtension "html"
      compile do
        indexes <- loadAll "homebrew/**/index.md"
        let ctx = mconcat
              [ listField "posts" datedCtx (return indexes)
              , constField "title" "Homebrew Material Index"
              , defaultContext
              ]

        getResourceBody
          >>= applyAsTemplate ctx
          >>= finalizeHtml ctx
    
    match "misc/*.html" do
      route idRoute
      compile $ getResourceBody
        >>= finalizeHtml datedCtx

    match ("images/*" .||. "js/*" .||. "*.html") do
      route   idRoute
      compile copyFileCompiler

    match "css/*" do
      route   idRoute
      compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler
  
  
--------------------------------------------------------------------------------

finalizeHtml :: Context String -> Item String -> Compiler (Item String)
finalizeHtml ctx =
  loadAndApplyTemplate "templates/default.html" ctx
    >=> relativizeUrls

datedCtx :: Context String
datedCtx =
  dateField "date" "%A %B %-e, %Y" <>
  defaultContext

systemLicenseString :: String -> String -> String
systemLicenseString system license = let
  systemId = toLower <$> system
  systemName = fromMaybe system $ Map.lookup systemId knownSystems
  
  defaultLicense = if "dnd" `isPrefixOf` systemId
    then "under the Open Game License"
    else "with All Rights Reserved"
  licenseId = toLower <$> license
  licenseName = if null license
    then defaultLicense
    else "under " ++ (fromMaybe license $ Map.lookup licenseId knownLicenses)
  in unwords ["Content for the", systemName, "system; released", licenseName]

knownSystems :: Map String String
knownSystems = Map.fromList
  [ "dnd5e" .= "Dungeons & Dragons 5e"
  , "fate" .= "FATE"
  ]
  where (.=) = (,)

knownLicenses :: Map String String
knownLicenses = Map.fromList
  [ "ogl" .= "the Open Game License"
  , "cc" .= "CreativeCommons-Atrribution-ShareAlike (CC-BY-SA)"
  ]
  where (.=) = (,)