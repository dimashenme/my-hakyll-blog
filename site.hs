{-# LANGUAGE OverloadedStrings #-}


----------------------------------------------------------------------
--
-- my hakyll blog assembled together using many other people's blog scripts
--
----------------------------------------------------------------------


import           Data.Monoid (mappend, (<>))
import           Hakyll
import qualified Data.Set as S
import           Data.List
import           Data.Char (toLower)
import           Text.Pandoc.Options
import           Control.Monad

-- Deploy command

config :: Configuration
config = defaultConfiguration
        {   deployCommand = "rsync -avz -e ssh --exclude 'drafts' --safe-links ./_site/ dmitri83@ssh.hcoop.net:www/blog/"}

-- Pandoc + Math options

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions


-- nonDrafts :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
-- nonDrafts = return . filter f
--   where
--     f = not . isPrefixOf "drafts/" . show . itemIdentifier

-- recentFirstNonDrafts :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
-- recentFirstNonDrafts items = do
--                        nondrafts <- nonDrafts items
--                        recentFirst nondrafts

main :: IO ()
main = hakyllWith config $ do
-- templates
    match "templates/*" $ compile templateCompiler
  
-- static stuff
  
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

-- special invariable pages

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

-- tags init

    tags <- buildTags "posts/*" (fromCapture "tag/*")

-- posts

    match ("posts/*.md" .||. "posts/*.markdown") $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
          >>= loadAndApplyTemplate "templates/post-body.html" (postCtx tags)
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= relativizeUrls


-- tags 
    tagsRules tags $ \tag pattern -> do
        let title = "Posts with label " ++ " &#8216;" ++ tag ++ "&#8217;"
        route tagRoute
        compile $ do
            posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)

            let tagCtx =
                    constField "title" title <>
                    listField "posts" (postCtx tags) (return posts) <>
                    defaultContext
                    
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls

    create ["tags/index.html"] $ do
      route idRoute
      compile $ do 
        cloud <- renderTagCloud 80 300 tags
        let cloudCtx =
              constField "title" "Tag cloud" <>
              constField "cloud" cloud <>
              defaultContext

        makeItem "" 
          >>= loadAndApplyTemplate "templates/tagcloud.html" cloudCtx 
          >>= loadAndApplyTemplate "templates/default.html" cloudCtx 
          >>= relativizeUrls
            
-- drafts
          
    match ("drafts/*.md" .||. "drafts/*.markdown") $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post-body.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/draft.html" (postCtx tags)
            >>= relativizeUrls

-- archive page

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

-- blog pages, including the main one

    pag <- buildPaginateWith grouper "posts/*" makeId

    paginateRules pag $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots pattern "content"

            let paginateCtx = paginateContext pag pageNum
                ctx =
                  constField "title" ("Blog Archive - Page " ++ (show pageNum)) <>
                    listField "posts" (teaserCtx tags) (return posts) <>
                    paginateCtx <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/blogpage.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

-- Contexts

postCtx :: Tags -> Context String
postCtx tags = mconcat
  [
    dateField "date" "%B %e, %Y"
  , tagsField "tags" tags    
  , defaultContext
  ]

teaserCtx tags = teaserField "teaser" "content" `mappend` (postCtx tags)

-- tag route

replaceWithDash :: Char -> Char
replaceWithDash c =
    if c == '.' || c == ' '
        then '-'
        else c

adjustLink = (filter (not . (== '/'))) . (map (toLower . replaceWithDash))

tagRoute :: Routes
tagRoute =
    setExtension ".html" `composeRoutes`
    gsubRoute "." adjustLink `composeRoutes`
        gsubRoute "/" (const "") `composeRoutes`
            gsubRoute "^tag" (const "tag/") `composeRoutes`
                gsubRoute "-html" (const "/index.html")

-- pagination

grouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
grouper ids = (liftM (paginateEvery 4) . sortRecentFirst) ids

makeId :: PageNumber -> Identifier
makeId pageNum = if pageNum == 1 then "index.html"
                 else fromFilePath $ "page/" ++ (show pageNum) ++ "/index.html"
