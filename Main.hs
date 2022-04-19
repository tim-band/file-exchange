{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (handle)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as Bs
import qualified Data.ByteString.Char8 as Bc
import Data.Char (isDigit)
import Data.List (sort)
import Data.Time.Clock (UTCTime)
import qualified Network.URI.Encode as Uri
import qualified Network.HTTP.Types.Status as Nst
import qualified Network.Wai.Parse as Wp
import Options.Applicative
import System.Directory (getModificationTime, listDirectory, doesFileExist)
import System.FilePath ((</>), splitExtension)
import qualified Text.Blaze.Html4.Strict as Ht
import qualified Text.Blaze.Html4.Strict.Attributes as Hta
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Web.Scotty as Sc

data Opts = Opts Int FilePath

opts :: ParserInfo Opts
opts = info ((Opts
  <$> option auto (short 'p' <> help "Port to listen on" <> value 3000)
  <*> argument str (metavar "DIRECTORY" <> help "Directory to serve")) <**> helper
  ) (fullDesc
    <> progDesc "Serves up an HTTP interface to a directory's contents"
    <> header "File exchange server"
  )

addModificationTimes :: FilePath -> [FilePath] -> IO [(UTCTime, FilePath)]
addModificationTimes root paths = amt paths where
  amt [] = return []
  amt (p:ps) = let
    path = root </> p
    amte :: IOError -> IO [(UTCTime, FilePath)]
    amte _ = amt ps in
      handle amte $ do
        isfile <- doesFileExist path
        if isfile
          then do
            t <- getModificationTime path
            rs <- amt ps
            return ((t, p) : rs)
          else amt ps

uniquify :: FilePath -> IO FilePath
uniquify p = let
  (nm, ext) = splitExtension p
  -- remove (ddd) from the end of the name
  name = case reverse nm of
    [] -> []
    (')':s) -> case dropWhile isDigit s of
      [] -> nm
      ('(':r) -> reverse r
      _ -> nm
    _ -> nm
  fileNames = (name ++ ext) : [name ++ "(" ++ show d ++ ")" ++ ext | d <- [1..] :: [Int]]
  available [] = error "should have been an infinite list"
  available (n:ns) = do
    e <- doesFileExist n
    if e then available ns else return n
  in available fileNames

main :: IO ()
main = do
  Opts port dir <- execParser opts
  Sc.scotty port $ do
  Sc.get "/download/:filename" $ do
    filename <-Sc.param "filename"
    Sc.setHeader "content-type" "application/octet-stream"
    Sc.file (dir </> filename)
  Sc.post "/upload" $ do
    fs <- Sc.files
    let fs' = [ (Bc.unpack (Wp.fileName fi), Wp.fileContent fi) | (fieldName, fi) <- fs, fieldName == "file" ]
    case fs' of
      [(fn, fc)] -> if fn == "\"\"" && fc == Bs.empty
        then Sc.redirect "/" -- no file to upload
        else let path = dir </> fn in liftIO $ do
          path' <- uniquify path
          Bs.writeFile path' fc
      [] -> Sc.raiseStatus Nst.badRequest400 "No file supplied"
      (_:_:_) -> Sc.raiseStatus Nst.badRequest400 "Too many files supplied"
    Sc.redirect "/"
  Sc.get "/" $ do
    ps <- Sc.liftAndCatchIO $ do
      fps <- listDirectory dir
      ps' <- addModificationTimes dir fps
      return $ sort ps'
    Sc.html $ renderMarkup $ Ht.html $ Ht.body $ do
      Ht.form Ht.! Hta.method "post" Ht.! Hta.enctype "multipart/form-data" Ht.! Hta.action "/upload" $ do
        Ht.input Ht.! Hta.type_ "file" Ht.! Hta.name "file"
        Ht.br
        Ht.input Ht.! Hta.type_ "submit" Ht.! Hta.value "Upload"
      Ht.h1 $ Ht.string "Available files"
      Ht.table $ do
        Ht.thead $ Ht.tr $ do
          Ht.th "File"
          Ht.th "Date"
        Ht.tbody $ forM_ ps $ \(t, fp) -> do
          Ht.tr $ do
            Ht.td $ Ht.a Ht.! Hta.href (Ht.stringValue ("/download/" ++ Uri.encode fp)) $ Ht.string fp
            Ht.td $ Ht.string $ show t
