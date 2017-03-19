{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid ((<>))
import Data.Version (showVersion)
import Data.Word
import Development.GitRev (gitHash)
import Options.Applicative
import System.Random.PCG
import Paths_miro (version)

import Miro.Option

main :: IO ()
main = do
  randSeed <- mkRngSeed
  opts <- sanitizeOpts =<< execParser (optsParser randSeed)
  generateMaze opts
  where
  optsParser randSeed = info
    ( helper
    <*> versionOption
    <*> programOptions randSeed
    )
    ( fullDesc
    <> header "miro - a maze generator"
    )
  versionOption = infoOption
    (concat [showVersion version, " ", $(gitHash)])
    (long "version" <> help "Show version")
  programOptions randSeed
    = Opts
    <$> optMazeTypeParser
    <*> optOutputParser
    <*> optSvgFilenameParser
    <*> optSvgWidthParser
    <*> optSeedParser randSeed
    <*> optSizeParser
    <*> optQuietParser
  mkRngSeed = do
    -- FIXME: Because pcg-random 0.1.3.4 does not export the FrozenGen data
    -- constructor, we cannot examine it easily (we could do stuff with its
    -- Storable typeclass functions, but that's quite painful). For now, we
    -- instantiate a throwaway RNG to generate 2 random Word64s, which are
    -- then used for the real RNG. Later when pcg-random fixes this, we can do
    -- away with this layer of indirection.
    rng <- createSystemRandom
    s1 <- uniform rng :: IO Word64
    s2 <- uniform rng :: IO Word64
    return (s1, s2)
