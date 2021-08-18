# This is excessive and I accept that
for GHC_VERSION in 8.6.1 8.6.2 8.6.3 8.6.4 6.5 8.8.1 8.8.2 8.8.3 8.8.4 8.10.1 8.10.2 8.10.3 8.10.4 8.10.5 9.0.1 
do
  ghcup install "$GHC_VERSION"
  ghcup set ghc "$GHC_VERSION"
  cabal build
done


