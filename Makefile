all:
	cabal build --ghc-options "-XScopedTypeVariables -Werror"

conf:
	cabal configure --disable-library-profiling --disable-executable-profiling

verify_config:
	runghc -ibottlelib VerifyConfig.hs
