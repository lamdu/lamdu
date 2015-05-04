GHC=ghc
RUNGHC=run${GHC}
GHC_FLAGS=-O2 -Wall -Werror -XScopedTypeVariables
SRCDIRS=-i.. -i../bottlelib -i../submodules/AlgoW

all: run_test_infer

build: build_cabal build_test_stuff

build_test_stuff: build_test_infer

run_test_infer:
	${RUNGHC} ${SRCDIRS} test_Infer.hs

run_optimized_test_infer: build_test_infer
	./test_Infer

build_test_infer:
	${GHC} ${GHC_FLAGS} --make test_Infer ${SRCDIRS}

build_cabal:
	make -C ..
