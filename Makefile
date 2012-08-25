CABAL = cabal-dev

default: build

build: configure
	$(CABAL) build

configure: Shipping.cabal install_dependencies
	$(CABAL) configure

configure_tests: Shipping.cabal install_test_dependencies
	$(CABAL) configure --enable-tests

install_test_dependencies: Shipping.cabal
	$(CABAL) install --enable-tests --only-dependencies 

install_dependencies: Shipping.cabal
	$(CABAL) install --only-dependencies

spec: configure_tests
	PATH=$$PATH:cabal-dev/bin $(CABAL) build > /dev/null
	$(CABAL) test
