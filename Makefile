./bin/InHs-exe:
	stack setup
	stack build
	stack install --local-bin-path ./bin InHs

test: clean
	stack build
	stack test
	stack install --local-bin-path ./bin InHs

setup: ./bin/InHs-exe

clean:
	rm -rf ./bin
