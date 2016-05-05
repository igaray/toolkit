# Run 'make deps' first to get the libyaml dependency and build it.
# Then run 'make all' to build the main executable.
# Then run './build/yamltest ./config{1,2,3}.yaml to test.

all:
	mkdir -p build
	clang -I./deps/libyaml/include -o build/main.o -c src/main.c 
	clang -o build/yamltest build/main.o ./deps/libyaml/src/.libs/libyaml.a

deps:
	mkdir deps
	cd deps
	wget https://bitbucket.org/xi/libyaml/get/2b9156756423.zip
	unzip 2b9156756423.zip
	mv xi-libyaml-2b9156756423 libyaml
	cd libyaml
	./bootstrap
	./configure
	make

clean:
	rm -rf build
