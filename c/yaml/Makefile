# Run 'make deps' first to get the libyaml dependency and build it.
# Then run 'make all' to build the main executable.
# Then run './build/yamltest ./config{1,2,3}.yaml to test.

all:
	mkdir -p build
	clang -I./deps/libyaml/include -g -o build/main.o -c src/main.c
	clang -g -o build/yamltest build/main.o ./deps/libyaml/src/.libs/libyaml.a

deps:
	mkdir -p deps
	wget https://bitbucket.org/xi/libyaml/get/2b9156756423.zip
	unzip 2b9156756423.zip
	mv xi-libyaml-2b9156756423 ./deps/libyaml
	rm -rf 2b9156756423.zip
	./deps/libyaml/bootstrap
	./deps/libyaml/configure
	make

clean:
	rm -rf build
