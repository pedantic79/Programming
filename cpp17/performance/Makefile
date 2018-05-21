default:
	mkdir -p build
	(cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES --build ..)

clean:
	rm -rf build

tidy: default
	clang-tidy -checks=cppcoreguidelines-*,cert-*,bugprone-*,llvm-*,misc-*,modernize-*,performance-*,portability-*,readability-* -p build *.cpp
