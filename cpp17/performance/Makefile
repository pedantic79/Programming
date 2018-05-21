default:
	mkdir -p build
	(cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES --build ..)

clean:
	rm -rf build

tidy: default
	clang-tidy -p build *.cpp
