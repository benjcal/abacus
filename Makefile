all:
	dune build

clean:
	dune clean

run: all
	dune exec src/main.exe

watch:
	dune build --watch