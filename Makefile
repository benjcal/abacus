all:
	dune build

clean:
	dune clean

run:
	dune exec src/oadder.exe