all: build

build:
	ocamlbuild -use-ocamlfind -plugin-tags "package(eliom.ocamlbuild)" \
                                  server/passpartout.cma server/passpartout.cmxa server/passpartout.cmxs \
                                  client/passpartout.js -use-menhir

run: build
	mkdir -p _run/log/passpartout/
	mkdir -p _run/data/passpartout/ocsipersist
	mkdir -p _run/upload
	ocsigenserver -c passpartout.conf -v

clean:
	rm -rf _build
