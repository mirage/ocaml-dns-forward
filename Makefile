TESTS = true

.PHONY: all clean test bundle COMMIT exe

all: dns-forwarder
	@

dns-forwarder:
	ocaml pkg/pkg.ml build --tests $(TESTS) -q

clean:
	ocaml pkg/pkg.ml clean

test:
	ocaml pkg/pkg.ml build --tests true
	ocaml pkg/pkg.ml test
