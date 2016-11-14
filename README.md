# torcontrol

%%VERSION%%

This library is intended to help an application utilize the features of the tor control port, e. g. setting up onion services or resolving hosts via Tor.

# Building

    ocaml pkg/pkg.ml build

To play around with the library, do `#use "top.ml"` in the ocaml repl.

# Documentation

* [Library documentation](https://sternenseemann.github.io/torcontrol/doc) (I will improve it, promise!).
* [Tor's specification](https://gitweb.torproject.org/torspec.git/tree/), especially relevant:
    * [control-spec.txt](https://gitweb.torproject.org/torspec.git/tree/control-spec.txt)
    * [rend-spec.txt](https://gitweb.torproject.org/torspec.git/tree/rend-spec.txt)
    * [tor-spec.txt](https://gitweb.torproject.org/torspec.git/tree/tor-spec.txt)
