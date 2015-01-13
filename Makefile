all: srv

srv:
	ocamlfind ocamlc -c -package cohttp.lwt,lwt,str,fieldslib.syntax -syntax camlp4o -linkpkg server.mli auth.mli
	ocamlfind ocamlc -package cohttp.lwt,lwt,lwt.syntax,str,fieldslib.syntax,cryptokit -syntax camlp4o -linkpkg route_tree.ml server.ml auth.ml app.ml

clean:
	rm -rf a.out *.o *.cm[oxia]
