module C = Configurator.V1

let () =
C.main ~name:"opencv4" (fun c ->
let default : C.Pkg_config.package_conf =
  { libs   = []
  ; cflags = []
  }
in
let conf =
  match C.Pkg_config.get c with
  | None -> default
  | Some pc ->
     match (C.Pkg_config.query pc ~package:"opencv4") with
     | None -> default
     | Some deps -> deps
in
(* Insert the actual configuration *)
C.Flags.write_sexp "cxx_flags.sexp" conf.cflags;
C.Flags.write_sexp "c_library_flags.sexp" conf.libs);
