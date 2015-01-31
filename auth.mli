module type Auth_int = sig
    val secret : string
    val secure : bool
    val login_path : string
    val authorized : (string * string) list -> bool
end

module Make (M : Auth_int)  : sig

    val auth : Swt.Middleware.t

end

val search_kvs : string -> (string * string) list -> string
