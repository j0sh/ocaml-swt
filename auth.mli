module type Auth_int = sig
    val secret : string
    val secure : bool
    val login_path : string
    val authorized : (string * string) list -> bool
    val server : (module Swt.Server_intf)
end

module Make (M : Auth_int)  : sig

    val auth : Swt.Middleware.t

end

val search_kvs : string -> (string * string) list -> string
