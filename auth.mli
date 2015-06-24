module type Auth_intf = sig
    val secret : string
    val secure : bool
    val login_path : string
    val authorized : (string * string) list -> bool
    val extras : ((string * string) list -> string) option
    val server : (module Swt.Server_intf)
end

module Make (M : Auth_intf)  : sig

    val auth : Swt.Middleware.t

end

(* NOTE this function is not pure; if [secret] is left as default, the state
 *      of Random is reset by calling Random.self_init () *)
val default_impl : ?secure:bool -> ?login_path:string ->
  ?server:(module Swt.Server_intf) -> ?secret:string ->
  ?extras:((string * string) list -> string) ->
  authorized:((string * string) list -> bool) -> unit -> (module Auth_intf)

val search_kvs : string -> (string * string) list -> string

val extras: Cohttp.Request.t -> string option
