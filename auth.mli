module type Auth_intf = sig
    val secret : string
    val secure : bool
    val login_path : string
    val logout_path : string
    val authorized : (string * string) list -> bool Lwt.t
    val extras : ((string * string) list -> string Lwt.t) option
    val server : (module Swt.Server_intf)
end

module type Auth = sig
  val auth : Swt.Middleware.t

  (* Utility function to check validity of a request without actually having
  * to call into middleware. Useful if the request needs to be validated out of
  * band for whatever reason. *)
  val valid : Cohttp.Request.t -> bool

  (* Generates a cookie header. Useful to force a login out of band. *)
  (* Takes a list of (key, value) parameters to be checked against.  *)
  val authorize : (string * string) list -> Cohttp.Header.t Lwt.t
end

module Make (M : Auth_intf)  : Auth

(* NOTE this function is not pure; if [secret] is left as default, the state
 *      of Random is reset by calling Random.self_init () *)
val default_impl : ?secure:bool -> ?login_path:string ->
  ?server:(module Swt.Server_intf) -> ?secret:string ->
  ?logout_path:string -> ?extras:((string * string) list -> string Lwt.t) ->
  ?seed:int array -> authorized:((string * string) list -> bool Lwt.t) -> unit ->
  (module Auth_intf)

val search_kvs : string -> (string * string) list -> string

val extras: Cohttp.Request.t -> string option
