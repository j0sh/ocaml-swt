module CoSrv = Cohttp_lwt_unix.Server

module Env : sig
    type t = {
        cxnid: CoSrv.conn;
        mutable request: Cohttp.Request.t;
        body: Cohttp_lwt_body.t;
        mutable params: (string * string) list;
    } with fields

    val make : ?params:(string * string) list -> CoSrv.conn ->
        Cohttp.Request.t -> Cohttp_lwt_body.t -> t

    val param : t -> string -> string

end

type resp = (Cohttp_lwt_unix.Response.t * Cohttp_lwt_body.t) Lwt.t

module Middleware : sig
    type t
    val create : (Env.t -> t -> resp) -> t
    val add : (Env.t -> t -> resp) -> t -> t
    val call : Env.t -> t -> resp
    val chain : t -> t -> t
end

module type Swt_intf = sig
end

module type Server_intf = sig

val get : string -> (Env.t -> resp) -> unit
val post: string -> (Env.t -> resp) -> unit
val head: string -> (Env.t -> resp) -> unit
val put : string -> (Env.t -> resp) -> unit
val delete: string -> (Env.t -> resp) -> unit
val patch: string -> (Env.t -> resp) -> unit
val put: string -> (Env.t -> resp) -> unit
val options: string -> (Env.t -> resp) -> unit
val other: string -> (Env.t -> resp) -> unit

end

module MakeServer(M:Swt_intf) : sig

include Server_intf

val run : ?port:int -> ?middleware:Middleware.t -> unit -> unit Lwt.t

end

module DefaultIntf   : Swt_intf
module DefaultServer : module type of MakeServer(DefaultIntf)
