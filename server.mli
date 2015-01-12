module Server = Cohttp_lwt_unix.Server

module Env : sig
    type t = {
        cxnid: Server.conn;
        request: Cohttp.Request.t;
        body: Cohttp_lwt_body.t;
        params: (string * string) list;
        mutable resp_hdr: Cohttp.Header.t;
    } with fields

    val make : ?resp_hdr:Cohttp.Header.t -> Server.conn ->
        Cohttp.Request.t -> Cohttp_lwt_body.t -> (string * string) list -> t

    val param : t -> string -> string

end

type resp = (Server.Response.t * Cohttp_lwt_body.t) Lwt.t

val get : string -> (Env.t -> resp) -> unit
val post: string -> (Env.t -> resp) -> unit
val head: string -> (Env.t -> resp) -> unit
val put : string -> (Env.t -> resp) -> unit
val delete: string -> (Env.t -> resp) -> unit
val patch: string -> (Env.t -> resp) -> unit
val put: string -> (Env.t -> resp) -> unit
val options: string -> (Env.t -> resp) -> unit
val other: string -> (Env.t -> resp) -> unit

val run : unit -> unit
