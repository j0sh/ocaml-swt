module Server = Cohttp_lwt_unix.Server

module Env = struct

  type t = {
    cxnid: Cohttp_lwt_unix.Server.conn;
    mutable request: Cohttp.Request.t;
    body: Cohttp_lwt_body.t;
    mutable params: (string * string) list;
  } with fields

  let make ?(params = []) cxnid request body =
    { cxnid; request; body; params; }

  let param env name =
    let p = params env in
    let f (k, _) = 0 = String.compare k name in
    let (_, v)  = List.find f p in v

end

type resp = (Server.Response.t * Cohttp_lwt_body.t) Lwt.t

module Middleware = struct
  type t = MW of (Env.t -> t -> resp) list
  let return mw = MW mw
  let empty = return []
  let add handler (MW mw) = return (handler :: mw)
  let create handler = add handler empty
  let call env = function
    | MW [] -> Server.respond_not_found ()
    | MW (h::t) -> h env (return t)
  let prepare (MW mw) = return (List.rev mw)
  let chain (MW a) (MW b) = return (b @ a)
end

let routes = Array.init 10 (fun _ -> Route_tree.new_node "")

let int_of_meth = function
  | `GET -> 0
  | `POST -> 1
  | `HEAD -> 2
  | `DELETE -> 3
  | `PATCH -> 4
  | `PUT -> 5
  | `OPTIONS -> 6
  | `CONNECT -> 7
  | `TRACE -> 8
  | `Other _ -> 9

let register meth route handler =
  let m = int_of_meth meth in
  routes.(m) <- Route_tree.add_node route handler routes.(m)

let get = register `GET
let post = register `POST
let head = register `HEAD
let delete = register `DELETE
let patch =  register `PATCH
let put = register `PUT
let options = register `OPTIONS
let connect = register `CONNECT
let trace = register `TRACE
let other = register (`Other "")

let dispatcher = Middleware.create begin fun env m ->
    let req = Env.request env in
    let meth = Cohttp.Request.meth req in
    let table = routes.(int_of_meth meth) in
    let uri = Cohttp.Request.uri req |> Uri.path in
    match Route_tree.search uri table with
    | Some (Route_tree.Result ((Some fn), params)) ->
      let f (a, b) = (a, Uri.pct_decode b) in
      List.map f params |> Env.set_params env;
      fn env
    | _ -> begin try
          (* todo: branch to middleware that properly handles files, dirs, &c *)
          let uri = Cohttp.Request.uri req in
          let fname = Server.resolve_local_file ~docroot:"static" ~uri in
          Server.respond_file ~fname ()
        with _ -> Server.respond_not_found ()
      end
  end

let exn_handler = Middleware.create begin fun env m ->
    let res = try Middleware.call env m with exn ->
      let body = Printexc.to_string exn in
      Server.respond_error ~body () in
    flush stdout;
    res
  end

let make_server port middleware =
  let middleware = Middleware.prepare middleware in
  let callback conninfo req body =
    let env = Env.make conninfo req body in
    Middleware.call env middleware in
  let conn_closed (ch, conn) = () in
  let config = Server.make ~callback ~conn_closed () in
  let mode = `TCP (`Port port) in
  Server.create ~mode config

let run ?(port = 8080) ?(middleware = Middleware.empty) () =
  let (@@) = Middleware.chain in
  let middleware = exn_handler @@ middleware @@ dispatcher in
  make_server port middleware
