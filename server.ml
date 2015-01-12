module Server = Cohttp_lwt_unix.Server

module Env = struct

     type t = {
        cxnid: Cohttp_lwt_unix.Server.conn;
        request: Cohttp.Request.t;
        body: Cohttp_lwt_body.t;
        params: (string * string) list;
        mutable resp_hdr: Cohttp.Header.t;
    } with fields

    let make ?(resp_hdr = Cohttp.Header.init ()) cxnid request body params =
        { cxnid; request; body; params; resp_hdr }

    let param env name =
        let p = params env in
        let f (k, _) = 0 = String.compare k name in
        let (_, v)  = List.find f p in v

end

type resp = (Server.Response.t * Cohttp_lwt_body.t) Lwt.t

let routes = Array.init 7 (fun _ -> Route_tree.new_node "")

let int_of_meth = function
    | `GET -> 0
    | `POST -> 1
    | `HEAD -> 2
    | `DELETE -> 3
    | `PATCH -> 4
    | `PUT -> 5
    | `OPTIONS -> 6
    | `Other _ -> 7

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
let other = register (`Other "")

let call meth conninfo req body =
    let table = routes.(int_of_meth meth) in
    let uri = Cohttp.Request.uri req |> Uri.path in
    match Route_tree.search uri table with
    | Some (Route_tree.Result ((Some fn), params)) ->
        let f (a, b) = (a, Uri.pct_decode b) in
        let params = List.map f params in
        begin try Env.make conninfo req body params |> fn
        with exn ->
            let body = Printexc.to_string exn in
            Server.respond_error ~body ()
        end
    | _ -> Server.respond_not_found ()

let make_server () =
    let callback conninfo req fbody =
        call (Cohttp.Request.meth req) conninfo req fbody in
    let conn_closed (ch, conn) = () in
    let config = Server.make ~callback ~conn_closed () in
    Server.create config

let run () =
    make_server () |> Lwt_main.run
