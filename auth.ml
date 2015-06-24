open Swt

module type Auth_intf = sig
  val secret : string
  val secure : bool
  val login_path : string
  val logout_path : string
  val authorized : (string * string) list -> bool
  val extras : ((string * string) list -> string) option
  val server : (module Server_intf)
end

exception Auth_error

let search_kvs key params = try
  let (_, v) = List.find (fun (k, _) -> key = k) params in v
with Not_found -> raise Auth_error

let gen_secret () =
  Random.self_init();
  String.init 25 (fun _ -> Char.chr ((Random.int 93) + 33)) (* ascii range *)

let default_impl ?(secure = false) ?(login_path = "/login")
  ?(server = (module DefaultServer : Server_intf)) ?(secret = gen_secret ())
  ?(logout_path = "/logout") ?extras ~authorized () =
  let impl : (module Auth_intf) = (module struct
    let secret = secret
    let secure = secure
    let login_path = login_path
    let logout_path = logout_path
    let authorized = authorized
    let extras = extras
    let server = server
  end) in
  impl

let split_kvs s =
  let kvs = Str.split (Str.regexp "&") s in
  let kvs = List.map (Str.split (Str.regexp "=")) kvs in
  List.map (function a::b::[] -> (a, b) | _ -> ("","")) kvs

let extras req = try
  let open Cohttp in
  let cookies = Cookie.Cookie_hdr.extract req.Request.headers in
  let token = search_kvs "a" cookies in
  let kvs = split_kvs token in
  Some (search_kvs "e" kvs)
with Auth_error -> None

module Make (M : Auth_intf)  = struct

  module HTTP = (val M.server : Server_intf)

  let gen_passwd =
    let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
    let len = String.length alphanum in
    function n ->
      let str = Bytes.create n in
      for i=0 to pred n do
        Bytes.set str i alphanum.[Random.int len]
      done;
      (str) |> Bytes.to_string

  let gen_mac t =
    let open Cryptokit in
    let hex = transform_string (Hexa.encode()) in
    hash_string (MAC.hmac_sha1 M.secret) t |> hex

  let _ = HTTP.post M.login_path begin fun env ->
    let open Cohttp in
      let req = Env.request env in
    let uri = Request.uri req in
      lwt body = Env.body env |> Cohttp_lwt_body.to_string in
  let params = Uri.query_of_encoded body in
  let params = List.map (fun (a, b) -> (a, (List.hd b))) params in
    let r1 = Uri.get_query_param uri "redir" in
    let r2 = Header.get req.Request.headers "Referer" in
    let redir = match r1, r2 with
    | Some r, _ -> r
    | None, None -> "/"
    | None, Some r -> begin
      match Uri.get_query_param (Uri.of_string r) "redir" with
      | None -> "/"
      | Some s -> s
    end in
  try
    if not (M.authorized params) then
      raise Auth_error
    else begin
      let t = gen_passwd 32 in
      let e = match M.extras with None -> "" | Some f -> f params in
      let qe = match e with "" -> "" | qe -> "&e=" ^ qe in
      let s = gen_mac (t ^ e) in
      let v = Printf.sprintf "t=%s&s=%s%s" t s qe in
      let cookie = Cohttp.Cookie.Set_cookie_hdr.make ~expiration:`Session ~secure:M.secure ~http_only:true ("a", v) in
      let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
      let headers = Cohttp.Header.init_with k v in
      CoSrv.respond_redirect ~headers ~uri:(Uri.of_string redir) ()
    end
  with Auth_error ->
    let path = M.login_path ^ "?redir=" ^ redir in
    CoSrv.respond_redirect (Uri.of_string path) ()
end

let _ = HTTP.post M.logout_path begin fun env ->
    let cookie = Cohttp.Cookie.Set_cookie_hdr.make
        ~expiration:(`Max_age 1L)
        ~secure:M.secure ~http_only:true ("a", "unset") in
    let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
    let headers = Cohttp.Header.init_with k v in
    CoSrv.respond_redirect ~headers ~uri:(Uri.of_string "/login") ()
  end

let auth = Middleware.create begin fun env m ->
    let open Cohttp in
    let req = Env.request env in
    let hdr = Header.remove req.Request.headers "swt-auth" in
    let cookies = Cookie.Cookie_hdr.extract hdr in
    let req_with_hdr h = Request.(make ~headers:h ~meth:req.meth
      ~version:req.version ~encoding:req.encoding req.uri) in
    env.Env.request <- req_with_hdr hdr;
    try
      let token = search_kvs "a" cookies in
      let kvs = split_kvs token in
      let tok = search_kvs "t" kvs in
      let sgn = search_kvs "s" kvs in
      let ext = try search_kvs "e" kvs with Auth_error -> "" in
      let mac = gen_mac (tok ^ ext) in
      if mac <> sgn then raise Auth_error else
        env.Env.request <- Header.add hdr "swt-auth" "ok" |> req_with_hdr;
        Middleware.call env m
    with Auth_error ->
      let uri = Request.uri req |> Uri.path in
      let redir = M.login_path in
      if uri = redir then Middleware.call env m else
        let redir = Printf.sprintf "%s?redir=%s" redir uri |> Uri.of_string in
        CoSrv.respond_redirect redir ()
  end

end
