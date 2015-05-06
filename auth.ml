open Swt

module type Auth_int = sig
  val secret : string
  val secure : bool
  val login_path : string
  val authorized : (string * string) list -> bool
end

exception Auth_error

let search_kvs key params =
  try
    let (_, v) = List.find (fun (k, _) -> key = k) params in v
  with Not_found -> raise Auth_error

module Make (M : Auth_int)  = struct

  let split_kvs s =
    let kvs = Str.split (Str.regexp "&") s in
    let kvs = List.map (Str.split (Str.regexp "=")) kvs in
    List.map (function a::b::[] -> (a, b) | _ -> ("","")) kvs

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

  let _ = post M.login_path begin fun env ->
      let req = Env.request env in
      let uri = Cohttp.Request.uri req in
      lwt body = Env.body env |> Cohttp_lwt_body.to_string in
  let params = Uri.query_of_encoded body in
  let params = List.map (fun (a, b) -> (a, (List.hd b))) params in
  let redir = match Uri.get_query_param uri "redir" with
    | None -> "/"
    | Some s -> s in
  try
    if not (M.authorized params) then
      raise Auth_error
    else begin
      let t = gen_passwd 32 in
      let s = gen_mac t in
      let v = Printf.sprintf "t=%s&s=%s" t s in
      let cookie = Cohttp.Cookie.Set_cookie_hdr.make ~expiration:`Session ~secure:M.secure ~http_only:true ("a", v) in
      let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
      let headers = Cohttp.Header.init_with k v in
      Server.respond_redirect ~headers ~uri:(Uri.of_string redir) ()
    end
  with Auth_error ->
    let path = M.login_path ^ "?redir=" ^ redir in
    Server.respond_redirect (Uri.of_string path) ()
end

let _ = post "/logout" begin fun env ->
    let cookie = Cohttp.Cookie.Set_cookie_hdr.make
        ~expiration:(`Max_age 1L)
        ~secure:M.secure ~http_only:true ("a", "unset") in
    let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize cookie in
    let headers = Cohttp.Header.init_with k v in
    Server.respond_redirect ~headers ~uri:(Uri.of_string "/login") ()
  end

let auth = Middleware.create begin fun env m ->
    let req = Env.request env in
    let hdr = Cohttp.Header.remove req.Cohttp.Request.headers "swt-auth" in
    let cookies = Cohttp.Cookie.Cookie_hdr.extract hdr in
    let req_with_hdr h = Cohttp.Request.(make ~headers:h ~meth:req.meth
      ~version:req.version ~encoding:req.encoding req.uri) in
    env.Env.request <- req_with_hdr hdr;
    try
      let token = search_kvs "a" cookies in
      let kvs = split_kvs token in
      let tok = search_kvs "t" kvs in
      let sgn = search_kvs "s" kvs in
      let mac = gen_mac tok in
      if mac <> sgn then raise Auth_error else
        env.Env.request <- Cohttp.Header.add hdr "swt-auth" "ok" |> req_with_hdr;
        Middleware.call env m
    with Auth_error ->
      let uri = Cohttp.Request.uri req |> Uri.path in
      let redir = M.login_path in
      if uri = redir then Middleware.call env m else
        let redir = Printf.sprintf "%s?redir=%s" redir uri |> Uri.of_string in
        Server.respond_redirect redir ()
  end

end
