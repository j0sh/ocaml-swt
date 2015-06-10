open Swt
open DefaultServer

let _ = get "/hello/:name" begin fun env ->
    let name = Env.param env "name" in
    let body = Printf.sprintf "Hola, %s!" name in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
end

module A = struct
    let secret = "el gato bebe leche"
    let secure = false
    let login_path = "/login"
    let authorized params =
        let username = Auth.search_kvs "username" params in
        let pass = Auth.search_kvs "password" params in
        username = "la llave" && pass = "open sesame"
    let server = (module DefaultServer : Server_intf)
end

let login_body path = "
<html>
  <body>
    <div>
        <p>Log In, Please</p>
        <form method='post' action='"^path^"' />
          <p>Username <input name='username' /></p>
          <p>Password <input name='password' type='password' /></p>
          <input type='submit' />
      </form>
    </div>
  </body>
</html>"

let _ = get A.login_path begin fun env ->
    let req = Env.request env in
    let uri = Cohttp.Request.uri req in
    let path = "/login" ^ match Uri.get_query_param uri "redir" with
        | None -> ""
        | Some r -> "?redir="^r in
    let body = login_body path in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
end

module M = Auth.Make(A)

let () = Lwt_main.run (run ~middleware:M.auth ())
