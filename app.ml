open Swt
open DefaultServer

let _ = get "/hello/:name" begin fun env ->
    let name = Env.param env "name" in
    let body = Printf.sprintf "Hola, %s!" name in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
end

let authorized params =
  let username = Auth.search_kvs "username" params in
  let pass = Auth.search_kvs "password" params in
  username = "la llave" && pass = "open sesame"

let auth = Auth.default_impl ~secret:"el gato bebe leche" ~authorized ()

module A = (val auth : Auth.Auth_intf)

let login_body = "
<html>
  <body>
    <div>
        <p>Log In, Please</p>
        <form method='post' action='" ^ A.login_path ^ "'>
          <p>Username <input name='username' /></p>
          <p>Password <input name='password' type='password' /></p>
          <input type='submit' />
      </form>
    </div>
  </body>
</html>"

let _ = get A.login_path begin fun env ->
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:login_body ()
end

module M = Auth.Make(A)

let () = Lwt_main.run (run ~middleware:M.auth ())
