open Server

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
end

let _ = get A.login_path begin fun env ->
    let body = "Please Log In" in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
end

module M = Auth.Make(A)

let () = run ~middleware:M.auth ()
