open Server

let _ = get "/hello/:name" begin fun env ->
    let name = Env.param env "name" in
    let body = Printf.sprintf "Hola, %s!" name in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
end

let () = run ()
