open Common

module Proxy = struct

  type t = {
    name: string [@main ];
    image: string [@default "nginx"];
    port: int [@default 8042 ];
    proxy_uri: string;
    htpasswd: string;
  } [@@deriving make]

  let name t = t.name

  (*
    - client_max_body_size:
    https://www.jamescoyle.net/how-to/1608-nginx-error-client-intended-to-send-too-large-body
    - proxy_read_timeout:
    http://howtounix.info/howto/110-connection-timed-out-error-in-nginx
  *)
  let to_service t =
    let shell_cmd =
      let open Genspio_edsl in
      let htpasswd_file =
        let path = "/setup/htpasswd" in
        let ensure =
          ensure ~name:"Create-htpasswd" (file_exists (string path)) [
            exec ["mkdir"; "-p"; "/setup"];
            write_stdout
              ~path:(string path)
              (string t.htpasswd >> exec ["cat"])
          ] in
        object method path = path method ensure = ensure end
      in
      let nginx_conf =
        let path = "/etc/nginx/conf.d/default.conf" in
        let ensure =
          ensure ~name:"Create-config" (bool false) [
            write_stdout
              ~path:(string path)
              (exec ["printf";
                     sprintf
                       "server {\\n\
                        client_max_body_size 50M;\\n\
                        listen %d;\\n\
                        location ~ {\\n\
                        auth_basic \"Restricted\";\\n\
                        auth_basic_user_file %s;\\n\
                        proxy_read_timeout 600;\\n\
                        proxy_pass %s;\\n\
                        }\\n\
                        }\\n"
                       t.port
                       htpasswd_file#path
                       t.proxy_uri
                    ;])
          ] in
        object method path = path method ensure = ensure end
      in
      seq [
        htpasswd_file#ensure;
        nginx_conf#ensure;
        sayf "Debug: %s" htpasswd_file#path;
        exec ["cat"; htpasswd_file#path];
        sayf "Debug: %s" nginx_conf#path;
        exec ["cat"; nginx_conf#path];
        exec ["nginx"; "-g"; "daemon off;"];
      ]
    in
    Docker_compose.Configuration.service t.name
      ~image:t.image
      ~start_up_script:(Genspio.Language.to_many_lines shell_cmd)


end
