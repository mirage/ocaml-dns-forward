
module Error = Dns_forward.Error.Infix

let fresh_id =
  let next = ref 1000 in
  fun () ->
    let this = !next in
    next := !next mod 0xffff;
    this

let make_a_query name =
  let open Dns.Packet in
  let id = fresh_id () in
  let detail = { qr = Query; opcode = Standard; aa = true; tc = false; rd = true; ra = false; rcode = NoError } in
  let questions = [ make_question Q_A name ] in
  let answers = [] in
  let authorities = [] in
  let additionals = [] in
  let pkt = { id; detail; questions; answers; authorities; additionals } in
  let buf = Dns.Buf.create 1024 in
  let buf = marshal buf pkt in
  Cstruct.of_bigarray buf

let parse_response response =
  let pkt = Dns.Packet.parse (Cstruct.to_bigarray response) in
  match pkt.Dns.Packet.detail with
  | { Dns.Packet.qr = Dns.Packet.Query; _ } ->
    Lwt.return (Result.Error (`Msg "parsed a response which was actually a query in disguise"))
  | { Dns.Packet.qr = Dns.Packet.Response; _ } ->
    begin match pkt.Dns.Packet.answers with
    | [ { Dns.Packet.rdata = Dns.Packet.A ipv4; _ } ] ->
      Lwt.return (Result.Ok ipv4)
    | xs -> Lwt.return (Result.Error (`Msg (Printf.sprintf "failed to find answers: [ %s ]" (String.concat "; " (List.map Dns.Packet.rr_to_string xs)))))
    end

let test_server () =
  match Lwt_main.run begin
    let module S = Server.Make(Rpc) in
    let s = S.make [ "foo", Ipaddr.V4 Ipaddr.V4.localhost; "bar", Ipaddr.of_string_exn "1.2.3.4" ] in
    let open Error in
    (* The virtual address we run our server on: *)
    let address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 53 } in
    S.serve ~address s
    >>= fun _ ->
    let expected_dst = ref false in
    let message_cb ?src:_ ?dst:d ~buf:_ () =
      ( match d with
        | Some d ->
          if Dns_forward.Config.Address.compare address d = 0 then expected_dst := true
        | None ->
          ()
      );
      Lwt.return_unit
    in
    Rpc.connect ~message_cb address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    Rpc.rpc c request
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" "127.0.0.1" (Ipaddr.V4.to_string ipv4);
    let open Lwt.Infix in
    Rpc.disconnect c
    >>= fun () ->
    if not (!expected_dst) then failwith ("Expected destination address never seen in message_cb");
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  | Result.Error (`Msg m) -> failwith m

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end

let test_local_lookups () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  match Lwt_main.run begin
    let module S = Server.Make(Rpc) in
    let foo_public = "8.8.8.8" in
    let foo_private = "192.168.1.1" in
    (* a public server mapping 'foo' to a public ip *)
    let public_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
    let public_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 4 } in
    let open Error in
    S.serve ~address:public_address public_server
    >>= fun _ ->
    let module R = Dns_forward.Resolver.Make(Rpc)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = public_address; zones = Domain.Set.empty; timeout_ms = None; order = 0 };
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    let local_names_cb question =
      let open Dns.Packet in
      match question with
      | { q_name; q_type = Q_A; _ } ->
        let rdata = A (Ipaddr.V4.of_string_exn foo_private) in
        let name = q_name and cls = RR_IN and flush = false and ttl = 100l in
        Lwt.return (Some [ { name; cls; flush; ttl; rdata } ])
      | _ ->
        Lwt.return None in
    R.create ~local_names_cb config
    >>= fun r ->
    let module F = Dns_forward.Server.Make(Rpc)(R) in
    F.create r
    >>= fun f ->
    let f_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 5 } in
    let open Error in
    F.serve ~address:f_address f
    >>= fun () ->
    Rpc.connect f_address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    Rpc.rpc c request
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" foo_private (Ipaddr.V4.to_string ipv4);
    let open Lwt.Infix in
    Rpc.disconnect c
    >>= fun () ->
    F.destroy f
    >>= fun () ->
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  | Result.Error (`Msg m) -> failwith m

let test_tcp_multiplexing () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  match Lwt_main.run begin
    let module Proto_server = Dns_forward.Rpc.Server.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
    let module Proto_client = Dns_forward.Rpc.Client.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
    let module S = Server.Make(Proto_server) in
    let foo_public = "8.8.8.8" in
    (* a public server mapping 'foo' to a public ip *)
    let public_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
    let public_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 6 } in
    let open Error in
    S.serve ~address:public_address public_server
    >>= fun _ ->
    let module R = Dns_forward.Resolver.Make(Proto_client)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = public_address; zones = Domain.Set.empty; timeout_ms = None; order = 0 };
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    R.create config
    >>= fun r ->
    let module F = Dns_forward.Server.Make(Proto_server)(R) in
    F.create r
    >>= fun f ->
    let f_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 7 } in
    let open Error in
    F.serve ~address:f_address f
    >>= fun () ->
    let expected_dst = ref false in
    let message_cb ?src:_ ?dst ~buf:_ () =
      ( match dst with
        | Some d ->
          if Dns_forward.Config.Address.compare f_address d = 0 then expected_dst := true
        | None ->
          ()
      );
      Lwt.return_unit
    in
    Proto_client.connect ~message_cb f_address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    let send_request () =
      Proto_client.rpc c request
      >>= fun response ->
      (* Check the response has the correct transaction id *)
      let request' = Dns.Packet.parse (Cstruct.to_bigarray request)
      and response' = Dns.Packet.parse (Cstruct.to_bigarray response) in
      Alcotest.(check int) "DNS.id" request'.Dns.Packet.id response'.Dns.Packet.id;
      parse_response response
      >>= fun ipv4 ->
      Alcotest.(check string) "IPv4" foo_public (Ipaddr.V4.to_string ipv4);
      Lwt.return (Result.Ok ()) in
    let rec seq f = function
      | 0 -> Lwt.return (Result.Ok ())
      | n ->
        f ()
        >>= fun () ->
        seq f (n - 1) in
    let rec par f = function
      | 0 -> Lwt.return (Result.Ok ())
      | n ->
        let first = f () in
        let rest = par f (n - 1) in
        first
        >>= fun () ->
        rest in
    (* Run 5 threads each sending 100 requests *)
    par (fun () -> seq send_request 100) 5
    >>= fun () ->
    let open Lwt.Infix in
    Proto_client.disconnect c
    >>= fun () ->
    F.destroy f
    >>= fun () ->
    if not (!expected_dst) then failwith ("Expected destination address never seen in message_cb");
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  | Result.Error (`Msg m) -> failwith m

let test_timeout () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  let module Proto_server = Dns_forward.Rpc.Server.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module Proto_client = Dns_forward.Rpc.Client.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module S = Server.Make(Proto_server) in
  let foo_public = "8.8.8.8" in
  (* a public server mapping 'foo' to a public ip *)
  let bar_server = S.make ~delay:60. [ "foo", Ipaddr.of_string_exn foo_public ] in
  let bar_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 8 } in

  let open Error in
  match Lwt_main.run begin
    S.serve ~address:bar_address bar_server
    >>= fun _ ->
    (* a resolver which uses both servers *)
    let module R = Dns_forward.Resolver.Make(Proto_client)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = bar_address; zones = Domain.Set.empty; timeout_ms = Some 0; order = 0 }
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    R.create config
    >>= fun r ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    let request =
      R.answer request r
      >>= function
      | Result.Ok _ -> failwith "timeout test expected a timeout"
      | Result.Error _ -> Lwt.return true in
    let timeout =
      Lwt_unix.sleep 5.
      >>= fun () ->
      Lwt.return false in
    Lwt.pick [ request; timeout ]
    >>= fun ok ->
    if not ok then failwith "server timeout was not respected";
    R.destroy r
    >>= fun () ->
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    (* the disconnects and close should have removed all the connections: *)
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
    Alcotest.(check int) "bar_server queries" 1 (S.get_nr_queries bar_server);
  | Result.Error (`Msg m) -> failwith m

let test_cache () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  let module Proto_server = Dns_forward.Rpc.Server.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module Proto_client = Dns_forward.Rpc.Client.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module S = Server.Make(Proto_server) in
  let foo_public = "8.8.8.8" in
  (* a public server mapping 'foo' to a public ip *)
  let bar_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
  let bar_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 12 } in

  let open Error in
  match Lwt_main.run begin
    S.serve ~address:bar_address bar_server
    >>= fun server ->
    (* a resolver which uses both servers *)
    let module R = Dns_forward.Resolver.Make(Proto_client)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = bar_address; zones = Domain.Set.empty; timeout_ms = Some 1000; order = 0 }
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    R.create config
    >>= fun r ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    R.answer request r
    >>= function
    | Result.Error _ -> failwith "failed initial lookup"
    | Result.Ok _ ->
    S.shutdown server
    >>= fun () ->
    R.answer request r
    >>= function
    | Result.Error (`Msg m) -> failwith ("failed cached lookup: " ^ m)
    | Result.Ok _ ->
    R.destroy r
    >>= fun () ->
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    (* the disconnects and close should have removed all the connections: *)
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
    Alcotest.(check int) "bar_server queries" 1 (S.get_nr_queries bar_server);
  | Result.Error (`Msg m) -> failwith m

(* One slow private server, one fast public server with different bindings for
   the same name. The order field guarantees that we take the answer from the
   slow private server. *)
let test_order () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  let module Proto_server = Dns_forward.Rpc.Server.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module Proto_client = Dns_forward.Rpc.Client.Make(Flow)(Dns_forward.Framing.Tcp(Flow))(Time) in
  let module S = Server.Make(Proto_server) in
  let foo_public = "8.8.8.8" in
  let foo_private = "192.168.1.1" in
  (* a public server mapping 'foo' to a public ip *)
  let public_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
  let public_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 10 } in
  (* a private server mapping 'foo' to a private ip *)
  let private_server = S.make [ "foo", Ipaddr.of_string_exn foo_private ] in
  let private_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 11 } in

  let open Error in
  match Lwt_main.run begin
    S.serve ~address:public_address public_server
    >>= fun _ ->
    S.serve ~address:private_address private_server
    >>= fun _ ->

    (* a resolver which uses both servers *)
    let module R = Dns_forward.Resolver.Make(Proto_client)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = public_address; zones = Domain.Set.empty; timeout_ms = None; order = 1 };
      { Server.address = private_address; zones = Domain.Set.empty; timeout_ms = None; order = 0 }
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    R.create config
    >>= fun r ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    let open Error in
    R.answer request r
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" foo_private (Ipaddr.V4.to_string ipv4);
    let open Lwt.Infix in
    R.destroy r
    >>= fun () ->
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    (* the disconnects and close should have removed all the connections: *)
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
    Alcotest.(check int) "private_server queries" 1 (S.get_nr_queries private_server);
    Alcotest.(check int) "public_server queries" 0 (S.get_nr_queries public_server);
  | Result.Error (`Msg m) -> failwith m

let test_forwarder_zone () =
  Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
  let module S = Server.Make(Rpc) in
  let foo_public = "8.8.8.8" in
  let foo_private = "192.168.1.1" in
  (* a VPN mapping 'foo' to an internal ip *)
  let foo_server = S.make [ "foo", Ipaddr.of_string_exn foo_private ] in
  let foo_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 1 } in
  (* a public server mapping 'foo' to a public ip *)
  let bar_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
  let bar_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 2 } in

  let open Error in
  match Lwt_main.run begin
    S.serve ~address:foo_address foo_server
    >>= fun _ ->

    S.serve ~address:bar_address bar_server
    >>= fun _ ->
    (* a resolver which uses both servers *)
    let module R = Dns_forward.Resolver.Make(Rpc)(Time) in
    let open Dns_forward.Config in
    let servers = Server.Set.of_list [
      { Server.address = foo_address; zones = Domain.Set.add [ "foo" ] Domain.Set.empty; timeout_ms = None; order = 0 };
      { Server.address = bar_address; zones = Domain.Set.empty; timeout_ms = None; order = 0 }
    ] in
    let config = { servers; search = [] } in
    let open Lwt.Infix in
    R.create config
    >>= fun r ->
    let module F = Dns_forward.Server.Make(Rpc)(R) in
    F.create r
    >>= fun f ->
    let f_address = { Dns_forward.Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 3 } in
    let open Error in
    F.serve ~address:f_address f
    >>= fun () ->
    Rpc.connect f_address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    Rpc.rpc c request
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" foo_private (Ipaddr.V4.to_string ipv4);
    let open Lwt.Infix in
    Rpc.disconnect c
    >>= fun () ->
    F.destroy f
    >>= fun () ->
    Lwt.return (Result.Ok ())
  end with
  | Result.Ok () ->
    (* the disconnects and close should have removed all the connections: *)
    Alcotest.(check int) "number of connections" 0 (List.length @@ Rpc.get_connections ());
    (* The server should have sent the query only to foo and not to bar *)
    Alcotest.(check int) "foo_server queries" 1 (S.get_nr_queries foo_server);
    Alcotest.(check int) "bar_server queries" 0 (S.get_nr_queries bar_server);
  | Result.Error (`Msg m) -> failwith m


let test_infra_set = [
  "Server responds correctly", `Quick, test_server;
]

let test_protocol_set = [
  "TCP multiplexing", `Quick, test_tcp_multiplexing;
]

let test_forwarder_set = [
  "Per-server timeouts", `Quick, test_timeout;
  "Zone config respected", `Quick, test_forwarder_zone;
  "Local names resolve ok", `Quick, test_local_lookups;
  "Server order", `Quick, test_order;
  "Caching", `Quick, test_cache;
]

open Dns_forward.Config

let config_examples = [
  "nameserver 10.0.0.2\nnameserver 1.2.3.4#54\nsearch a b c",
  { servers = Server.Set.of_list [
    { Server.address = { Address.ip = Ipaddr.V4 (Ipaddr.V4.of_string_exn "10.0.0.2"); port = 53 }; zones = Domain.Set.empty; timeout_ms = None; order = 0 };
    { Server.address = { Address.ip = Ipaddr.V4 (Ipaddr.V4.of_string_exn "1.2.3.4"); port = 54 }; zones = Domain.Set.empty; timeout_ms = None; order = 0 };
    ]; search = [ "a"; "b"; "c" ]
  };
  "nameserver 10.0.0.2\n",
  { servers = Server.Set.of_list [
    { Server.address = { Address.ip = Ipaddr.V4 (Ipaddr.V4.of_string_exn "10.0.0.2"); port = 53 }; zones = Domain.Set.empty; timeout_ms = None; order = 0 };
    ]; search = []
  };
  String.concat "\n" [
    "# a pretend VPN zone with a private nameserver";
    "nameserver 1.2.3.4";
    "zone mirage.io foo.com";
    "timeout 5000";
    "order 1";
    "";
    "# a default nameserver";
    "nameserver 8.8.8.8";
  ], {
    servers = Server.Set.of_list [
      { Server.address = { Address.ip = Ipaddr.V4 (Ipaddr.V4.of_string_exn "8.8.8.8"); port = 53 }; zones = Domain.Set.empty; timeout_ms = None; order = 0; };
      { Server.address = { Address.ip = Ipaddr.V4 (Ipaddr.V4.of_string_exn "1.2.3.4"); port = 53 };
        zones = Domain.Set.of_list [ [ "mirage"; "io" ]; [ "foo"; "com" ] ];
        timeout_ms = Some 5000;
        order = 1;
      };
    ]; search = [];
  };
]

let test_parse_config txt expected () =
  match of_string txt with
  | Result.Error (`Msg m) -> failwith m
  | Result.Ok x ->
    if compare expected x <> 0
    then failwith ("failed to parse " ^ txt)

let test_config = List.map (fun (txt, expected) ->
  "DNS " ^ (String.escaped txt), `Quick, test_parse_config txt expected
) config_examples

let () =
  Alcotest.run "dns-forward" [
    "Test infrastructure", test_infra_set;
    "Test forwarding", test_forwarder_set;
    "Test protocols", test_protocol_set;
    "Test config parsing", test_config;
  ]
