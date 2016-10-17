
module Error = Dns_forward_error.Infix

let fresh_id =
  let next = ref 0 in
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
  match pkt.Dns.Packet.answers with
  | [ { Dns.Packet.rdata = Dns.Packet.A ipv4; _ } ] ->
    Lwt.return (`Ok ipv4)
  | _ -> Lwt.return (`Error (`Msg "failed to find answers"))

let test_server () =
  match Lwt_main.run begin
    let module S = Server.Make(Rpc) in
    let s = S.make [ "foo", Ipaddr.V4 Ipaddr.V4.localhost; "bar", Ipaddr.of_string_exn "1.2.3.4" ] in
    let open Error in
    (* The virtual address we run our server on: *)
    let address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 53 } in
    S.serve ~address s
    >>= fun () ->
    Rpc.connect address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    Rpc.rpc c request
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" "127.0.0.1" (Ipaddr.V4.to_string ipv4);
    Lwt.return (`Ok ())
  end with
  | `Ok () -> ()
  | `Error (`Msg m) -> failwith m

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end

let test_forwarder_zone () =
  match Lwt_main.run begin
    let module S = Server.Make(Rpc) in
    let foo_public = "8.8.8.8" in
    let foo_private = "192.168.1.1" in
    (* a VPN mapping 'foo' to an internal ip *)
    let foo_server = S.make [ "foo", Ipaddr.of_string_exn foo_private ] in
    let foo_address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 1 } in
    let open Error in
    S.serve ~address:foo_address foo_server
    >>= fun () ->
    (* a public server mapping 'foo' to a public ip *)
    let bar_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
    let bar_address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 2 } in
    S.serve ~address:bar_address bar_server
    >>= fun () ->
    (* a forwarder which uses both servers *)
    let module F = Dns_forward.Make(Rpc)(Rpc)(Time) in
    let config = [
      { Dns_forward_config.address = foo_address; zones = [ [ "foo" ] ] };
      { Dns_forward_config.address = bar_address; zones = [] }
    ] in
    let open Lwt.Infix in
    F.make config
    >>= fun f ->
    let f_address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 3 } in
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
    Lwt.return (`Ok ())
  end with
  | `Ok () -> ()
  | `Error (`Msg m) -> failwith m

let test_local_lookups () =
  match Lwt_main.run begin
    let module S = Server.Make(Rpc) in
    let foo_public = "8.8.8.8" in
    let foo_private = "192.168.1.1" in
    (* a public server mapping 'foo' to a public ip *)
    let public_server = S.make [ "foo", Ipaddr.of_string_exn foo_public ] in
    let public_address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 4 } in
    let open Error in
    S.serve ~address:public_address public_server
    >>= fun () ->
    let module F = Dns_forward.Make(Rpc)(Rpc)(Time) in
    let config = [
      { Dns_forward_config.address = public_address; zones = [] };
    ] in
    let open Lwt.Infix in
    F.make config
    >>= fun f ->
    let f_address = { Dns_forward_config.ip = Ipaddr.V4 Ipaddr.V4.localhost; port = 5 } in
    let local_names_cb question =
      let open Dns.Packet in
      match question with
      | { q_name; q_type = Q_A; _ } ->
        let rdata = A (Ipaddr.V4.of_string_exn foo_private) in
        let name = q_name and cls = RR_IN and flush = false and ttl = 100l in
        Lwt.return (Some [ { name; cls; flush; ttl; rdata } ])
      | _ ->
        Lwt.return None in
    let open Error in
    F.serve ~address:f_address ~local_names_cb f
    >>= fun () ->
    Rpc.connect f_address
    >>= fun c ->
    let request = make_a_query (Dns.Name.of_string "foo") in
    Rpc.rpc c request
    >>= fun response ->
    parse_response response
    >>= fun ipv4 ->
    Alcotest.(check string) "IPv4" foo_private (Ipaddr.V4.to_string ipv4);
    Lwt.return (`Ok ())
  end with
  | `Ok () -> ()
  | `Error (`Msg m) -> failwith m

let test_infra_set = [
  "Server responds correctly", `Quick, test_server;
]

let test_forwarder_set = [
  "Zone config respected", `Quick, test_forwarder_zone;
  "Local names resolve ok", `Quick, test_local_lookups;
]

let () =
  Alcotest.run "dns-forward" [
    "Test infrastructure", test_infra_set;
    "Test forwarding", test_forwarder_set;
  ]
