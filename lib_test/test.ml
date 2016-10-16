
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
    S.serve s address
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

let test_infra_set = [
  "Server responds correctly", `Quick, test_server;
]

let () =
  Alcotest.run "dns-forward" [
    "Test infrastructure", test_infra_set;
  ]
