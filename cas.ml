module Cas (App: App.APP) = struct

  open Lwt
  open Config
  open Ocsigen_messages
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
  open Eliom_tools.F


  exception CASConnectionError of string
  exception CASDataError of string
  exception XMLDataError

  (* Download the data at the url specified by url, via a Lwt.
   * Throws CASConnectionError as Lwt exceptions. *)
  let download_data url =
    try%lwt
      let%lwt http_frame = Ocsigen_http_client.get_url url in
      match Ocsigen_http_frame.(http_frame.frame_content) with
      | Some stream ->
        begin
          let real_stream = Ocsigen_stream.get stream in
          let str = Ocsigen_stream.string_of_stream 10000 real_stream in
          Ocsigen_stream.finalize stream `Success >>= fun () -> 
          str
        end
      | None -> fail (CASConnectionError "Ocsigen did not provide a valid http stream.")
    with
    | Ocsigen_lib.Ip_address.No_such_host -> fail (CASConnectionError "no such host") 
    | CASConnectionError(e) -> fail (CASConnectionError(e))
    | ex -> fail (CASConnectionError ("unknown error " ^ Printexc.to_string ex))

  type xml_tree_checker =
    | Node of string * xml_tree_checker
    | Attribute of string * string
    | InlineData

  let rec get_xml_value xml tree = match (xml, tree) with
    | Simplexmlparser.Element(tag, _, children), Node(tag2, child_tree) when tag = tag2 ->
      begin
        let rec iter_children = function
          | [] -> raise XMLDataError
          | t::q ->
            try
              get_xml_value t child_tree
            with
            | XMLDataError -> iter_children q
        in
        iter_children children
      end
    | Simplexmlparser.Element(tag, attributes, _), Attribute(tag2, attribute) when tag = tag2->
      begin
        try
          snd (List.find (fun (name, value) -> name = attribute) attributes)
        with
        | Not_found -> raise XMLDataError
      end
    | Simplexmlparser.PCData(s), InlineData -> s
    | _ -> raise XMLDataError

  (* Read the xml CAS data and return the user login.
   * Throws a CASDataError if anything wrong occurs. *)
  let cas_xml_get_login data =
    let xml::[] = Simplexmlparser.xmlparser_string data in
    try
      get_xml_value xml (Node("cas:serviceResponse", Node("cas:authenticationSuccess", Node("cas:user", InlineData))))
    with
    | XMLDataError -> raise (CASDataError data)


  let cas_xml_is_successful_debug func data =
    let xml::[] = Simplexmlparser.xmlparser_string data in
    match xml with
    | Element(a, t, children) -> List.map (fun (a, b) -> func a; func b) t

  let send_error str =
    Ocsigen_messages.errlog str;
    Eliom_registration.Html5.send 
      (Template.make_page [pcdata ("Error: " ^ str)])

  let service_path = ["login"; "cas"]
  let service_url = List.fold_left (fun a b -> a ^ "/" ^ b) "" service_path
  let _ = Ocsigen_messages.errlog service_url

  let main_service =
    Eliom_registration.Any.register_service
      ~path:service_path
      ~get_params:Eliom_parameter.(string "ticket")
      (fun ticket () ->
         try%lwt
           let ssl = if Eliom_request_info.get_ssl () then "s" else "" in
           let path = Format.sprintf "http%s://%s:%d" ssl (Eliom_request_info.get_hostname ()) (Eliom_request_info.get_server_port ()) in
           let cas_url = Format.sprintf "%s/serviceValidate?ticket=%s&service=%s%s" cas_server ticket path service_url in
           let%lwt cas_data = download_data cas_url in 
           let user_id = cas_xml_get_login cas_data in
           let%lwt () = User.perform_login user_id in

           Eliom_registration.Redirection.send App.welcome_service
         with
         | CASConnectionError(error) -> send_error ("Could not connect to the CAS to check the authentification: " ^ error)
         | CASDataError(error) -> send_error ("CAS data not recognized: " ^ error)
      );
    Eliom_registration.Redirection.register_service
      ~path:service_path
      ~options:`TemporaryRedirect
      ~get_params:Eliom_parameter.(unit)
      (fun () () ->
           let ssl = if Eliom_request_info.get_ssl () then "s" else "" in
           let path = Format.sprintf "http%s://%s:%d" ssl (Eliom_request_info.get_hostname ()) (Eliom_request_info.get_server_port ()) in
         return (Eliom_service.preapply
                   (Eliom_service.Http.external_service ~prefix:cas_server ~path:["login"] ~get_params:Eliom_parameter.(string "service") ())
                   (path ^ service_url))
      )

end
