open Lwt

exception CASConnectionError of string

let download_data url =
	catch (fun () ->
	begin
		(Ocsigen_http_client.get_url url >>= fun (http_frame) ->
			match Ocsigen_http_frame.(http_frame.frame_content) with
			| Some stream ->
				begin
				let real_stream = Ocsigen_stream.get stream in
				let str = Ocsigen_stream.string_of_stream 10000 real_stream in
				Ocsigen_stream.finalize stream `Success >>= fun () -> 
					str
				end
			| None -> raise (CASConnectionError "Ocsigen did not provide a valid http stream.") )
	end)
	begin
		function
		| Ocsigen_lib.Ip_address.No_such_host -> fail (CASConnectionError "no such host") 
		| ex -> fail (CASConnectionError ("unknown error " ^ Printexc.to_string ex))
	end
	
		
