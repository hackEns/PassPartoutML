open Lwt

let download_data url =
	(Ocsigen_http_client.get_url url >>= fun (http_frame) ->
		let Some stream = Ocsigen_http_frame.(http_frame.frame_content) in
		let real_stream = Ocsigen_stream.get stream in
		let str = Ocsigen_stream.string_of_stream 10000 real_stream in
		Ocsigen_stream.finalize stream;
		str)
