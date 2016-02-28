let get_file_data sent_file =
	let s = Bytes.create 100 in
	let res = ref "" in
	let offset = ref 0 in
	let continue_loop = ref true in
	lwt sent_file_descr = Lwt_unix.openfile (Eliom_request_info.get_tmp_filename sent_file) [] 0o600 in
	let input_chan = Lwt_io.of_fd ~mode:Input sent_file_descr in
	lwt strings = (Lwt_stream.to_list (Lwt_io.read_lines input_chan)) in
	Lwt.return (List.fold_left (^) "" strings)
