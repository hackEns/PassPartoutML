
open Lwt
open Irmin_unix
module Store =
  Irmin_git.FS (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
let prog = Store.Repo.create config

exception Keyring_exist
exception Keyring_bad_format

let (get_keyring_data: string -> string Lwt.t) =
  fun keyring_data ->
    prog
    >>= Store.master task
    >>= fun t -> Store.read_exn (t "reading") [keyring_data]

let get_keyring_list () =
  let l = ref [] in
  prog
  >>= Store.master task
  >>= fun t ->
  let%lwt () = Store.iter (t "whole keyring list") (fun k _ ->
      l := (String.concat "/" k)::!l;
      Lwt.return_unit)
  in
  Lwt.return !l

(* register the appropriate permissions *)
let init_engine () =
  let%lwt all_keyring = get_keyring_list () in
  Lwt_list.iter_p (fun s -> User.register_permission s) all_keyring

let _ = User.register_permission "create keyring"

let set_keyring_data : string  -> string -> unit Lwt.t = fun name keyring_data ->
  prog
  >>= Store.master task
  >>= fun t ->
  Store.update (t "setting") [name] keyring_data


let new_keyring name data =
  let check_good_name name =
    for i = 0 to (String.length name) - 1 do
      if name.[i] < 'A' || name.[i] > 'z' then
        raise Keyring_bad_format
    done;
  in
  check_good_name name;
  try%lwt
    let%lwt _ = get_keyring_data name in
    raise Keyring_exist
  with
  | Invalid_argument(_) ->
    begin
      set_keyring_data name data >>= fun () ->
      User.register_permission name
    end


[%%client
   (* site * username * password *)
  type keyring_entry = string * string * string


  let _ =
    let open Lwt in
    Lwt_js_events.onload () >>= fun _ -> begin
      Lwt.return (Js.Unsafe.js_expr "sjcl")##.random##startCollectors
    end

  exception WrongPassword
  let decipher key data : string =
    try
      Js.to_string ((Js.Unsafe.js_expr "sjcl")##decrypt (Js.string key) (Js.string data))
    with
    | _ -> raise WrongPassword


  let cipher key data : string =
    Js.to_string ((Js.Unsafe.js_expr "sjcl")##encrypt (Js.string key) (Js.string data))

  let empty_keyring:(keyring_entry list) = []

  let load_data:(string -> keyring_entry list) = fun data -> data |> Js.string |> Json.unsafe_input

  let encode_data d = d |> Json.output |> Js.to_string

  let cipher_data password data = data |> encode_data |> cipher password

]
