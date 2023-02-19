(*
 * MODEL
 *)
type model = int
type rs = model React.signal
type rf = ?step:React.step -> model -> unit
type rp = rs * rf

let init : model = 0

(*
 * UPDATE
 *)

type action = Click | NOOP [@@warning "-37"]

let update action ((r, f) : rp) =
  let current_model = React.S.value r in
  let new_model =
    match action with
    | NOOP -> current_model
    | Click -> current_model + 1
  in
  f new_model
;;

(*
 * VIEW
 *)

let view_btn ((r, _f) : rp) =
  let model = React.S.value r in
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(button [ txt (string_of_int model) ])
;;

let view_btn_dom (rp : rp) =
  Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button (view_btn rp)
;;

let view ((r, f) : rp) =
  Js_of_ocaml_lwt.Lwt_js_events.(
    async (fun () ->
        clicks
          (view_btn_dom (r, f))
          (fun _evt _ -> Lwt.return @@ update Click (r, f))))

  ; Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_button @@ view_btn (r, f)
;;

let main (_ : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t) =
  let document = Js_of_ocaml.Dom_html.document in
  Js_of_ocaml.Dom.appendChild document##.body (view @@ React.S.create init)
  ; Lwt.return ()
;;

let _ =
  let%lwt evt = Js_of_ocaml_lwt.Lwt_js_events.onload () in
  Lwt.return @@ main evt
;;
