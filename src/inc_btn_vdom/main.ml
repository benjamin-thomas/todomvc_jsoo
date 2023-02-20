[@@@warning "-32-37-69"]

module V = Vdom

(*
 * MODEL
 *)

type data = Not_started | Loading | Loaded of string
type model = { x : int; d : data }

(* HTTP *)
let get_todo =
  let open Js_browser in
  let xhr = XHR.create () in
  XHR.open_ xhr "GET" "https://jsonplaceholder.typicode.com/todos/1"
  ; XHR.set_onreadystatechange xhr @@ fun () ->
    match XHR.ready_state xhr with
    | Done ->
        print_endline "Got data!"
        ; print_endline (XHR.response_text xhr)
    | _ -> ()
;;

(*
 * UPDATE
 *)

type msg = Inc | Dec | Get_todo

let init : model * msg V.Cmd.t = V.return { x = 0; d = Not_started }

let update (model : model) (msg : msg) : model * msg V.Cmd.t =
  match msg with
  | Inc -> V.return { model with x = model.x + 1 }
  | Dec -> ({ model with x = model.x - 1 }, V.Cmd.batch [])
  | Get_todo -> V.return { model with d = Loading }
;;

(* | Get_todo -> V.return 9 ~c:[ V.Cmd.batch [] ] *)

let button txt msg =
  V.(input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ])
;;

(*
 * VIEW
 *)

let p = V.elt "p"
let br = V.elt "br"

let view (model : model) : msg V.vdom =
  let row =
    V.(div [ button "-" Dec; text (string_of_int model.x); button "+" Inc ])
  in
  V.(
    div
      [ row
      ; div
          [ p [ text "Hello custom p tag" ]
          ; br []
          ; p [ text "Hello custom p tag, again" ]
          ; button (string_of_int model.x) Inc
          ; br []
          ; button "Click to fetch data" Get_todo
          ]
      ])
;;

(* let run_http_get ~url ~payload ~on_success () =
     let open Js_browser.XHR in
     let r = create () in
     open_ r "GET" url
     ; set_onreadystatechange r (fun () ->
           match ready_state r with
           | Done -> on_success (response_text r)
           | _ -> ())
     ; send r payload
   ;; *)

let app = V.app ~init ~view ~update ()

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Js_browser.(Element.append_child (Document.body document))
;;

let () = Js_browser.(Window.set_onload window run)
