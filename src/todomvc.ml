(*
  foreman start -f Procfile.dev
*)

[@@@warning "-27-32-34-37-69"]

let task_input =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    input ~a:[a_class ["new-todo"]; a_placeholder "What needs to be done?"; a_autofocus ()] ()
  )

let task_input_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input task_input
let set_task_input v = task_input_dom##.value := Js_of_ocaml.Js.string v

(*
let task_entry ((r, f) : rp) =
  bind_event Ev.keypresses task_input_dom (fun evt ->
      Lwt.return
      @@
      if evt##.keyCode = 13 then (
        Controller.update Add (r, f)
        ; set_task_input ""
      ))

  ; bind_event Ev.inputs task_input_dom (fun _ ->
        Lwt.return
        @@ Controller.update (Update_field task_input_dom##.value) (r, f))

  ; Html5.(
      header ~a:[ a_class [ "header" ] ] [ h1 [ txt "todos" ]; task_input ])
;;
*)

type task =
  { description : string
  ; id : int
  }

type action = Add

(* let update action (r, f) =
   let m = React.S.value r in
   let m = match action with
    | Add -> 0 *)

let task_entry (r, f) =
  Js_of_ocaml_lwt.Lwt_js_events.keypresses task_input_dom
    (fun (evt: Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t) (_: unit Lwt.t) ->
       if evt##.keyCode = 13 then
         (print_endline "Enter was pressed!";
          (* update Add (r, f) *)
          set_task_input "")
       else
         print_endline "Some other key was pressed!";
       Lwt.return ())

let view =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    div
      ~a:[ a_class [ "todomvc-wrapper" ] ]
      [ section ~a:[ a_class [ "todoapp" ] ]
          [ header ~a:[a_class ["header"]]
              [ h1 [txt "todos"]
              ; task_input
              ]
          ]
      ; footer ~a: [a_class ["info"]]
          [ p [txt "Double-click to edit a todo"]
          ; p [txt "Written by "; a ~a:[a_href "https://github.com/benjamin-thomas"] [txt "Benjamin Thomas"]]
          ; p [txt "Part of "; a ~a:[a_href "http://todomvc.com"] [txt "TodoMVC"]]
          ]
      ])
;;

(*
 Dom.appendChild parent
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div (View.view rp))
*)

let main (_ : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t) =
  let document = Js_of_ocaml.Dom_html.document in
  (* let child = Js_of_ocaml.Dom_html.createH1 document in
     Js_of_ocaml.Dom.appendChild document##.body child
     ; child##.textContent :=
         Js_of_ocaml.Js.Opt.return (Js_of_ocaml.Js.string "Hello from JSOO!"); *)
  Js_of_ocaml.Dom.appendChild document##.body
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div view)
; Lwt.return ()
;;

(*
let _ = Lwt.bind (Js_of_ocaml_lwt.Lwt_js_events.onload ()) (fun evt -> main evt)
*)

(*
let _ =
  let open Lwt.Infix in
  Js_of_ocaml_lwt.Lwt_js_events.onload () >>= main
;;
*)

let _ =
  let%lwt evt = Js_of_ocaml_lwt.Lwt_js_events.onload () in
  Lwt.return @@ main evt
;;
