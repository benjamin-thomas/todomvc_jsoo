(*
  foreman start -f Procfile.dev
*)

[@@@warning "-23-27-32-34-37-69"]

let task_input =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    input
      ~a:
        [ a_input_type `Text
        ; a_class [ "new-todo" ]
        ; a_placeholder "What needs to be done?"
        ; a_autofocus ()
        ]
      ())
;;

let task_input_dom = Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input task_input

type todo = { descr : string }
type model = { field : string; todos : todo list }
type rs = model React.signal
type rf = ?step:React.step -> model -> unit
type rp = rs * rf

let init = { field = ""; todos = [ { descr = "First todo!" } ] }

type action = Add

let update action ((r, f) : rp) =
  let curr_model = React.S.value r in
  let new_model =
    match action with
    | Add ->
        (* FIXME: field is empty here! *)
        let new_todo = { descr = "FIXME: " ^ curr_model.field } in
        { curr_model with todos = new_todo :: curr_model.todos }
  in
  f new_model
;;

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Js_of_ocaml_lwt.Lwt_js_events.(async @@ fun () -> ev elem handler)
;;

let task_entry ((r, f) : rp) =
  let clear_input = task_input_dom##.value := Js_of_ocaml.Js.string "" in

  let update_on_enter evt =
    Lwt.return
      (if evt##.keyCode = 13 then (
        update Add (r, f)
        ; clear_input
      ))
  in

  bind_event Js_of_ocaml_lwt.Lwt_js_events.keypresses task_input_dom
    update_on_enter
  ; task_input
;;

let css_visibility (model : model) =
  match model.todos with
  | [] -> "visibility: hidden;"
  | _ -> "visibility: visible;"
;;

let view_todo (r, f) todo =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    li
      [ div
          ~a:[ a_class [ "view" ] ]
          [ input ~a:[ a_input_type `Checkbox; a_class [ "toggle" ] ] ()
          ; label [ txt todo.descr ]
          ; input
              ~a:[ a_input_type `Text; a_class [ "edit" ]; a_value todo.descr ]
              ()
          ]
      ])
;;

let todo_list (r, f) =
  let css_visibility (todos : todo list) =
    match todos with
    | [] -> "visibility: hidden;"
    | _ -> "visibility: visible;"
  in
  let todos_of_model model = model.todos in

  let s_todos = React.S.map (fun m -> m.todos) r in
  let r_todos = ReactiveData.RList.from_signal (React.S.map todos_of_model r) in
  let rl = ReactiveData.RList.map (view_todo (r, f)) r_todos in
  Js_of_ocaml_tyxml.Tyxml_js.(
    Html5.(
      section
        ~a:
          [ a_class [ "main" ]
          ; R.Html5.a_style (React.S.map css_visibility s_todos)
          ]
        [ R.Html5.ul ~a:[ a_class [ "todo-list" ] ] rl ]))
;;

let view ((r, f) : rp) =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    div
      ~a:[ a_class [ "todomvc-wrapper" ] ]
      [ section
          ~a:[ a_class [ "todoapp" ] ]
          [ header
              ~a:[ a_class [ "header" ] ]
              [ h1 [ txt "todos" ]; task_entry (r, f); todo_list (r, f) ]
          ]
      ; footer
          ~a:[ a_class [ "info" ] ]
          [ p [ txt "Double-click to edit a todo (NOOP for now)" ] ]
      ])
;;

let main (_ : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t) =
  let rp = React.S.create init in
  let document = Js_of_ocaml.Dom_html.document in
  Js_of_ocaml.Dom.appendChild document##.body
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@ view rp)
  ; Lwt.return ()
;;

let _ =
  let%lwt evt = Js_of_ocaml_lwt.Lwt_js_events.onload () in
  Lwt.return @@ main evt
;;
