(*
  foreman start -f Procfile.dev
*)

[@@@warning "-23-26-27-32-34-37-69"]

type todo = { descr : string }
type model = { field : string; todos : todo list }
type rs = model React.signal
type rf = ?step:React.step -> model -> unit
type rp = rs * rf

let task_input ((r, f) : rp) =
  let s_field = React.S.map (fun m -> m.field) r in
  Js_of_ocaml_tyxml.Tyxml_js.(
    Html.(
      R.Html5.input
        ~a:
          [ a_input_type `Text
          ; a_class [ "new-todo" ]
          ; a_placeholder "What needs to be done?"
          ; a_autofocus ()
          ; R.Html.a_value s_field
          ]
        ()))
;;

let task_input_dom (rp : rp) =
  Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_input (task_input rp)
;;

let init = { field = ""; todos = [ { descr = "First todo!" } ] }

type action = Add | Update of string

let update action ((r, f) : rp) =
  let curr_model = React.S.value r in
  let new_model =
    match action with
    | Update str -> { curr_model with field = str }
    | Add ->
        let new_todo = { descr = curr_model.field } in
        { curr_model with todos = new_todo :: curr_model.todos; field = "" }
  in
  f new_model
;;

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Js_of_ocaml_lwt.Lwt_js_events.(async @@ fun () -> ev elem handler)
;;

let task_entry ((r, f) : rp) =
  (* let clear_input = task_input_dom##.value := Js_of_ocaml.Js.string "" in *)
  bind_event Js_of_ocaml_lwt.Lwt_js_events.keypresses
    (task_input_dom (r, f))
    (fun evt ->
      Lwt.return @@ if evt##.keyCode = 13 then update Add (r, f)
      (* ; clear_input *))

  ; bind_event Js_of_ocaml_lwt.Lwt_js_events.inputs
      (task_input_dom (r, f))
      (fun _ ->
        Lwt.return
        @@ update
             (Update
                ((task_input_dom (r, f))##.value |> Js_of_ocaml.Js.to_string))
             (r, f))

  ; task_input (r, f)
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
