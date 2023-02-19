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

type todo =
  { descr : string
  }

type model =
  { todos : todo list
  }

let init =
  { todos = [{descr = "First todo!"}
            ; {descr = "Second todo!"}]}

(* rs => reactive signal? *)
type rs = model React.signal

(* rf => reactive function? *)
type rf = ?step:React.step -> model -> unit

(* rp => reactive pair? *)
type rp = rs * rf

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



let css_visibility (model:model) =
  match model.todos with
  | [] -> "visibility: hidden;"
  | _ -> "visibility: visible;"

let todo_item ((r, f) : rp) (todo : todo) =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    li
      [ div
          ~a:[ a_class [ "view" ] ]
          [txt todo.descr]
      ])

let todo_list ((r, f) : rp) =
  let css_visibility (todos: todo list) =
    match todos with
    | [] -> "visibility: hidden;"
    | _ -> "visibility: visible;"
  in
  let list_of_visible_tasks m =
    let is_visible _todo = true in
    List.filter is_visible m.todos
  in

  let r_todos = React.S.map (fun m -> m.todos) r in
  let rl =
    ReactiveData.RList.from_signal (React.S.map list_of_visible_tasks r)
  in
  let rl = ReactiveData.RList.map (todo_item (r, f)) rl in
  Js_of_ocaml_tyxml.Tyxml_js.(
    Html5.(
      section
        ~a:
          [ a_class [ "main" ]
          ; R.Html5.a_style (React.S.map css_visibility r_todos)
          ]
        [ Js_of_ocaml_tyxml.Tyxml_js.Html5.input ()
        ; label ~a:[ a_label_for "toggle-all" ] [ txt "Mark all as complete" ]
        ; R.Html5.ul ~a:[ a_class [ "todo-list" ] ] rl
        ]))
;;

let view ((r, f):rp) =
  Js_of_ocaml_tyxml.Tyxml_js.Html5.(
    div
      ~a:[ a_class [ "todomvc-wrapper" ] ]
      [ section ~a:[ a_class [ "todoapp" ] ]
          [ header ~a:[a_class ["header"]]
              [ h1 [txt "todos"]
              ; todo_list (r, f)
              ]
          ]
      ; footer ~a: [a_class ["info"]]
          [ p [txt "Double-click to edit a todo"]
          ; p [txt "Written by "; a ~a:[a_href "https://github.com/benjamin-thomas"] [txt "Benjamin Thomas"]]
          ; p [txt "Part of "; a ~a:[a_href "http://todomvc.com"] [txt "TodoMVC"]]
          ]
      ])
;;


let main (_ : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t) =
  let (rp:rp) = React.S.create init in
  let document = Js_of_ocaml.Dom_html.document in
  Js_of_ocaml.Dom.appendChild document##.body
    (Js_of_ocaml_tyxml.Tyxml_js.To_dom.of_div @@ view rp)
; Lwt.return ()
;;

let _ =
  let%lwt evt = Js_of_ocaml_lwt.Lwt_js_events.onload () in
  Lwt.return @@ main evt
;;
