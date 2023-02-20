open Vdom

(* Custom commands *)

type 'msg Vdom.Cmd.t +=
  | Http_get of { url : string; payload : string; on_success : string -> 'msg }
  | After of int * 'msg

let http_get (type msg) ~url ~payload (on_success : _ -> msg) : msg Vdom.Cmd.t =
  Http_get { url; payload; on_success }
;;

let button ?(a = []) txt f =
  input [] ~a:(onclick (fun _ -> f) :: type_button :: value txt :: a)
;;

let after x f = After (x, f)

type content = Nothing | Loading of string | Data of string
type model = { url : string; focused : bool; content : content }

let init =
  return
    { url = "https://jsonplaceholder.typicode.com/todos/1"
    ; focused = false
    ; content = Nothing
    }
;;

let update model = function
  | `Set url -> return { model with url }
  | `FetchStart ->
      return
        { model with content = Loading model.url }
        ~c:
          (if String.ends_with ~suffix:"2" model.url then
            (* [ http_get ~url:model.url ~payload:"" (fun r -> `Fetched r) ] *)
            (* Line above also works! *)
            [ Http_get
                { url = model.url
                ; payload = ""
                ; on_success = (fun r -> `Fetched r)
                }
            ]
          else
            [ after 2000 (`Fetch model.url) ])
  | `Fetch url ->
      return model ~c:[ http_get ~url ~payload:"" (fun r -> `Fetched r) ]
  | `Fetched s -> return { model with content = Data s }
  | `Focused b -> return { model with focused = b }
;;

let view { url; focused; content } =
  div
    [ input
        ~a:
          [ int_prop "size"
              (if focused then
                200
              else
                100)
          ; value url
          ; oninput (fun s -> `Set s)
          ; onfocus (`Focused true)
          ; onblur (`Focused false)
          ]
        []
    ; div [ button "Fetch" `FetchStart ]
    ; (match content with
      | Nothing ->
          text
            "Please type an URL to load.\n\
             (NOTE: no artificial delay if URL ends with '2')"
      | Loading url -> text (Printf.sprintf "Loading %s, please wait..." url)
      | Data data -> elt "pre" [ text data ])
    ]
;;

(* APP *)

let app = { init; update; view }

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Js_browser.(Element.append_child (Document.body document))
;;

let () = Js_browser.(Window.set_onload window run)

(* Custom command handlers *)

let run_http_get ~url ~payload ~on_success () =
  let open Js_browser.XHR in
  let r = create () in
  open_ r "GET" url
  ; set_response_type r "text"
  ; set_onreadystatechange r (fun () ->
        match ready_state r with
        | Done -> on_success (response_text r)
        | _ -> ())
  ; send r (Ojs.string_to_js payload)
;;

let cmd_handler ctx = function
  | Http_get { url; payload; on_success } ->
      run_http_get ~url ~payload
        ~on_success:(fun s -> Vdom_blit.Cmd.send_msg ctx (on_success s))
        ()
      ; true
  | After (n, msg) ->
      ignore
        Js_browser.(
          Window.set_timeout window (fun () -> Vdom_blit.Cmd.send_msg ctx msg) n)
      ; true
  | _ -> false
;;

let () = Vdom_blit.(register (cmd { f = cmd_handler }))
