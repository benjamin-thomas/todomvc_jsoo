module V = Vdom

let p = V.elt "p"
let br = V.elt "br"

type model = int

let update (model : model) = function
  | `Inc -> model + 1
  | `Dec -> model - 1
;;

let init = 0

let button txt msg =
  V.(input [] ~a:[ onclick (fun _ -> msg); type_button; value txt ])
;;

let view model =
  let row =
    V.(div [ button "-" `Dec; text (string_of_int model); button "+" `Inc ])
  in
  V.(
    div
      [ row
      ; div
          [ p [ text "Hello custom p tag" ]
          ; br []
          ; p [ text "Hello custom p tag, again" ]
          ; button (string_of_int model) `Inc
          ]
      ])
;;

let app = V.simple_app ~init ~view ~update ()

open Js_browser

let run () =
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)
;;

let () = Window.set_onload window run
