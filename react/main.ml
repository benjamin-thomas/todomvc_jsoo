(*
dune exec --display=quiet react/main.exe
*)

(*
The string "\x1B[8D" is an ANSI escape code that tells the terminal to move the cursor back 8 characters.
The D character indicates that the cursor should move to the left, and the 8 specifies the number of characters to move.
This code can be used to overwrite the previously printed digits with new ones.
*)
let mv_cursor = "\x1B[8D"

let pr_time t =
  let tm = Unix.localtime t in
  Printf.printf "%s%02d:%02d:%02d%!" mv_cursor tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;;

let seconds, run =
  let evt, send = React.E.create () in
  let exec () =
    while true do
      send (Unix.gettimeofday ())
      ; Unix.sleep 1
    done
  in
  (evt, exec)
;;

let _printer = React.E.map pr_time seconds
let () = run ()
