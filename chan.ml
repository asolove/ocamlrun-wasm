type 'a chan =
  { send_q : ('a * (unit, unit) continuation) Queue.t;
    recv_q : ('a, unit) continuation Queue.t;
  }

effect Send : ('a chan * 'a) -> unit
effect Receive : 'a chan -> 'a
effect Fork : (unit -> unit) -> unit
effect Yield : unit

let create () =
  { send_q = Queue.create ();
    recv_q = Queue.create ()
  }

let send (ch, msg) = perform (Send (ch, msg))
let recv ch = perform (Receive ch)
let fork t = perform (Fork t)
let yield () = perform Yield

let run main =
  let run_q = Queue.create () in
  let enqueue t = Queue.push t run_q in
  let rec dequeue () =
    if Queue.is_empty run_q then ()
    else continue (Queue.pop run_q) ()
  in let rec spawn f =
    match f () with
    | () -> dequeue ()
    | effect Yield k ->
        ( enqueue k; dequeue() )
    | effect (Fork fn) k ->
        ( enqueue k; spawn fn )
    | effect (Send ({recv_q; send_q}, msg)) k ->
        if Queue.is_empty recv_q
        then (Queue.push (msg, k) send_q; dequeue ())
        else (enqueue k; continue (Queue.pop recv_q) msg)
    | effect (Receive {recv_q; send_q}) k ->
        if Queue.is_empty send_q
        then (Queue.push k recv_q; dequeue ())
        else let msg, k2 = Queue.pop send_q in (enqueue k2; continue k msg)
  in
  spawn main
