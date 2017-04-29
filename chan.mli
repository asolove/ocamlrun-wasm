type 'a chan
val create : unit -> 'a chan

val send : ('a chan * 'a) -> unit
val recv : 'a chan -> 'a

val fork : (unit -> unit) -> unit
val yield : unit -> unit

val run: (unit -> unit) -> unit
