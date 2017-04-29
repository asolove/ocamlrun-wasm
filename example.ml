let example () = 
  let q = Chan.create () in
  Chan.fork (fun () ->
    print_string "\nSending message 1\n";
    Chan.send (q, 1);
    print_string "\nSending message 2\n";
    Chan.send (q, 2));
  Chan.fork (fun () ->
    print_int (Chan.recv q);
    print_int (Chan.recv q));;

let () = Chan.run example

