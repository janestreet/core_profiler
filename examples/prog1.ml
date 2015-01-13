open! Core.Std

let func () =
  let key = Random.int 100 in
  if key mod 3 <> 0 then begin
    ignore (Array.create ~len:100 10);
  end

let () =
  for _i = 1 to 10_000_000 do
    func ()
  done
