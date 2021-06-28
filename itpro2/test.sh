./interpret <<EOF
(let f =
  (let x = 3 in
    (fun y -> (x + y)))
 in
  (let x = 7 in (f 10)))
EOF
