
[%describe
[@attr "test"]
[%it 
let x = "test" in
let y = "foobar" in
Printf.printf "%s %s" x y 
]
;
[%it
let x = "test" in
let y = "foobar" in
Printf.printf "%s %s" x y 
]
]