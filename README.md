# type_inference
型推論をするプログラム．パーサ部分はまだ書いていない

```
$ cargo run
let x := 15 in let f(y,z) := y + z in if 0 < x then x else f(10,x)
Int
```
