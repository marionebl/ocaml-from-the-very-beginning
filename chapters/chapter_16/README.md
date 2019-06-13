## stats

```
ocamlopt textstat.mli textstat.ml stats.ml -o stats
./stats stats.ml
```

## reverse

```
ocamlopt reverse.ml -o reverse
./reverse stats.ml s.ml
```

## fibonacci

```bash
ocamlc fibonacci.ml -o fibonacci-bc
time ocamlrun ./fibonacci-bc 40
# real    0m16.795s
# user    0m16.723s
# sys     0m0.021s

ocamlopt fibonacci.ml -o fibonacci
time ./fibonacci 40
# real    0m1.469s
# user    0m1.460s
# sys     0m0.013s
```

## match

```
ocamlopt match.ml -o match
./match README.md match
```
