# raytracing

Oto bardzo prosty raytracing na podstawie naszych wypocin z [warsztatów haskellowych](https://gitlab.binarapps.com/-/snippets/40).

## Sposób użycia

Po sklonowaniu:

```sh
cabal build
```

I renderujemy:

```sh
cat smcfos.stl | cabal run | tail -n +2 > output.pbm
```

Objaśnienie:
* `cat smcfos.stl` przekazuje na wejcie naszego programu zawratość pliku tekstowego `smcfos.stl`
* `cabal run` odpala program, odczytuje standardowe wejcie, renderuje co trzeba i przekazuje dalej
* `tail -n +2` filtruje standardowe wejście usuwając pierwszą linijkę (Pamiętacie jak mówiłem, że `cabal run` próbuje przekompilować na nowo projekt? Otóż właśnie tu jest to upierdliwe, bo skompilowany projekt zawsze wypluje linijkę `Up to date` - i tę linijkę chcemy zignorować. Pamiętajcie, że jakiekolwiek zmiany w kodzie spowodują, że przy uruchomieniu `cabal run` projekt skompiluje się na nowo, co zaowocuje więcej niż jedną liniką, którą bedziemy chcieli zignorować.)
* `output.pbm` to nasz obrazek w formacie PBM.

## Podgląd

A tak to wygląda po odpaleniu ☝️:

![SomeMisteriousCreatureFromOuterSpace](./output.pbm)
