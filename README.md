# ocamlinit

The `ocamlinit` tool shows dependencies between OCaml modules.
(not tested on non \*nix environments).

## Require

- OCaml >= 4,
- and ocamldep

## Install

Type `make` and execute the binary `ocamlinit`.

## Usage

Usage: ocamlinit [options] [files]

Extensions  
  -D <opts>   throw opts to ocamldep
  -I <dir>      same as: -D '-I dir'
  -q            encompass file name by double quotation
   
Declarations
  -prefix <str> print with prefix <str>
  -load         print with prefix '#load'
  -use          print with prefix '#use'
  -mod          print with prefix '#mod\_use'
   
Suffixs     
  -suffix <str> print with suffix <str>
  -cmo          print with suffix '.cmo'
  -cma          print with suffix '.cma'
  -ml           print with suffix '.ml'
   
Compositions
  -load-cmo     print with '#load' and '.cmo'
  -use-ml       print with '#use' and '.ml'
  -mod-ml       print with '#mod_use' and '.ml'
  -beta         add directives of Camlp4,Num and Unix
   
            
  -help         Display this list of options
  --help        Display this list of options

## Example

Consider the following dependencies.
```
A
├── B
│   └── C
└── D ─└─ E
```
Then the command works as:
```
$ ocamlinit E.ml
A
D
B
C
E

$ ocamlinit -mod-ml E.ml
#mod_use "A.ml"
#mod_use "D.ml"
#mod_use "B.ml"
#mod_use "C.ml"
#mod_use "E.ml"

$ ocamlinit -mod-ml -beta E.ml
#use "topfind"
#require "camlp4.listcomprehension"
#load "camlp4of.cma"
#load "unix.cma"
#load "nums.cma"
#mod_use "A.ml"
#mod_use "D.ml"
#mod_use "B.ml"
#mod_use "C.ml"
#mod_use "E.ml"
```

### using -beta option
If your OCaml interpreter is old and an error occurs while reading
your .ocamlinit, like:
```
Cannot find file topfind.
Unknown directive `require'.
```
You can remove the two lines from your .ocamlinit
```
#use "topfind"
#require "camlp4.listcomprehension"
```
and put the newline at top of your .ocamlinit:
```
#load "dynlink.cma"
```
