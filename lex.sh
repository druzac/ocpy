#!/bin/bash

ocaml first_pass.ml < $1 | ocaml tokenize.ml
