#!/usr/bin/swipl -q -f

:- use_module(xsd2json).
:- use_module(library(http/json)).

main :-
  current_prolog_flag(argv, Argv),
  Argv = [Filename|ArgvRest],
  (
    ArgvRest = [trace|_],
    chr_leash(none),
    chr_trace,
    xsd2json(Filename,JSON)
  ;
    xsd2json(Filename,JSON)
  ),
  json_write(user_output,JSON),
  halt(0).
