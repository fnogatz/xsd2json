:- encoding(utf8).
:- use_module(xsd2json).
:- use_module(library(http/json)).

:- initialization main.

from_stream :- 
  xsd2json(stream(user_input),JSON), 
  json_write(user_output,JSON).

main :- from_stream.
