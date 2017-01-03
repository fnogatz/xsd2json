:- module(helpers, [
  string_concat/2,
  html_to_string/2,
  xml_to_string/2,
  sgml_to_string/2
]).
:- meta_predicate markup_to_string(3, ?, ?).
:- use_module(library(sgml_write)).


/**
 * string_concat/2
 * string_concat(List_Of_Strings,Concatenated_String)
 *
 * Concatenates all strings of a given `List_Of_Strings` to
 *   `Concatenated_String` by use of the predefined 
 *   `string_concat/3`.
 *
 * Examples:
 *   string_concat(['a','b','c'], "abc").
 *   string_concat([a,b,c],"abc").
 */
string_concat([],'').
string_concat([A],A).
string_concat([A,B|Bs],Result) :-
  string_concat(A,B,Temp),
  string_concat([Temp|Bs],Result).


/**
 * switch/2
 * switch/1
 *
 * Predicate used to catch a Stream output.
 */
switch(B) :-
  telling(A),
  A=B, !.
switch(A) :-
  told,
  tell(A).

switch(A, B) :-
  telling(A),
  tell(B).


/**
 * sgml_to_string/2
 *
 * Create a string representation for the given SGML/XML
 *   structure using SWI-Prolog's sgml_write/2.
 * This creates a temporary file using tmp_file_stream/3,
 *   which is automatically deleted when the program halts.
 *
 * Examples:
 *   sgml_to_string(element(p,[],['Some']),"<p>Some</p>").
 */
sgml_to_string(SGML,String) :-
  Options = [
    header(false)
  ],
  markup_to_string(sgml_write,Options,SGML,String).

xml_to_string(XML,String) :-
  Options = [
    header(false)
  ],
  markup_to_string(xml_write,Options,XML,String).

html_to_string(HTML,String) :-
  dtd(html5,DTD),
  Options = [
    header(false),
    dtd(DTD)
  ],
  markup_to_string(html_write,Options,HTML,String).

markup_to_string(Writer,Options,Markup,String) :-
  tmp_file_stream(text,File,In),
  switch(S,In),
  call(Writer,In,Markup,Options),
  switch(S),
  open(File,read,Out),
  read_stream_to_codes(Out,Codes),
  string_to_list(String,Codes),
  close(Out).  
