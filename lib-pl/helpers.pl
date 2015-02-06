:- module(helpers, [ string_concat/2 ]).


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
string_concat([A,B|Bs],Result) :- string_concat(A,B,Temp), string_concat([Temp|Bs],Result).
