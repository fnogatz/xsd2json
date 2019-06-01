:- module(merge_json, [ merge_json/3, merge_json/4, merge_facets/3, merge_facet/4 ]).
:- use_module(helpers).


/**
 * merge_json/4
 * merge_json(JSON1,JSON2,Merged,On_Conflict)
 *
 * Merge two JSON values.
 *
 * On_Conflict:
 *   - 0/hard: fail
 *   - 9/soft: rename keys in objects if they occur multiple times
 *
 * Exampes:
 *   merge_json(json([type=1]), json([type=1]), json([type=1]), _).
 *   merge_json(json([type=1]), json([type=2]), Merged, soft).
 */
merge_json(JSON1,JSON2,Merged,soft) :- merge_json(JSON1,JSON2,Merged,9).
merge_json(JSON1,JSON2,Merged,hard) :- merge_json(JSON1,JSON2,Merged,0).

merge_json(JSON1,JSON2,_Merged,_On_Conflict) :- (var(JSON1); var(JSON2)), !, false.

merge_json(json([]),json(JSON_List2),json(JSON_List2),_On_Conflict) :- !.
merge_json(json(JSON_List1),json([]),json(JSON_List1),_On_Conflict) :- !.

merge_json(json(['$ref'=Ref]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Avoid combining "$ref" with other properties (c.f. http://json-schema.org/latest/json-schema-core.html#rfc.section.7)
  AllOf = json([
    allOf= [
      json([
        '$ref'= Ref
      ])
    ]
  ]),
  merge_json(AllOf,json(JSON_List2),json(Merged),On_Conflict).
merge_json(json(JSON_List1),json(['$ref'=Ref]),json(Merged),On_Conflict) :-
  % Avoid combining "$ref" with other properties (c.f. http://json-schema.org/latest/json-schema-core.html#rfc.section.7)
  AllOf = json([
    allOf= [
      json([
        '$ref'= Ref
      ])
    ]
  ]),
  merge_json(json(JSON_List1),AllOf,json(Merged),On_Conflict).

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key also exists in JSON_List2
  lookup(Key,JSON_List2,Value2,JSON2_Without_Key),
  (
      Value == Value2,
      New_Value = Value
    ;
      % If `Key` is `description` simply concat the string values
      %   by using a \n separator.
      Key == description,
      atomic(Value),
      atomic(Value2),
      string_concat(Value,'\n',ValueNl),
      string_concat(ValueNl,Value2,New_Value)
  ),
  merge_json(json(Rest_JSON_List1),json(JSON2_Without_Key),json(Rest_Merged),On_Conflict),
  Merged = [Key=New_Value|Rest_Merged].

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key also exists in JSON_List2 and value is no atom
  lookup(Key,JSON_List2,Value_in_JSON_List2,JSON2_Without_Key),
  \+atom(Value),
  (
      % If `Key` is `required` or `enum` use union instead of
      %   the merge_json/3 predicate which would result
      %   in an append of both lists.
      % This might be necessary due to different orders to
      %  apply the CHR rules.
      (Key == required; Key == enum),
      !,
      union(Value,Value_in_JSON_List2,Merged_Value)
    ;
      Key == facets,
      !,
      merge_facets(Value,Value_in_JSON_List2,Merged_Value)
    ;
      merge_json(Value,Value_in_JSON_List2,Merged_Value)
  ),
  % merge the rest of the lists independently of the
  %   current key
  merge_json(json(Rest_JSON_List1),json(JSON2_Without_Key),json(Rest_Merged),On_Conflict),
  Merged = [Key=Merged_Value|Rest_Merged].

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key also exists in JSON_List2 and value is no atom
  lookup(Key,JSON_List2,_Value_in_JSON_List2),
  \+atom(Value),
  % couldn't be merged --> rename
  On_Conflict == 9,
  string_concat('@',Key,New_Key),
  merge_json(json([New_Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict).

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key does not exist in JSON_List2
  \+lookup(Key,JSON_List2,_),
  merge_json(json(Rest_JSON_List1),json(JSON_List2),json(Rest_Merged),On_Conflict),
  Merged = [Key=Value|Rest_Merged].

merge_json(List1,List2,Merged_List,_On_Conflict) :-
  is_list(List1), is_list(List2),
  append(List1,List2,Merged_List).


/**
 * merge_json/3
 * merge_json(JSON1,JSON2,Merged)
 *
 * Merge two JSON values.
 * Predicate won't be true for nonvar(JSON1) or nonvar(JSON2).
 * JSON1 and JSON2 are either both of form
 *   `json(_)` or a list (for array values).
 */
% Uncomment for test: 
% merge_json(JSON1,JSON2,_) :- write(JSON1), nl, write(JSON2), nl,nl,nl, false.
merge_json(JSON1,JSON2,JSON) :-
  merge_json(JSON1,JSON2,JSON,hard).


/**
 * merge_facets/3.
 */
merge_facets(json([]),json(JSON_List2),json(JSON_List2)).
merge_facets(json(JSON_List1),json([]),json(JSON_List1)) :- JSON_List1 \= [].
merge_facets(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(JSON)) :-
  lookup(Key,JSON_List2,Value_in_JSON_List2,JSON2_Without_Key),
  merge_facet(Key,Value,Value_in_JSON_List2,Merged_Value),
  merge_facets(json(Rest_JSON_List1),json(JSON2_Without_Key),json(Rest_Merged)),
  JSON = [Key=Merged_Value|Rest_Merged].
merge_facets(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(JSON)) :-
  \+lookup(Key,JSON_List2,_Value_in_JSON_List2),
  merge_facets(json(Rest_JSON_List1),json(JSON_List2),json(Rest_Merged)),
  JSON = [Key=Value|Rest_Merged].
%merge_facets(A,B,_) :- writeln(A), writeln(B), !, false.


/**
 * merge_facet/4
 * merge_facet(Facet,Value1,Value2,Result_Value)
 */
merge_facet(minLength,A,B,A) :- A >= B.
merge_facet(minLength,A,B,B) :- A < B.
merge_facet(maxLength,A,B,A) :- A =< B.
merge_facet(maxLength,A,B,B) :- A > B.
merge_facet(minimum,A,B,A) :- A >= B.
merge_facet(minimum,A,B,B) :- A < B.
merge_facet(maximum,A,B,A) :- A =< B.
merge_facet(maximum,A,B,B) :- A > B.
merge_facet(pattern,A,B,R) :- string_concat(['(',A,'|',B,')'],R).
merge_facet(exclusiveMinimum,A,A,A).
merge_facet(exclusiveMaximum,A,A,A).
