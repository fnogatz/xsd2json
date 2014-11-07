:- encoding(utf8).
:- module(merge_json, [ merge_json/3, merge_json/4, lookup/4 ]).

/**
 * merge_json/4
 * merge_json(JSON1,JSON2,Merged,On_Conflict)
 *
 * Merge two JSON values.
 *
 * On_Conflict:
 *   - 0/hard: fail
 *   - 9/soft: rename keys in objects if the occure multiple
 *           times
 *
 * Exampes:
 *   merge_json(json([type=1]), json([type=1]), json([type=1]), _).
 *   merge_json(json([type=1]), json([type=2]), Merged, soft).
 */
merge_json(JSON1,JSON2,Merged,soft) :- merge_json(JSON1,JSON2,Merged,9).
merge_json(JSON1,JSON2,Merged,hard) :- merge_json(JSON1,JSON2,Merged,0).

merge_json(JSON1,JSON2,_Merged,_On_Conflict) :- (var(JSON1); var(JSON2)), !, false.

merge_json(json([]),json(JSON_List2),json(JSON_List2),_On_Conflict).

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key also exists in JSON_List2 and value is equal
  lookup(Key,JSON_List2,Value,JSON2_Without_Key),
  merge_json(json(Rest_JSON_List1),json(JSON2_Without_Key),json(Rest_Merged),On_Conflict),
  Merged = [Key=Value|Rest_Merged].

merge_json(json([Key=Value|Rest_JSON_List1]),json(JSON_List2),json(Merged),On_Conflict) :-
  % Key also exists in JSON_List2 and value is no atom
  lookup(Key,JSON_List2,Value_in_JSON_List2,JSON2_Without_Key),
  \+atom(Value),
  % If `Key` is `required` or `enum` use union instead of
  %   the merge_json/3 predicate which would result
  %   in an append of both lists.
  % This might be necessary due to different orders to
  %  apply the CHR rules.
  (
      (Key == required; Key == enum), 
      union(Value,Value_in_JSON_List2,Merged_Value)
    ;
      Key \== required, 
      Key \== enum,
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
  New_Key = '@'Key,
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
 * lookup/3
 * lookup(Key,Key_Value_List,Value)
 * 
 * Predicate to search for a Key in a Key-Value-List.
 *
 * Examples:
 *   lookup(key,[key=value,key2=value2],value).
 *   lookup(key2,[key=value,key2=value2],Value2).  %% Value2 = value2.
 *   \+(lookup(key,[],Value)).
 */
lookup(Key,Store,Value) :- 
  lookup(Key,Store,Value,_).


/**
 * lookup/4
 * lookup(Key,Key_Value_List,Value,List_Without_This_Pair)
 * 
 * Search for a given Key in a Key-Value-List and return
 * the List without this pair.
 *
 * Examples:
 *   lookup(key,[key=value,key2=value2],value,[key2=value2]).
 *   lookup(key,[key=value],Value,Rest).  %% Value = value, Rest = [].
 */
lookup(Key,[Key=Value|Without_Key],Value,Without_Key).
lookup(Key,[Not_Key=Some_Value|Rest],Value,[Not_Key=Some_Value|Without_Key]) :- 
  Key \= Not_Key, 
  lookup(Key,Rest,Value,Without_Key).