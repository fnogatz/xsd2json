:- module(xsd2json, [ xsd2json/2, flatten_xsd/1 ]).
:- use_module(merge_json).
:- use_module(library(chr)).


/**
 * transform/0
 * [CHR-Constraint]
 *
 * Constraint to trigger the transformation process.
 */
:- chr_constraint transform/0.


/**
 * build_schema/0
 * [CHR-Constraint]
 *
 * Constraint to trigger the build process of the global
 *   JSON Schema, i.e. create inline JSON Schemas.
 */
:- chr_constraint build_schema/0.
build_schema
  \
    transform
  <=>
    true.


/**
 * json/2
 * json(ID,json(JSON))
 * [CHR-Constraint]
 *
 * The `json/2` constraints holds the `JSON` entity for the
 *   XSD object with the same `ID`.
 */
:- chr_constraint json/2.

/**
 * Merge two JSONs of the same `ID`.
 */
json(ID,JSON1), 
    json(ID,JSON2) 
  <=> 
    merge_json(JSON1,JSON2,JSON)
  |
    json(ID,JSON).


/**
 * json_created/2
 * json_created(ID,List_Of_Children_IDs)
 * [CHR-Constraint]
 *
 * The `json_created/2` holds a `List_Of_Children_IDs`
 *   of children nodes of the given `ID` for which
 *   already `json/2` constraints has been generated.
 */
:- chr_constraint json_created/2.

/**
 * Merge two `json_created/2` constraints of the same ID.
 */
json_created(ID,List1), 
    json_created(ID,List2)
  <=>
    union(List1,List2,Union),
    json_created(ID,Union).


/**
 * Propagate for each created JSON a `json_created/2`
 *   constraint for the parent node.
 */
json(ID,_JSON),
    node(_Namespace,_Name,ID,_Siblings,Parent_ID)
  ==>
    json_created(Parent_ID,[ID]).


/**
 * schema_definition/3
 * schema_definition(Schema_Name,ID,JSON)
 * [CHR-Constraint]
 *
 * Definition of a inline JSON Schema, assigned to
 *   the node of `ID`.
 */
:- chr_constraint schema_definition/3.

schema_definition(Name,ID,JSON1),
    schema_definition(Name,ID,JSON2)
  <=>
    merge_json(JSON1,JSON2,JSON)
  |
    schema_definition(Name,ID,JSON).


/**
 * node/5
 * node(Namespace,Name,ID,Children_IDs,Parent_ID)
 * [CHR-Constraint]
 *
 * Hold the node's information like its `Name`, `Namespace`,
 *   `ID` and children as well as the `Parent_ID` of its
 *   parent node.
 */
:- chr_constraint node/5.


/**
 * node_attribute/4
 * node_attribute(ID,Key,Value,Source)
 * [CHR-Constraint]
 *
 * Hold the attribute of the node of `ID`as a 
 *   `Key`-`Value`-pair. The `Source` is either `default` for
 *   attributes which were added as default values or `source`
 *   if it was set in the DOM.
 * Note: The `Value` is always a string.
 *
 * Examples:
 *   node_attribute(_Some_ID,minOccurs,'1',default).
 */
:- chr_constraint node_attribute/4.

% remove default attributes if the source already contains
%   it
node_attribute(ID,Key,_Value_Kept,source)
  \ 
    node_attribute(ID,Key,_Value_Removed,default) 
  <=>
    true.


/**
 * text_node/3
 * text_node(ID,Text,Parent_ID)
 * [CHR-Constraint]
 *
 * Representation of a XML text node like it is used
 *   in `<xs:documentation>My Documentation</xs:documentation>`
 */
:- chr_constraint text_node/3.


/**
 * namespace_uri/2
 * namespace_uri(Namespace,URI)
 * [dynamic Prolog]
 *
 * Hold the `URI` of a `Namespace`. Will be set dynamically by
 *   `register_namespace/3`.
 */
:- dynamic namespace_uri/2.


/**
 * parse_options/1
 * parse_options(List_Of_Options)
 * 
 * Define a `List_Of_Options` for parse a XSD file by use
 *   of the built-in `load_structure/3`.
 */
parse_options([
  % read in XML file with handling of namespaces in mind
  dialect(xmlns),

  % remove spaces before and after tags, ignore whitespace-only elements
  space(remove),
  
  % quiet namespace handling
  % if a node has a namespace (like `xs` in `xs:element`) which hasn't
  %   been declared via `xmlns:xs="...URI..."` this will suppress the
  %   error message
  xml_no_ns(quiet),

  % create a new `namespace/2` constraint for each namespace
  call(xmlns, register_namespace)
]).


/**
 * load_xml/2
 * load_xml(Input,XSD)
 *
 * Load a XSD file specified by `Input` and return the
 *   DOM tree in `XSD`.
 *
 * Examples:
 *   load_xsd(stream(user_input),XSD).  %% binds XSD to the DOM tree
 */
load_xsd(Input,XSD) :- 
  parse_options(Parse_Options), 
  load_structure(Input,XSD,Parse_Options).


/**
 * flatten_xsd/1
 * flatten_xsd(Input)
 *
 * Load a XSD file specified by `Input` and flatten
 *   its DOM tree. This will result in multple `node/5`
 *   and `text_node/2` constraints.
 *
 * Examples:
 *   flatten_xsd(stream(user_input)).
 */
flatten_xsd(Input) :- 
  load_xsd(Input,XSD),
  root_id(Root_ID),
  xsd_flatten_nodes(Root_ID,0,XSD,_Children_IDs).


/**
 * xsd2json/2
 * xsd2json(Input,JSON)
 *
 * Convert a XSD file specified in `Input` to the
 *   equivalent JSON Schema instance and bind the
 *   result on the `JSON` variable.
 */
xsd2json(Input,Result) :- 
  load_xsd(Input,XSD),
  root_id(Root_ID),
  xsd_flatten_nodes(Root_ID,0,XSD,Children_IDs),
  transform,
  Children_IDs = [First_Element|_],
  build_schema,
  get_json(First_Element,JSON),
  remove_at_from_property_names(JSON,Result).


/**
 * register_namespace/3
 * register_namespace(+NameSpace, +URL, +Parser)
 *
 * Predicate being called when a Prolog's built-in
 *   `load_structure/3` is called with the option
 *   `call(xmlns, register_namespace)` was set.
 * Creates a new `namespace/2` CHR constraint for
 *   each namespace in the XSD.
 */

% Namespace without URL --> must be default XSD Namespace
register_namespace(Namespace,Namespace,_Parser) :-
  asserta(namespace_uri(Namespace,'http://www.w3.org/2001/XMLSchema')).

% Namespace and URL given
register_namespace(Namespace,URL,_Parser) :-
  Namespace \== URL,
  asserta(namespace_uri(Namespace,URL)).


/**
 * namespace/3
 * namespace(Name,Namespace,Name_Without_Namespace)
 *
 * Predicate to split a Name into its Namespace and 
 * Name_Without_Namespace.
 *
 * Examples:
 *   namespace(xs:element,xs,element).
 *   namespace('xs:element',xs,element).
 *   namespace(element,_,element).
 */
namespace(Name,Namespace,Name_Without_NS) :-
  Name = Namespace:Name_Without_NS.
namespace(Name_String,Namespace,Name_Without_NS) :-
  atom(Name_String),
  term_to_atom(Name,Name_String),
  Name = Namespace:Name_Without_NS.
% no namespace present
namespace(Name,_,Name_Term) :-
  \+atom(Name),
  term_to_atom(Name_Term,Name).


/**
 * xsd_namespace/1
 * xsd_namespace(Namespace)
 *
 * Predicate which holds the standard namespaces for
 *   XML Schema instances.
 */
xsd_namespace('http://www.w3.org/2001/XMLSchema').
xsd_namespace(Namespace) :- 
  namespace_uri(Namespace,'http://www.w3.org/2001/XMLSchema').


/**
 * xsd_namespaces/1
 * xsd_namespaces(Namespaces)
 *
 * Predicate which is true if all of the items in the
 *   list satisfy the `xsd_namespace/1` predicate.
 *
 * Examples:
 *   xsd_namespaces([]).
 *   xsd_namespace(['http://www.w3.org/2001/XMLSchema']).
 */
xsd_namespaces([]).
xsd_namespaces([Namespace|Namespaces]) :- 
  xsd_namespace(Namespace), 
  xsd_namespaces(Namespaces).


/**
 * cast/3
 * cast(Type,In,Out)
 *
 * Predicate to cast between multiple types. This might
 *   be useful to convert into the right types before
 *   generating the JSON.
 * `Type` is the name of a JSON Schema primitive type.
 * An unknown `Type` is handled like a cast to itself.
 *
 * Examples:
 *   cast(string,3,'3').
 *   cast(number,'3',3).
 */
cast(number,I,O) :- to_number(I,O).
cast(integer,I,O) :- to_number(I,O).
% unknown cast
cast(_Other,I,O) :- I = O.


/**
 * cast_by_json/3
 * cast_by_json(JSON,In,Out)
 *
 * Examples:
 *   cast_by_json([type=integer],'3',3).
 *   cast_by_json([type=integer],3,3).
 *   cast_by_json([type=integer],3,3).
 */
/**
 * Uncomment following line for testing purposes:
 */
% cast_by_json(JSON,I,O) :- write('Cast JSON'), nl, write(JSON), nl, nl, I=O.
cast_by_json(json(JSON),I,O) :- cast_by_json(JSON,I,O).
cast_by_json(JSON,I,O) :-
  is_list(JSON),
  merge_json:lookup(type,JSON,Type,_Rest),
  cast(Type,I,O).


/**
 * to_number/2
 * to_number(Atom_Or_Number,Number)
 *
 * Get the atom or string or number as valid number.
 *
 * Examples:
 *   to_number('123',123).
 *   to_number(123,123).
 */
% is atom
to_number(In,Number) :- 
  atom(In), 
  atom_number(In,Number).
% is already number
to_number(Number,Number) :- 
  \+atom(Number), 
  Number.


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


/**
 * elength/1
 * elength(List_Of_Lists)
 *
 * Predicate which is true if all given lists are
 *   of the same length.
 *
 * Examples:
 *   elength([]).
 *   elength([[],[]]).
 *   elength([[1,2],[3,4],[5,6]]).
 */
elength([]).
elength(Ls) :- elength(Ls,_Length).


/**
 * elength/2
 * elength(List_Of_Lists,Length)
 *
 * Predicate which is true if all given lists in the
 *   `List_Of_Lists` are of the length `Length`.
 */
elength([Single_List],Length) :-
  length(Single_List,Length).
elength([L|Ls],Length) :-
  length(L,Length),
  elength(Ls,Length).


/**
 * remove_at_from_property_names/2
 * remove_at_from_property_names(JSON,New_JSON)
 *
 * Traverses through a JSON tree and removes
 *   all object keys with an `@` in it if
 *   there's no one without.
 */
remove_at_from_property_names(json([]),json([])).

remove_at_from_property_names(json(List),JSON) :-
  lookup(properties,List,json(Properties),List_Without_Properties),
  lookup(AtKey,Properties,AtKey_Value,Properties_Without_AtKey),
  mark_attribute(Mark),
  string_concat(Mark,Key_Str,AtKey),
  (
      lookup(Key_Str,Properties_Without_AtKey,Key_Value,Properties_Without_AtKey_And_Key),
      Key = Key_Str
    ;
      term_to_atom(Key,Key_Str),
      lookup(Key,Properties_Without_AtKey,Key_Value,Properties_Without_AtKey_And_Key)
  ),
  remove_at_from_property_names(json(Properties_Without_AtKey_And_Key),json(New_Properties_Without_AtKey_And_Key)),
  New_Properties = [AtKey=AtKey_Value,Key=Key_Value|New_Properties_Without_AtKey_And_Key],
  remove_at_from_property_names(json(List_Without_Properties),json(New_List_Without_Properties)),
  JSON = json([properties=json(New_Properties)|New_List_Without_Properties]).

remove_at_from_property_names(json(List),JSON) :-
  lookup(properties,List,json(Properties),List_Without_Properties),
  lookup(AtKey,Properties,AtKey_Value,Properties_Without_AtKey),
  mark_attribute(Mark),
  string_concat(Mark,Key,AtKey),
  \+lookup(Key,Properties_Without_AtKey,_Key_Value,_Properties_Without_AtKey_And_Key),
  remove_at_from_property_names(json(Properties_Without_AtKey),json(New_Properties_Without_AtKey)),
  New_Properties = [Key=AtKey_Value|New_Properties_Without_AtKey],
  remove_at_from_property_names(json(List_Without_Properties),json(New_List_Without_Properties)),
  JSON = json([properties=json(New_Properties)|New_List_Without_Properties]).

remove_at_from_property_names(json(List),JSON) :-
  List = [Key=Value|Rest],
  is_list(Value),
  remove_at_from_property_names(json(Rest),json(New_Rest)),
  JSON = json([Key=Value|New_Rest]).

remove_at_from_property_names(json(List),JSON) :-
  List = [Key=Value|Rest],
  Value \= json(_),
  remove_at_from_property_names(json(Rest),json(New_Rest)),
  JSON = json([Key=Value|New_Rest]).

remove_at_from_property_names(json(List),JSON) :-
  lookup(properties,List,json(Properties),List_Without_Properties),
  \+((lookup(AtKey,Properties,_AtKey_Value,_Properties_Without_AtKey),
    mark_attribute(Mark),
    string_concat(Mark,_Key_Str,AtKey))),
  remove_at_from_property_names(json(List_Without_Properties),json(New_List_Without_Properties)),
  (
      Properties == [],
      New_Properties = []
    ;
      Properties = [First=First_Value|Rest_Properties],
      remove_at_from_property_names(json(Rest_Properties),json(New_Rest_Properties)),
      (
          First_Value = json(_),
          remove_at_from_property_names(First_Value,New_First_Value)
        ;
          First_Value \= json(_),
          New_First_Value = First_Value
      ),
      New_Properties = [First=New_First_Value|New_Rest_Properties]
  ),
  JSON = json([properties=json(New_Properties)|New_List_Without_Properties]).

remove_at_from_property_names(json(List),JSON) :-
  \+lookup(properties,List,json(_Properties),_List_Without_Properties),
  List = [Key=json(Value)|Rest],
  remove_at_from_property_names(json(Rest),json(New_Rest)),
  remove_at_from_property_names(json(Value),json(New_Value)),
  JSON = json([Key=json(New_Value)|New_Rest]).

remove_at_from_property_names(JSON,H) :-
  var(H),
  write('  (1) Not found at remove_at_from_property_names/2: '), write(JSON), nl, nl.


/**
 * mark_attribute/1
 * mark_attribute(Mark)
 *
 * Hold the default `Mark` to flag attribute names in
 *   `xs:complexType`.
 */
mark_attribute('@').


/**
 * root_id/1
 * root_id(ID)
 *
 * Hold the default ID for the root element
 */
root_id([]).


/**
 * new_id/3
 * new_id(Parents_ID,Position,New_ID)
 *
 * Create a new ID by the parent's ID and the element's
 * position.
 */
new_id(Base_ID,Pos,ID) :-
  ID = [Pos|Base_ID].


/**
 * first_id/1
 * first_id(ID)
 *
 * Get the `ID` of the first root node regarding the naming
 *   scheme of `root_id/1` and `new_id/3`.
 */
first_id(ID) :-
  root_id(Root_ID),
  new_id(Root_ID,0,ID).


/**
 * xsd_flatten_nodes/4
 *
 * Flatten a XSD DOM tree by creating `node/4`,
 *   `attribute/`
 *   and `text_node/2` constraints.
 */
xsd_flatten_nodes(_Base_ID,_Pos,[],[]).

xsd_flatten_nodes(Base_ID,Pos,[Node|Nodes],[ID|Sibling_IDs]) :-
  Node = element(Node_Type,Node_Attributes,Child_Nodes),  %% is an XML node, no text
  new_id(Base_ID,Pos,ID),
  namespace(Node_Type,Namespace,Node_Type_Without_NS),
  % flatten the node's attributes
  xsd_flatten_attributes(ID,Node_Attributes),
  node(Namespace,Node_Type_Without_NS,ID,Children_IDs,Base_ID),
  % flatten sibling nodes
  Next_Pos is Pos+1,
  xsd_flatten_nodes(Base_ID,Next_Pos,Nodes,Sibling_IDs),
  % flatten all children
  xsd_flatten_nodes(ID,0,Child_Nodes,Children_IDs).

xsd_flatten_nodes(Base_ID,Pos,[Node|Nodes],[ID|Sibling_IDs]) :-
  atom(Node),  %% is simply a text node
  new_id(Base_ID,Pos,ID),
  text_node(ID,Node,Base_ID),
  % flatten sibling nodes
  Next_Pos is Pos+1,
  xsd_flatten_nodes(Base_ID,Next_Pos,Nodes,Sibling_IDs).


/**
 * xsd_flatten_attributes/2
 * xsd_flatten_attributes(ID,List_Of_Attributes)
 *
 * Flatten a `List_Of_Attributes` of the form
 *   [attribute1=valu1,attribute2=value2,...]
 *   by creating a `node_attribute/3` constraints.
 *
 * Examples:
 *   xsd_flatten_attributes([0],[minOccurs='1'])
 *     ==> node_attribute([0],minOccurs,'1')
 */
xsd_flatten_attributes(_ID,[]).
xsd_flatten_attributes(ID,[Attribute=Value|List_Of_Attributes]) :-
  node_attribute(ID,Attribute,Value,source),
  xsd_flatten_attributes(ID,List_Of_Attributes).


/**
 * valid_xsd_type/2
 * valid_xsd_type(Type_With_Namespace,Name)
 *
 * Predicate which is satisfiable if the given
 *   `Type_With_Namespace` is a valid XSD simple type.
 *   The type name without its namespace gets bound
 *   to `Name`.
 */
valid_xsd_type(Type_With_NS,Name) :-
  namespace(Type_With_NS,Namespace,Name),
  xsd_namespace(Namespace),
  xsd_type(Name).


/**
 * valid_xsd_type/1
 * valid_xsd_type(Type_With_Namespace)
 *
 * Predicate which is satisfiable if the given
 *   `Type_With_Namespace` is a valid XSD simple type.
 *   This uses `valid_xsd_type/2`.
 */
valid_xsd_type(Type_With_NS) :- valid_xsd_type(Type_With_NS,_).


/**
 * xsd_type/1
 * xsd_type(XSD_Type)
 *
 * Predicate to check whether the given `XSD_Type` is a known
 *   XSD simple type.
 * Uses the convert_xsd_type/2 predicate.
 *
 * Examples:
 *   xsd_type(string).
 *   xsd_type(positiveInteger).
 *   \+xsd_type(foobar).
 */
xsd_type(Type) :- convert_xsd_type(Type,_).


/**
 * convert_xsd_type/2
 * convert_xsd_type(XSD_Type,json(JSON_List))
 *
 * Translate a `XSD_Type` to its equivalent JSON Schema
 *   representation, specified as `json(JSON_List)`.
 */
convert_xsd_type(string,json([type=string])).
convert_xsd_type(boolean,json([type=boolean])).
convert_xsd_type(float,json([type=number])).
convert_xsd_type(double,json([type=number])).
convert_xsd_type(decimal,json([type=number])).
convert_xsd_type(integer,json([type=integer])).

convert_xsd_type(positiveInteger,json([type=integer,minimum=0,exclusiveMinimum= @(true)])).
convert_xsd_type(negativeInteger,json([type=integer,maximum=0,exclusiveMaximum= @(true)])).
convert_xsd_type(nonPositiveInteger,json([type=integer,maximum=0,exclusiveMaximum= @(false)])).
convert_xsd_type(nonNegativeInteger,json([type=integer,minimum=0,exclusiveMinimum= @(false)])).


/**
 * convert_xsd_restriction/3
 * convert_xsd_restriction(XSD_Constraint,Value,json(JSON_List))
 *
 * Translate the `XSD_Constraint` with its `Value` to the 
 *   equivalent JSON constraints specified in `json(JSON_List)`. 
 *   Additional type conversions (string to number) might be 
 *   done.
 */
% minExclusive
convert_xsd_restriction(minExclusive,Value,json(JSON_List)) :-
  to_number(Value,Number),
  JSON_List = [minimum=Number,exclusiveMinimum= @(true)].

% maxExclusive
convert_xsd_restriction(maxExclusive,Value,json(JSON_List)) :-
  to_number(Value,Number),
  JSON_List = [maximum=Number,exclusiveMaximum= @(true)].

% minInclusive
convert_xsd_restriction(minInclusive,Value,json(JSON_List)) :-
  to_number(Value,Number),
  JSON_List = [minimum=Number,exclusiveMinimum= @(false)].

% maxInclusive
convert_xsd_restriction(maxInclusive,Value,json(JSON_List)) :-
  to_number(Value,Number),
  JSON_List = [maximum=Number,exclusiveMaximum= @(false)].

% minLength
convert_xsd_restriction(minLength,Value,json(JSON_List)) :-
  to_number(Value,Number),
  integer(Number),  %% MUST be an integer, http://tools.ietf.org/html/draft-fge-json-schema-validation-00#section-5.2.2
  JSON_List = [minLength=Number].

% maxLength
convert_xsd_restriction(maxLength,Value,json(JSON_List)) :-
  to_number(Value,Number),
  integer(Number),  %% MUST be an integer, http://tools.ietf.org/html/draft-fge-json-schema-validation-00#section-5.2.1
  JSON_List = [maxLength=Number].

% length
convert_xsd_restriction(length,Value,json(JSON_List)) :-
  to_number(Value,Number),
  integer(Number),  %% MUST be an integer, http://tools.ietf.org/html/draft-fge-json-schema-validation-00#section-5.2.1
  JSON_List = [minLength=Number,maxLength=Number].

% pattern
convert_xsd_restriction(pattern,Value,json(JSON_List)) :-
  JSON_List = [pattern=Value].


/**
 * convert_xsd_restriction/2
 * convert_xsd_restriction(XSD_Constraint,JSON_Constraint)
 *
 * Translate the `XSD_Constraint` to the equivalent `JSON_Constraint`
 *   by use of the `convert_xsd_restriction/3` predicate.
 */
convert_xsd_restriction(XSD_Name,JSON_Name) :- 
  convert_xsd_restriction(XSD_Name,JSON_Name,_Type).


/**
 * reference_type/2
 * reference_type(Type,JSON)
 *
 * Create the `JSON` for the given `Type`. Either uses
 *   the `convert_xsd_type/2` predicate for XML built-in
 *   types or JSON Schema's `{ $ref: "#/definitions/..." }`
 *   referencing syntax.
 */
reference_type(Type_With_NS,Attribute_JSON) :-
  valid_xsd_type(Type_With_NS,Type),
  convert_xsd_type(Type,Attribute_JSON).

reference_type(Type_With_NS,Attribute_JSON) :-
  \+valid_xsd_type(Type_With_NS,_),
  string_concat('#/definitions/',Type_With_NS,Type_Ref),
  Attribute_JSON = json(['$ref'=Type_Ref]).


/**
 * sum_occurs/3
 * sum_occurs(Occurs_1,Occurs_2,Sum_Occurs)
 *
 * Sums up to `minOccurs` or `maxOccurs` values represented
 *   as strings.
 *
 * Examples:
 *   sum_occurs('1','1','2').
 *   sum_occurs('1','unbounded','unbounded').
 */
sum_occurs(unbounded,_,unbounded).
sum_occurs(_,unbounded,unbounded).
sum_occurs(A,B,Res) :- 
  to_number(A,An),
  to_number(B,Bn),
  Resn is An+Bn,
  term_to_atom(Resn,Res).


/**
 * remove_xsd_element/1
 * remove_xsd_element(Element_Name)
 */
node(Namespace,Name,_ID,_Children,_Parent_ID) 
  <=>
    remove_xsd_element(Name),
    xsd_namespace(Namespace)
  |
    true.


/**
 * remove_node/1
 * remove_node(ID)
 * [CHR-Constraint]
 *
 * Remove a `node/4` constraint and its `node_attribute/4`
 *   constraints as well as its children nodes.
 */
:- chr_constraint remove_node/1.
remove_node(ID)
  \ 
    node_attribute(ID,_,_,_) 
  <=> 
    true.

remove_node(ID)
  \ 
    node(Parent_Namespace,Parent_Name,Parent_ID,Siblings,Parent_Parent_ID) 
  <=> 
    member(ID,Siblings), 
    delete(Siblings,ID,Siblings_Without_ID)
  |
    node(Parent_Namespace,Parent_Name,Parent_ID,Siblings_Without_ID,Parent_Parent_ID).

remove_node(ID) \ json(ID,_) <=> true.
remove_node(ID) \ node(_,_,ID,Children,_) <=> remove_nodes(Children).


/**
 * remove_nodes/1
 * remove_nodes(List_Of_IDs)
 *
 * Remove a list of `node/4` constraints by use of
 *   the `remove_node/1` constraint.
 */
remove_nodes([]).
remove_nodes([ID|Rest]) :- 
  remove_node(ID),
  remove_nodes(Rest).


/**
 * ##### DO BEFORE transform/0 #####
 * 
 * Manipulations which has to be done before
 *   the conversion starts, i.e. before the `transform/0`
 *   constrains was added.
 */

/**
 * Combine multiple `xs:element` within a `xs:sequence`
 *   with the same `Name` and `Type`.
 */
node(NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node_attribute(Element_1_ID,name,Name,_),
    node_attribute(Element_1_ID,type,Type,_)
  \
    node(NS2,element,Element_1_ID,Element_1_Children,Sequence_ID),
    node(NS3,element,Element_2_ID,_Element_2_Children,Sequence_ID),
    node_attribute(Element_2_ID,name,Name,_),
    node_attribute(Element_2_ID,type,Type,_),
    node_attribute(Element_1_ID,minOccurs,MinOccurs_1,_),
    node_attribute(Element_1_ID,maxOccurs,MaxOccurs_1,_),
    node_attribute(Element_2_ID,minOccurs,MinOccurs_2,_),
    node_attribute(Element_2_ID,maxOccurs,MaxOccurs_2,_)
  <=>
    xsd_namespaces([NS1,NS2,NS3]),
    sum_occurs(MinOccurs_1,MinOccurs_2,MinOccurs),
    sum_occurs(MaxOccurs_1,MaxOccurs_2,MaxOccurs)
  |
    remove_node(Element_2_ID),
    node(NS1,element,Element_1_ID,Element_1_Children,Sequence_ID),
    node_attribute(Element_1_ID,minOccurs,MinOccurs,source),
    node_attribute(Element_1_ID,maxOccurs,MaxOccurs,source).


/**
 * Combine multiple `xs:pattern` within a `xs:restriction`.
 *
 * Following 
 *   http://www.w3.org/TR/xmlschema-2/#src-multiple-patterns
 *   they are ORed.
 */
node(NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID)
  \
    node(NS2,pattern,Pattern_1_ID,Pattern_1_Children,Restriction_ID),
    node(NS3,pattern,Pattern_2_ID,_Pattern_2_Children,Restriction_ID),
    node_attribute(Pattern_1_ID,value,Pattern_1,_),
    node_attribute(Pattern_2_ID,value,Pattern_2,_)
  <=>
    xsd_namespaces([NS1,NS2,NS3]),
    string_concat(['(',Pattern_1,'|',Pattern_2,')'],New_Pattern)
  |
    remove_node(Pattern_2_ID),
    node(NS2,pattern,Pattern_1_ID,Pattern_1_Children,Restriction_ID),
    node_attribute(Pattern_1_ID,value,New_Pattern,source).


/**
 * 
 */
%% node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
%%     json(ComplexType_ID,json(JSON))
%%   \
%%     node(NS2,attribute,Attribute_ID,_Attribute_Children,ComplexType_ID)
%%   <=>
%%     xsd_namespaces([NS1,NS2,NS3])
%%     merge_json:lookup(properties,)



/**
 * ## Set defaults ##
 *
 * Note: This will already be executed without the 
 *   `transform/0` constraint being added.
 */

/**
 * Add `minOccurs` and `maxOccurs` attribute to all 
 *   `xs:element` XSD nodes. Default is 1.
 *
 * Note: By definition (http://www.w3schools.com/schema/el_element.asp)
 *   `minOccurs` and `maxOccurs` must not be set for
 *   the element whose parent is the `schema` node.
 *   This can be ignored as it will have no effect for
 *   those elements.
 */
node(Namespace,element,Element_ID,_Element_Children,_Parent_ID) 
  ==>
    xsd_namespace(Namespace) 
  |
    node_attribute(Element_ID,minOccurs,'1',default).

node(Namespace,element,Element_ID,_Element_Children,_Parent_ID) 
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Element_ID,maxOccurs,'1',default).


/**
 * Add `use` attribute to all `xs:attribute` XSD nodes.
 *   Default is `optional`.
 */
node(Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Attribute_ID,use,optional,default).


/**
 * Add `fixed` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Fixed)`.
 */
node(Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Attribute_ID,fixed,_Unbound,default).


/**
 * Add `default` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Default)`.
 */
node(Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Attribute_ID,default,_Unbound,default).


/**
 * Add `id` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Value)`.
 *
 * This `id` isn't used yet as there is no equivalent in
 *   JSON Schema.
 */
node(Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Attribute_ID,id,_Unbound,default).


/**
 * Add `type` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Type)`.
 */
node(Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(Attribute_ID,type,_Unbound,default).


/**
 * ##########  XS:RESTRICTION  ##########
 */

/**
 * Every restriction has at least its base type in an `base=Base`
 *   attribute.
 */
transform, 
    node(Namespace,restriction,ID,_Children,_Parent_ID),
    node_attribute(ID,base,Base,_)
  ==>
    xsd_namespace(Namespace),
    namespace(Base,Base_Namespace,Base_Type),
    xsd_namespace(Base_Namespace),
    convert_xsd_type(Base_Type,JSON)
  |
    json(ID,JSON).


/**
 * ##########  XS:ENUMERATION  ##########
 */

/**
 * `xs:enumeration` element as child of a `xs:restriction` node.
 */
transform, 
    node(NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID),
    node(NS2,enumeration,Enumeration_ID,_Enumeration_Children,Restriction_ID),
    node_attribute(Enumeration_ID,value,Value,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(Restriction_ID,json([enum=[Value]])).


/**
 * ##########  XS:MINEXCLUSIVE  ##########
 * ##########  XS:MAXEXCLUSIVE  ##########
 * ##########  XS:MININCLUSIVE  ##########
 * ##########  XS:MAXINCLUSIVE  ##########
 * ##########  XS:MINLENGTH  ##########
 * ##########  XS:MAXLENGTH  ##########
 * ##########  XS:LENGTH  ##########
 * ##########  XS:PATTERN ###########
 *
 * As child of a `xs:restriction` node.
 * 
 * Currently those items are all interpreted as numbers
 *   while the specification also mentions date and time
 *   facets.
 */
transform, 
    node(NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID),
    node(NS2,Constraint_Name_XSD,Constraint_ID,_Constraint_Children,Restriction_ID),
    node_attribute(Constraint_ID,value,Value,_)
  ==>
    xsd_namespaces([NS1,NS2]),
    convert_xsd_restriction(Constraint_Name_XSD,Value,json(JSON_List))
  |
    json(Restriction_ID,json(JSON_List)).


/**
 * ##########  XS:ELEMENT  ##########
 */

/**
 * `xs:element` with `fixed` attribute set.
 */
transform,
    node(Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(ID,fixed,Fixed,_)
  ==>
    xsd_namespace(Namespace)
  |
    json(ID,json([enum=[Fixed]])).


/**
 * `xs:element` with `type` attribute set which can be
 *   translated to a primitive JSON Schema type.
 */
transform,
    node(Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(ID,type,Type_With_NS,_)
  ==>
    xsd_namespace(Namespace),
    valid_xsd_type(Type_With_NS,Type)
  |
    convert_xsd_type(Type,JSON),
    json(ID,JSON).


/**
 * `xs:element` with `type` attribute set which must be
 *   self defined.
 */
transform,
    node(Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(ID,type,Type_With_NS,_)
  ==>
    xsd_namespace(Namespace),
    \+valid_xsd_type(Type_With_NS,_Type)
  |
    json(ID,json([type=Type_With_NS])).


/**
 * ##########  XS:ANNOTATION  ##########
 * ##########  XS:DOCUMENTATION  ##########
 */

/**
 * Convert a `xs:annotation/xs:documentation` within a
 *   `xs:element` node.
 */
transform,
    node(NS1,element,Element_ID,_Element_Children,_Element_Parent_ID),
    node(NS2,annotation,Annotation_ID,_Annotation_Children,Element_ID),
    node(NS3,documentation,Documentation_ID,_Documentation_Children,Annotation_ID),
    text_node(_Text_Node_ID,Text,Documentation_ID)
  ==> 
    xsd_namespaces([NS1,NS2,NS3])
  |
    json(Element_ID,json([description=Text])).


/**
 * Convert a `xs:annotation/xs:documentation` within a
 *   `xs:simpleType` node which has a `@name` attribute set.
 */
transform,
    node(NS1,simpleType,SimpleType_ID,_SimpleType_Children,_SimpleType_Parent_ID),
    node_attribute(SimpleType_ID,name,_Name,_),
    node(NS2,annotation,Annotation_ID,_Annotation_Children,SimpleType_ID),
    node(NS3,documentation,Documentation_ID,_Documentation_Children,Annotation_ID),
    text_node(_Text_Node_ID,Text,Documentation_ID)
  ==> 
    xsd_namespaces([NS1,NS2,NS3])
  |
    json(SimpleType_ID,json([description=Text])).


/**
 * ##########  XS:TOTALDIGITS  ##########
 * ##########  XS:FRACTIONDIGITS  ##########
 *
 * Those items are not supported in JSON Schema as
 *   JavaScript has no restrictions in the number of
 *   digits of a floating point number.
 */
remove_xsd_element(totalDigits).
remove_xsd_element(fractionDigits).


/**
 * ##########  XS:WHITESPACE  ##########
 *
 * The `xs:whiteSpace` restriction is just a hint for the
 *   XML processor how to handle whitespaces, no validation
 *   rules are associated (http://www.w3.org/TR/xmlschema-2/#element-whiteSpace).
 * As there is no equivalent in JSON Schema, it gets
 *   removed.
 */
remove_xsd_element(whiteSpace).


/**
 * ##########  XS:ALL  ##########
 */

/**
 * is_required_property/2
 * is_required_property(MinOccurs,MaxOccurs)
 *
 * Predicate which determines whether a JSON property is
 *   required, i.e. has to be added to the object's `required`
 *   array, or not, regarding its `MinOccurs` and `MaxOccurs`
 *   values.
 */
is_required_property('1',_).


/**
 * `xs:all/xs:element` DOM tree. The JSON object's
 *   `required` is set with respect to the `MinOccurs` (0|1 by 
 *   definition) and `MaxOccurs` (1) values by use of the 
 *   `is_required_property/2` predicate.
 */
transform, 
    node(NS1,all,All_ID,_All_Children,_All_Parent_ID),
    node(NS2,element,Element_ID,_Element_Children,All_ID),
    json(Element_ID,Element_JSON),
    node_attribute(Element_ID,minOccurs,MinOccurs,_),
    node_attribute(Element_ID,maxOccurs,MaxOccurs,_),
    node_attribute(Element_ID,name,Element_Name,_) 
  ==>
    xsd_namespaces([NS1,NS2])
  |
    Properties = [Element_Name=Element_JSON],
    JSON = [type=object,properties=json(Properties)],
    (
        is_required_property(MinOccurs,MaxOccurs), 
        Full_JSON = [required=[Element_Name]|JSON]
      ;
        \+is_required_property(MinOccurs,MaxOccurs), 
        Full_JSON = JSON
    ),
    json(All_ID,json(Full_JSON)).


/**
 * ##########  XS:SEQUENCE  ##########
 */

/**
 * `xs:element` within a `xs:sequence` with the `maxOccurs` attribute
 *   set to '1' and `minOccurs` to '0' or '1'.
 */
transform,
    node(NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node(NS2,element,Element_ID,_Element_Children,Sequence_ID),
    json(Element_ID,Element_JSON),
    node_attribute(Element_ID,minOccurs,MinOccurs,_),
    node_attribute(Element_ID,maxOccurs,'1',_),
    node_attribute(Element_ID,name,Element_Name,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    Properties = [Element_Name=Element_JSON],
    JSON = [type=object,properties=json(Properties)],
    % add `required=[...]` if `minOccurs` = 1
    (MinOccurs = '0', Full_JSON = JSON;
      MinOccurs = '1', Full_JSON = [required=[Element_Name]|JSON]),
    json(Sequence_ID,json(Full_JSON)).


/**
 * `xs:element` within a `xs:sequence` with the `maxOccurs` attribute
 *   set to 'unbounded' or >= 2.
 */
transform,
    node(NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node(NS2,element,Element_ID,_Element_Children,Sequence_ID),
    json(Element_ID,Element_JSON),
    node_attribute(Element_ID,minOccurs,MinOccurs,_),
    node_attribute(Element_ID,maxOccurs,MaxOccurs,_),
    node_attribute(Element_ID,name,Element_Name,_)
  ==>
    xsd_namespaces([NS1,NS2]),
    to_number(MinOccurs,MinOccurs_Number),
    (
        MaxOccurs == unbounded
      ;
        to_number(MaxOccurs,MaxOccurs_Number), 
        MaxOccurs_Number >= 2
    )
  |
    Array_JSON = [
      type=array,
      items=Element_JSON,
      minItems=MinOccurs_Number
    ],
    % add `maxItems=...` if not 'unbounded'
    (
        MaxOccurs == unbounded, 
        Array_JSON_With_MaxItems = Array_JSON
      ;
        MaxOccurs \== unbounded, 
        Array_JSON_With_MaxItems = [maxItems=MaxOccurs_Number|Array_JSON]
    ),
    Properties = [Element_Name=json(Array_JSON_With_MaxItems)],
    JSON = [
      type=object,
      properties=json(Properties)],
    % add `required=[...]` if `minOccurs` > 0
    (MinOccurs_Number >= 1, Full_JSON = [required=[Element_Name]|JSON];
      MinOccurs_Number < 1, Full_JSON = JSON),
    json(Sequence_ID,json(Full_JSON)).


/**
 * ##########  XS:COMPLEXTYPE  ##########
 */

/**
 * `xs:complexType` which has a `xs:all` child.
 */
transform,
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(NS2,all,All_ID,_All_Children,ComplexType_ID),
    json(All_ID,All_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(ComplexType_ID,All_JSON).


/**
 * `xs:complexType` which has a `xs:sequence` child.
 */
transform,
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(NS2,sequence,Sequence_ID,_Sequence_Children,ComplexType_ID),
    json(Sequence_ID,Sequence_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(ComplexType_ID,Sequence_JSON).


/**
 * ##########  XS:ATTRIBUTE  ##########
 */

/**
 * `xs:complexType` which has an attribute with `@type`
 *   attribute being set, i.e. `\+var(Type)`.
 *
 * Not supported attributes of this object as having no
 *   result to the created JSON:
 *   - form
 *   - id
 */
transform,
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(NS2,attribute,Attribute_ID,_Attribute_Children,ComplexType_ID),
    node_attribute(Attribute_ID,name,Attribute_Name,_),
    node_attribute(Attribute_ID,type,Type_With_NS,_),
    node_attribute(Attribute_ID,use,Use,_),
    node_attribute(Attribute_ID,fixed,Fixed,_),
    node_attribute(Attribute_ID,default,Default,_)
  ==>
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2]),
    reference_type(Type_With_NS,json(Attribute_JSON))
  |
    % check `fixed` entity
    (
        var(Fixed),
        Attribute_JSON2 = Attribute_JSON
      ;
        \+var(Fixed),
        cast_by_json(Attribute_JSON,Fixed,Fixed_Casted),
        Attribute_JSON2 = [enum=[Fixed_Casted]|Attribute_JSON]
    ),
    % check `default` entity
    (
        var(Default),
        Attribute_JSON3 = Attribute_JSON2
      ;
        /**
         * As mentioned in the XSD specification not both
         *   `fixed` and `enum` can be set.
         */
        \+var(Default),
        var(Fixed),      % see explanation above
        cast_by_json(Attribute_JSON2,Default,Default_Casted),
        Attribute_JSON3 = [default=Default_Casted|Attribute_JSON2]
    ),
    % generate JSON
    mark_attribute(Mark),
    string_concat(Mark,Attribute_Name,Attribute_Name2),
    JSON1 = [
      type=object,
      properties=json([
        Attribute_Name2=json(Attribute_JSON3)
      ])
    ],
    % check `required` entity
    (
        Use == required,
        JSON2 = [required=[Attribute_Name]|JSON1]
      ;
        Use \= required,
        JSON2 = JSON1
    ),
    json(ComplexType_ID,json(JSON2)).


/**
 * `xs:complexType` which has an attribute with an 
 *   inline type definition, i.e. `var(Type)` and an
 *   `xs:simpleType` child node.
 *
 * This rule will be called once the JSON for the inner 
 *   `xs:simpleType` has been generated.
 */
transform,
    node(NS3,simpleType,SimpleType_ID,_SimpleType_Children,Attribute_ID),
    json(SimpleType_ID,json(SimpleType_JSON)),
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(NS2,attribute,Attribute_ID,_Attribute_Children,ComplexType_ID),
    node_attribute(Attribute_ID,name,Attribute_Name,_),
    node_attribute(Attribute_ID,type,Unbound_Type,_),
    node_attribute(Attribute_ID,use,Use,_),
    node_attribute(Attribute_ID,fixed,Fixed,_),
    node_attribute(Attribute_ID,default,Default,_)
  ==>
    var(Unbound_Type),
    xsd_namespaces([NS1,NS2,NS3])
  |
    Attribute_JSON = SimpleType_JSON,
    % check `fixed` entity
    (
        var(Fixed),
        Attribute_JSON2 = Attribute_JSON
      ;
        \+var(Fixed),
        cast_by_json(Attribute_JSON,Fixed,Fixed_Casted),
        Attribute_JSON2 = [enum=[Fixed_Casted]|Attribute_JSON]
    ),
    % check `default` entity
    (
        var(Default),
        Attribute_JSON3 = Attribute_JSON2
      ;
        /**
         * As mentioned in the XSD specification not both
         *   `fixed` and `enum` can be set.
         */
        \+var(Default),
        var(Fixed),      % see explanation above
        cast_by_json(Attribute_JSON2,Default,Default_Casted),
        Attribute_JSON3 = [default=Default_Casted|Attribute_JSON2]
    ),
    mark_attribute(Mark),
    string_concat(Mark,Attribute_Name,Attribute_Name2),
    JSON1 = [
      type=object,
      properties=json([
        Attribute_Name2=json(Attribute_JSON3)
      ])
    ],
    % check `required` entity
    (
        Use == required,
        JSON2 = [required=[Attribute_Name]|JSON1]
      ;
        Use \= required,
        JSON2 = JSON1
    ),
    json(ComplexType_ID,json(JSON2)).


/**
 * `xs:complexType` which has an attribute with `@ref`
 *   attribute being set.
 *
 * As specified, `@name` and `@type` can not be both 
 *   present.
 */
transform,
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(NS2,attribute,Attribute_ID,_Attribute_Children,ComplexType_ID),
    node_attribute(Attribute_ID,ref,Ref,_),
    node_attribute(Attribute_ID,use,Use,_),
    node_attribute(Attribute_ID,fixed,_Fixed,_),
    node_attribute(Attribute_ID,default,_Default,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    string_concat('#/definitions/@',Ref,Definition_Ref),
    Attribute_JSON = [
      '$ref'=Definition_Ref
    ],
    JSON1 = [
      type=object,
      properties=json([
        Ref=json(Attribute_JSON)
      ])
    ],
    % check `required` entity
    (
        Use == required,
        JSON2 = [required=[_Attribute_Name]|JSON1]
      ;
        Use \= required,
        JSON2 = JSON1
    ),
    json(ComplexType_ID,json(JSON2)).


/**
 * `xs:complexType` within a `xs:schema` node and having
 *   an attribute with `@type` attribute being set, i.e. 
 *   `\+var(Type)`. This is a global defined type and 
 *   therefore propagates a new `schema_definition`
 *   constraint with @`Name` as name.
 */
transform,
    node(NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(NS2,attribute,Attribute_ID,_Attribute_Children,Schema_ID),
    node_attribute(Attribute_ID,name,Attribute_Name,_),
    node_attribute(Attribute_ID,type,Type_With_NS,_),
    node_attribute(Attribute_ID,fixed,Fixed,_),
    node_attribute(Attribute_ID,default,Default,_)
  ==>
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2]),
    reference_type(Type_With_NS,json(Attribute_JSON))
  |
    % check `fixed` entity
    (
        var(Fixed),
        Attribute_JSON2 = Attribute_JSON
      ;
        \+var(Fixed),
        cast_by_json(Attribute_JSON,Fixed,Fixed_Casted),
        Attribute_JSON2 = [enum=[Fixed_Casted]|Attribute_JSON]
    ),
    % check `default` entity
    (
        var(Default),
        Attribute_JSON3 = Attribute_JSON2
      ;
        /**
         * As mentioned in the XSD specification not both
         *   `fixed` and `enum` can be set.
         */
        \+var(Default),
        var(Fixed),      % see explanation above
        cast_by_json(Attribute_JSON2,Default,Default_Casted),
        Attribute_JSON3 = [default=Default_Casted|Attribute_JSON2]
    ),
    string_concat('@',Attribute_Name,Definition_Name),
    schema_definition(Definition_Name,Attribute_ID,json(Attribute_JSON3)).


/**
 * ##########  XS:SIMPLETYPE  ##########
 */

/**
 * `xs:simpleType` which has a `xs:restriction` child.
 */
transform,
    node(NS1,simpleType,SimpleType_ID,_SimpleType_Children,_SimpleType_Parent_ID),
    node(NS2,restriction,Restriction_ID,_Restriction_Children,SimpleType_ID),
    json(Restriction_ID,Restriction_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(SimpleType_ID,Restriction_JSON).


/**
 * ##########  XS:SCHEMA  ##########
 */
transform,
    node(NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(NS2,element,Element_ID,_Element_Children,Schema_ID),
    json(Element_ID,Element_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(Schema_ID,Element_JSON).


/**
 * Create `schema_definition/3` constraint for each 
 *   `complexType` node which has the `@name` attribute
 *   set.
 */
transform,
    node(NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    json(ComplexType_ID,ComplexType_JSON),
    node_attribute(ComplexType_ID,name,Definition_Name,_)
  ==>
    xsd_namespaces([NS1])
  |
    schema_definition(Definition_Name,ComplexType_ID,ComplexType_JSON).


/**
 * Create `schema_definition/3` constraint for each 
 *   `simpleType` node which has the `@name` attribute
 *   set.
 */
transform,
    node(NS1,simpleType,SimpleType_ID,_SimpleType_Children,_SimpleType_Parent_ID),
    json(SimpleType_ID,SimpleType_JSON),
    node_attribute(SimpleType_ID,name,Definition_Name,_)
  ==>
    xsd_namespaces([NS1])
  |
    schema_definition(Definition_Name,SimpleType_ID,SimpleType_JSON).


/**
 * ##########  ON BUILD_SCHEMA  ##########
 */

build_schema, 
    schema_definition(Name,_ID,Inline_Schema),
    node(Namespace,schema,Schema_ID,_Schema_Children,_)
  ==>
    xsd_namespace(Namespace),
    first_id(Schema_ID)
  |
    json(Schema_ID,json([definitions=json([Name=Inline_Schema])])).


/**
 * ##########  ON FINISH  ##########
 */

/**
 * get_json/2
 * get_json(ID,Unbound_Var)
 *
 * Bind a variable to the content of the json/2 Constraint 
 * with the same ID.
 * Usually used when finished to get the root's JSON.
 */
:- chr_constraint get_json/2.

json(ID,JSON)
  \
    get_json(ID,Result) 
  <=> 
    JSON = Result.