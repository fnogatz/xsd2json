:- module(xsd2json, [
  xsd2json/2,
  xsd2json/3,
  flatten_xsd/1,
  version/1
]).
:- use_module(merge_json).
:- use_module(helpers).
:- use_module(library(chr)).
:- use_module(library(http/http_open)).
:- use_module(library(url)).
:- use_module(library(option)).
:- use_module(library(sgml)).


version(Version) :-
  predicate_property(version(_V),file(File)),
  absolute_file_name('../package.json',Package,[relative_to(File)]),
  open(Package,read,Stream),
  json_read(Stream,json(PackageJSON)),
  memberchk(version=Version,PackageJSON).


/**
 * transform/1
 * [CHR-Constraint]
 *
 * Constraint to trigger the transformation process.
 */
:- chr_constraint transform/1.


/**
 * build_schema/1
 * [CHR-Constraint]
 *
 * Constraint to trigger the build process of the global
 *   JSON Schema, i.e. create inline JSON Schemas.
 */
:- chr_constraint build_schema/1.
build_schema(IName)
  \
    transform(IName)
  <=>
    true.


/**
 * json/3
 * json(IName,ID,json(JSON))
 * [CHR-Constraint]
 *
 * The `json/3` constraints holds the `JSON` entity for the
 *   XSD object with the same `ID`.
 */
:- chr_constraint json/3.

/**
 * Merge two JSONs of the same `ID`.
 */
json(IName,ID,JSON1),
    json(IName,ID,JSON2) 
  <=>
    merge_json(JSON1,JSON2,JSON)
  |
    json(IName,ID,JSON).


/**
 * json_created/3
 * json_created(IName,ID,List_Of_Children_IDs)
 * [CHR-Constraint]
 *
 * The `json_created/3` holds a `List_Of_Children_IDs`
 *   of children nodes of the given `ID` for which
 *   already `json/2` constraints has been generated.
 */
:- chr_constraint json_created/3.

/**
 * Merge two `json_created/3` constraints of the same ID.
 */
json_created(IName,ID,List1),
    json_created(IName,ID,List2)
  <=>
    union(List1,List2,Union),
    json_created(IName,ID,Union).


:- chr_constraint xsd2json_result/2.


/**
 * Propagate for each created JSON a `json_created`
 *   constraint for the parent node.
 */
json(IName,ID,_JSON),
    node(IName,_Namespace,_Name,ID,_Siblings,Parent_ID)
  ==>
    json_created(IName,Parent_ID,[ID]).


/**
 * schema_definition/4
 * schema_definition(IName,Schema_Name,ID,JSON)
 * [CHR-Constraint]
 *
 * Definition of a inline JSON Schema, assigned to
 *   the node of `ID`.
 */
:- chr_constraint schema_definition/4.

schema_definition(IName,Name,ID,JSON1),
    schema_definition(IName,Name,ID,JSON2)
  <=>
    merge_json(JSON1,JSON2,JSON)
  |
    schema_definition(IName,Name,ID,JSON).


/**
 * node/6
 * node(IName,Namespace,Name,ID,Children_IDs,Parent_ID)
 * [CHR-Constraint]
 *
 * Hold the node's information like its `Name`, `Namespace`,
 *   `ID` and children as well as the `Parent_ID` of its
 *   parent node.
 */
:- chr_constraint node/6.


/**
 * node_attribute/5
 * node_attribute(IName,ID,Key,Value,Source)
 * [CHR-Constraint]
 *
 * Hold the attribute of the node of `ID`as a
 *   `Key`-`Value`-pair. The `Source` is either `default` for
 *   attributes which were added as default values or `source`
 *   if it was set in the DOM.
 * Note: The `Value` is always a string.
 *
 * Examples:
 *   node_attribute('file.xsd',_Some_ID,minOccurs,'1',default).
 */
:- chr_constraint node_attribute/5.

% remove default attributes if the source already contains
%   it
node_attribute(IName,ID,Key,_Value_Kept,source)
  \
    node_attribute(IName,ID,Key,_Value_Removed,default)
  <=>
    true.


/**
 * text_node/4
 * text_node(Input_Name,ID,Text,Parent_ID)
 * [CHR-Constraint]
 *
 * Representation of a XML text node like it is used
 *   in `<xs:documentation>My Documentation</xs:documentation>`
 */
:- chr_constraint text_node/4.


/**
 * default_parse_options/1
 * default_parse_options(List_Of_Options)
 *
 * Define a `List_Of_Options` for parse a XSD file by use
 *   of the built-in `load_structure/3`.
 */
default_parse_options([
  % read in XML file with handling of namespaces in mind
  dialect(xmlns),

  % remove spaces before and after tags, ignore whitespace-only elements
  space(preserve),

  % quiet namespace handling
  % if a node has a namespace (like `xs` in `xs:element`) which hasn't
  %   been declared via `xmlns:xs="...URI..."` this will suppress the
  %   error message
  xml_no_ns(quiet),

  % pass namespace as `ns(Prefix, URI)`
  keep_prefix(true)
]).

memberi(List,El) :-
  member(El,List).

parse_options(Opts,Parse_Options) :-
  default_parse_options(Default_Parse_Options),
  include(memberi([space(_)]),Opts,Opts_For_SGML),
  merge_options(Opts_For_SGML,Default_Parse_Options,Parse_Options).


/**
 * load_xsd/3
 * load_xsd(Input,Opts,XSD)
 *
 * Load a XSD file specified by `Input` and return the
 *   DOM tree in `XSD`.
 *
 * Examples:
 *   load_xsd(stream(user_input),XSD).  %% binds XSD to the DOM tree
 */
load_xsd(Input,Opts,XSD) :-
  parse_options(Opts,Parse_Options),
  (
      (
          string_concat('http://',_,Input)
        ;
          string_concat('https://',_,Input)
      ),
      http_open:http_open(Input,In,[])
    ;
      In = Input
  ),
  load_structure(In,XSD,Parse_Options).


/**
 * flatten_xsd/2
 * flatten_xsd(Input,Options)
 *
 * Load a XSD file specified by `Input` and flatten
 *   its DOM tree. This will result in multple `node/5`
 *   and `text_node/2` constraints.
 *
 * Examples:
 *   flatten_xsd(stream(user_input),[]).
 */
flatten_xsd(Input,Options) :-
  load_xsd(Input,Options,XSD),
  root_id(Root_ID),
  input_name(Input,Input_Name),
  Namespaces = [],
  xsd_flatten_nodes(Input_Name,Root_ID,0,XSD,Namespaces,_Children_IDs).


/**
 * flatten_xsd/1
 * flatten_xsd(Input)
 *
 * Load a XSD file specified by `Input` using
 *   no special options.
 *
 * Examples:
 *   flatten_xsd(stream(user_input)).
 */
flatten_xsd(Input) :-
  Options = [],
  flatten_xsd(Input,Options).


/**
 * xsd2json/3
 * xsd2json(Input,Options,JSON)
 *
 * Convert a XSD file specified in `Input` to the
 *   equivalent JSON Schema instance and bind the
 *   result on the `JSON` variable.
 */
xsd2json(Input,Options,Result) :-
  load_xsd(Input,Options,XSD),
  root_id(Root_ID),
  input_name(Input,Input_Name),
  Namespaces = [],
  xsd_flatten_nodes(Input_Name,Root_ID,0,XSD,Namespaces,Children_IDs),
  % chr_show_store(xsd2json), %% uncomment for debugging (before transformation)
  transform(Input_Name),
  % chr_show_store(xsd2json), %% uncomment for debugging (after transformation)
  Children_IDs = [First_Element|_],
  build_schema(Input_Name),
  get_json(Input_Name,First_Element,JSON),
  cleanup_json(JSON,Result),
  xsd2json_result(Input_Name,Result).


/**
 * xsd2json/2
 * xsd2json(Input,JSON)
 */
xsd2json(Input,Result) :-
  xsd2json(Input,[],Result).


/**
 * input_name/2.
 *
 * Create an Input_Name for a given input resource.
 */
input_name(stream(A),R) :-
  string_concat('stream_',A,R).
input_name(Input,Input) :-
  is_absolute_url(Input).
input_name(Filepath,Abs_Filepath) :-
  absolute_file_name(Filepath,Abs_Filepath).


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
  Name = Namespace:Name_Without_NS,
  !.
namespace(Name_String,Namespace,Name_Without_NS) :-
  atom(Name_String),
  atomic_list_concat([Namespace,Name_Without_NS],':',Name_String).
% no namespace present
namespace(Name,'',Name_Term) :-
  term_to_atom(Name_Term,Name).


xsd_namespace_uri('http://www.w3.org/2001/XMLSchema').


/**
 * xsd_namespace/1
 * xsd_namespace(Namespace)
 *
 * Predicate which holds the standard namespaces for
 *   XML Schema instances.
 */
xsd_namespace(ns(_,URI)) :-
  xsd_namespace_uri(URI).


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


no_xsd_namespace(Type_With_NS) :-
  (
    Type_With_NS = ns('',URI):_Type,
    \+xsd_namespace_uri(URI)
  ;
    atom(Type_With_NS)
  ).


/**
 * Add a namespace to all types defined in the `definitions`
 *   property.
 */
add_namespace(json(JSON),_NS,json(JSON)) :-
  \+lookup(definitions,JSON,_,_).

add_namespace(json(JSON),NS,json(JSON_With_NS)) :-
  lookup(definitions,JSON,json(Definitions),JSON_Without_Definitions),
  atom_concat(NS,':',Prefix),
  prefix_keys(Definitions,Prefix,Prefixed_Definitions),
  JSON_With_NS = [definitions=json(Prefixed_Definitions)|JSON_Without_Definitions].


/**
 * Add a prefix to all keys in a list of key-value-pairs.
 *
 * Examples:
 *   prefix_keys([a=3,b=1],u,[ua=3,ub=1]).
 */
prefix_keys([],_Prefix,[]).
prefix_keys([Key=Value|Rest],Prefix,[New_Key=Value|RestR]) :-
  atom_concat(Prefix, Key, New_Key),
  prefix_keys(Rest,Prefix,RestR).


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
cast_by_json(_Other,I,I) :- !.


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
 * relative_input/3
 *
 * Examples:
 *   relative_input('../test/a.xsd','b.xsd','../test/b.xsd').
 *   relative_input('http://localhost/a.xsd','b.xsd','http://localhost/b.xsd').
 */
relative_input(Base,Relative,Res) :-
  file_directory_name(Base,Dir),
  directory_file_path(Dir,Relative,Res).


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
 * cleanup_json/2
 */
cleanup_json(JSON,Result) :-
  remove_at_from_property_names(JSON,R1),
  resolve_facets(R1,R2),
  % add $schema property
  merge_json(R2, json(['$schema'='http://json-schema.org/draft-04/schema#']), R3),
  Result = R3.


/**
 * resolve_facets/2
 */
resolve_facets([],[]).

resolve_facets([H|T],[NH|NT]) :-
  resolve_facets(T,NT),
  resolve_facets(H,NH).

resolve_facets(json([]),json([])).

resolve_facets(json(List),json(JSON)) :-
  \+lookup(facets,List,json(_Properties),_List_Without_Properties),
  List = [Key=json(Value)|Rest],
  resolve_facets(json(Rest),json(New_Rest)),
  resolve_facets(json(Value),json(New_Value)),
  JSON = [Key=json(New_Value)|New_Rest].

resolve_facets(json(List),json(JSON)) :-
  \+lookup(facets,List,json(_Properties),_List_Without_Properties),
  List = [Key=Value|Rest],
  Value \= json(_),
  resolve_facets(json(Rest),json(New_Rest)),
  JSON = [Key=Value|New_Rest].

resolve_facets(json(List),json(JSON)) :-
  lookup(facets,List,json(Facets),List_Without_Facets),
  lookup(type,List_Without_Facets,Type,List_Wo_Facets_Type),
  merge_json(json([type= Type | Facets]), json(List_Wo_Facets_Type), json(JSON)).

resolve_facets(json(List),json(JSON)) :-
  lookup(facets,List,json(Facets),List_Without_Facets),
  lookup(base,List_Without_Facets,json(Base),List_Wo_Facets_Base),
  lookup(allOf,List_Wo_Facets_Base,AllOf,List_Wo_Facets_Base_AllOf),
  resolve_facets(AllOf,Resolved_AllOf),
  append(Resolved_AllOf,[json(Base),json(Facets)],New_AllOf),
  JSON = [
    allOf= New_AllOf
    | List_Wo_Facets_Base_AllOf
  ].

resolve_facets(json(List),json(JSON)) :-
  lookup(facets,List,json(Facets),List_Without_Facets),
  lookup(base,List_Without_Facets,json(Base),List_Wo_Facets_Base),
  \+lookup(allOf,List_Wo_Facets_Base,_AllOf,_),
  JSON = [
    allOf= [
      json(Base),
      json(Facets)
    ]
    | List_Wo_Facets_Base
  ].


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
 * xsd_flatten_nodes/6
 *
 * Flatten a XSD DOM tree by creating `node/5`,
 *   `attribute/`
 *   and `text_node/3` constraints.
 */
xsd_flatten_nodes(_IName,_Base_ID,_Pos,[],_Namespaces,[]).

xsd_flatten_nodes(IName,Base_ID,Pos,[Node|Nodes],Namespaces,[ID|Sibling_IDs]) :-
  % is an xs:documentation, so lax parsing
  Node = element(Node_Type,Node_Attributes,Child_Nodes),
  namespace(Node_Type,Namespace,Node_Type_Without_NS),
  xsd_namespace(Namespace),
  Node_Type_Without_NS = documentation,
  !,
  new_id(Base_ID,Pos,ID),
  new_id(ID,0,Text_Node_ID),
  % rebuild XML string
  html_to_string(Child_Nodes,String),
  node(IName,Namespace,Node_Type_Without_NS,ID,[Text_Node_ID],Base_ID),
  text_node(IName,Text_Node_ID,String,ID),
  % nevertheless flatten its attributes
  xsd_flatten_attributes(IName,ID,Node_Attributes,Namespaces),
  % flatten sibling nodes
  Next_Pos is Pos+1,
  xsd_flatten_nodes(IName,Base_ID,Next_Pos,Nodes,Namespaces,Sibling_IDs).

xsd_flatten_nodes(IName,Base_ID,Pos,[Node|Nodes],Namespaces,[ID|Sibling_IDs]) :-
  Node = element(Node_Type,Node_Attributes,Child_Nodes),  %% is an XML node, no text
  new_id(Base_ID,Pos,ID),
  namespace(Node_Type,Namespace,Node_Type_Without_NS),
  % check for defined xmlns namespaces
  get_defined_namespaces(Node_Attributes,Defined_Namespaces),
  merge_namespaces(Namespaces,Defined_Namespaces,Merged_Namespaces),
  % flatten the node's attributes
  xsd_flatten_attributes(IName,ID,Node_Attributes,Merged_Namespaces),
  % set this node
  node(IName,Namespace,Node_Type_Without_NS,ID,Children_IDs,Base_ID),
  % flatten sibling nodes
  Next_Pos is Pos+1,
  xsd_flatten_nodes(IName,Base_ID,Next_Pos,Nodes,Namespaces,Sibling_IDs),
  % flatten all children
  xsd_flatten_nodes(IName,ID,0,Child_Nodes,Merged_Namespaces,Children_IDs).

xsd_flatten_nodes(IName,Base_ID,Pos,[Node|Nodes],Namespaces,[ID|Sibling_IDs]) :-
  atom(Node),  %% is simply a text node
  new_id(Base_ID,Pos,ID),
  text_node(IName,ID,Node,Base_ID),
  % flatten sibling nodes
  Next_Pos is Pos+1,
  xsd_flatten_nodes(IName,Base_ID,Next_Pos,Nodes,Namespaces,Sibling_IDs).


/**
 * xsd_flatten_attributes/4
 * xsd_flatten_attributes(IName,ID,List_Of_Attributes,Namespaces)
 *
 * Flatten a `List_Of_Attributes` of the form
 *   [attribute1=value1,attribute2=value2,...]
 *   by creating `node_attribute/4` constraints.
 * For @type attributes in the given XSD-Namespaces it adds
 *   the namespace as `ns(Prefix,URI):` prefix.
 *
 * Examples:
 *   xsd_flatten_attributes('file.xsd',[0],[minOccurs='1'],[])
 *     ==> node_attribute('file.xsd',[0],minOccurs,'1')
 *   xsd_flatten_attributes('file.xsd',[0],[type=string],[ns('','http://www.w3.org/2001/XMLSchema')])
 *     ==> node_attribute('file.xsd',[0],type,ns('','http://www.w3.org/2001/XMLSchema'):string)
 */
xsd_flatten_attributes(_IName,_ID,[],_Namespaces).
xsd_flatten_attributes(IName,ID,[Attribute=Value|List_Of_Attributes],Namespaces) :-
  member(Attribute,[type,base]),
  namespace(Value,Prefix,Type),
  member(ns(Prefix,URI),Namespaces),
  xsd_namespace_uri(URI),
  !,
  node_attribute(IName,ID,Attribute,ns(Prefix,URI):Type,source),
  xsd_flatten_attributes(IName,ID,List_Of_Attributes,Namespaces).
xsd_flatten_attributes(IName,ID,[Attribute=Value|List_Of_Attributes],Namespaces) :-
  node_attribute(IName,ID,Attribute,Value,source),
  xsd_flatten_attributes(IName,ID,List_Of_Attributes,Namespaces).


/**
 * get_defined_namespaces/2
 * get_defined_namespaces(+List_Of_Attributes, -List_Of_Namespaces)
 *
 * Get a list of namespaces defined in a list of
 *   attributes.
 *
 * Example:
 *   get_defined_namespaces([xmlns='http://some'], [ns('','http://some')]).
 */
get_defined_namespaces([],[]).
get_defined_namespaces([xmlns=URI|R],[ns('',URI)|RR]) :-
  !,
  get_defined_namespaces(R,RR).
get_defined_namespaces([xmlns:Prefix=URI|R],[ns(Prefix,URI)|RR]) :-
  !,
  get_defined_namespaces(R,RR).
get_defined_namespaces([_Attribute|R],RR) :-
  !,
  get_defined_namespaces(R,RR).


/**
 * merge_namespaces/3
 * merge_namespaces(+List1, +List2, -Merged)
 */
merge_namespaces([],List2,List2).
merge_namespaces([ns(Prefix,URI)|R],List2,Result) :-
  member(ns(Prefix,URI2),List2),
  !,
  URI = URI2,
  merge_namespaces(R,List2,Result).
merge_namespaces([ns(Prefix,URI)|R],List2,[ns(Prefix,URI)|Result]) :-
  \+member(ns(Prefix,_URI),List2),
  !,
  merge_namespaces(R,List2,Result).


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
convert_xsd_type(anyType,json([])).
convert_xsd_type(anyURI,json([type= string,format= uri])).
convert_xsd_type(base64Binary,json([type= string])).
convert_xsd_type(boolean,json([type= boolean])).
convert_xsd_type(byte,json([type= integer,maximum= 127,exclusiveMaximum= @(false),minimum= -128,exclusiveMinimum= @(false)])).
convert_xsd_type(date,json([type= string,format= 'date-time'])).
convert_xsd_type(dateTime,json([type= string,format= 'date-time'])).
convert_xsd_type(decimal,json([type= number])).
convert_xsd_type(double,json([type= number])).
convert_xsd_type(duration,json([type= string,pattern= '^-?P([0-9]+Y)?([0-9]+M)?([0-9]+D)?(T([0-9]+H)?([0-9]+M)?([0-9]+S)?)?$'])).
convert_xsd_type(float,json([type= number])).
convert_xsd_type(gDay,json([type= integer,minimum= 1,exclusiveMinimum= @(false),maximum= 31, exclusiveMaximum= @(false)])).
convert_xsd_type(gMonth,json([type= integer,minimum= 1,exclusiveMinimum= @(false),maximum= 12, exclusiveMaximum= @(false)])).
convert_xsd_type(gMonthDay,json([type= string,pattern= '^--(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$'])).
convert_xsd_type(gYear,json([type= integer])).
convert_xsd_type(gYearMonth,json([type= string,pattern= '^[0-9]+-([0][1-9]|1[0-2])$'])).
convert_xsd_type(hexBinary,json([type= string,pattern= '^[0-9a-fA-F]*$'])).
convert_xsd_type(int,json([type= integer])).
convert_xsd_type(integer,json([type= integer])).
convert_xsd_type(language,json([type= string])).
convert_xsd_type(long,json([type= integer])).
convert_xsd_type(negativeInteger,json([type= integer,maximum= 0,exclusiveMaximum= @(true)])).
convert_xsd_type(nonNegativeInteger,json([type= integer,minimum= 0,exclusiveMinimum= @(false)])).
convert_xsd_type(nonPositiveInteger,json([type= integer,maximum= 0,exclusiveMaximum= @(false)])).
convert_xsd_type(normalizedString,json([type= string])).
convert_xsd_type(positiveInteger,json([type= integer,minimum= 0,exclusiveMinimum= @(true)])).
convert_xsd_type(short,json([type= integer])).
convert_xsd_type(string,json([type= string])).
convert_xsd_type(time,json([type= string,format= 'date-time'])).
convert_xsd_type(token,json([type= string])).
convert_xsd_type('NMTOKEN',json([type= string])).
convert_xsd_type('ID',json([type= string])).
convert_xsd_type('IDREF',json([type= string])).
convert_xsd_type(unsignedByte,json([type= integer,minimum= 0, exclusiveMinimum= @(false), maximum= 255,exclusiveMaximum= @(false)])).
convert_xsd_type(unsignedLong,json([type= integer,minimum= 0, exclusiveMinimum= @(false)])).
convert_xsd_type(unsignedInt,json([type= integer,minimum= 0, exclusiveMinimum= @(false)])).
convert_xsd_type(unsignedShort,json([type= integer,minimum= 0, exclusiveMinimum= @(false)])).

convert_xsd_type(anySimpleType,json(JSON)) :-
  JSON = [
    oneOf= [
      json([type= integer]),
      json([type= string]),
      json([type= number]),
      json([type= boolean]),
      json([type= null])
    ]
  ].


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
node(_IName,Namespace,Name,_ID,_Children,_Parent_ID)
  <=>
    remove_xsd_element(Name),
    xsd_namespace(Namespace)
  |
    true.


/**
 * remove_node/2
 * remove_node(IName,ID)
 * [CHR-Constraint]
 *
 * Remove a `node` constraint and its `node_attribute`
 *   constraints as well as its children nodes.
 */
:- chr_constraint remove_node/2.
remove_node(IName,ID)
  \
    node_attribute(IName,ID,_,_,_)
  <=>
    true.

remove_node(IName,ID)
  \
    node(IName,Parent_Namespace,Parent_Name,Parent_ID,Siblings,Parent_Parent_ID)
  <=>
    member(ID,Siblings),
    delete(Siblings,ID,Siblings_Without_ID)
  |
    node(IName,Parent_Namespace,Parent_Name,Parent_ID,Siblings_Without_ID,Parent_Parent_ID).

remove_node(IName,ID) \ json(IName,ID,_) <=> true.
remove_node(IName,ID) \ node(IName,_,_,ID,Children,_) <=> remove_nodes(IName,Children).


/**
 * remove_nodes/1
 * remove_nodes(List_Of_IDs)
 *
 * Remove a list of `node/4` constraints by use of
 *   the `remove_node/1` constraint.
 */
remove_nodes(_IName,[]).
remove_nodes(IName,[ID|Rest]) :-
  remove_node(IName,ID),
  remove_nodes(IName,Rest).


/**
 * ##### DO BEFORE transform/1 #####
 *
 * Manipulations which has to be done before
 *   the conversion starts, i.e. before the `transform/1`
 *   constrains was added.
 */

/**
 * Combine multiple `xs:element` within a `xs:sequence`
 *   with the same `Name` and `Type`.
 */
node(IName,NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node_attribute(IName,Element_1_ID,name,Name,_),
    node_attribute(IName,Element_1_ID,type,Type,_)
  \
    node(IName,NS2,element,Element_1_ID,Element_1_Children,Sequence_ID),
    node(IName,NS3,element,Element_2_ID,_Element_2_Children,Sequence_ID),
    node_attribute(IName,Element_2_ID,name,Name,_),
    node_attribute(IName,Element_2_ID,type,Type,_),
    node_attribute(IName,Element_1_ID,minOccurs,MinOccurs_1,_),
    node_attribute(IName,Element_1_ID,maxOccurs,MaxOccurs_1,_),
    node_attribute(IName,Element_2_ID,minOccurs,MinOccurs_2,_),
    node_attribute(IName,Element_2_ID,maxOccurs,MaxOccurs_2,_)
  <=>
    xsd_namespaces([NS1,NS2,NS3]),
    sum_occurs(MinOccurs_1,MinOccurs_2,MinOccurs),
    sum_occurs(MaxOccurs_1,MaxOccurs_2,MaxOccurs)
  |
    remove_node(IName,Element_2_ID),
    node(IName,NS1,element,Element_1_ID,Element_1_Children,Sequence_ID),
    node_attribute(IName,Element_1_ID,minOccurs,MinOccurs,source),
    node_attribute(IName,Element_1_ID,maxOccurs,MaxOccurs,source).


/**
 * Combine multiple `xs:pattern` within a `xs:restriction`.
 *
 * Following
 *   http://www.w3.org/TR/xmlschema-2/#src-multiple-patterns
 *   they are ORed.
 */
node(IName,NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID)
  \
    node(IName,NS2,pattern,Pattern_1_ID,Pattern_1_Children,Restriction_ID),
    node(IName,NS3,pattern,Pattern_2_ID,_Pattern_2_Children,Restriction_ID),
    node_attribute(IName,Pattern_1_ID,value,Pattern_1,_),
    node_attribute(IName,Pattern_2_ID,value,Pattern_2,_)
  <=>
    xsd_namespaces([NS1,NS2,NS3]),
    string_concat(['(',Pattern_1,'|',Pattern_2,')'],New_Pattern)
  |
    remove_node(IName,Pattern_2_ID),
    node(IName,NS2,pattern,Pattern_1_ID,Pattern_1_Children,Restriction_ID),
    node_attribute(IName,Pattern_1_ID,value,New_Pattern,source).


/**
 * ## Set defaults ##
 *
 * Note: This will already be executed without the
 *   `transform/1` constraint being added.
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
node(IName,Namespace,element,Element_ID,_Element_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Element_ID,minOccurs,'1',default).

node(IName,Namespace,element,Element_ID,_Element_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Element_ID,maxOccurs,'1',default).


/**
 * Add `use` attribute to all `xs:attribute` XSD nodes.
 *   Default is `optional`.
 */
node(IName,Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Attribute_ID,use,optional,default).


/**
 * Add `fixed` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Fixed)`.
 */
node(IName,Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Attribute_ID,fixed,_Unbound,default).


/**
 * Add `default` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Default)`.
 */
node(IName,Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Attribute_ID,default,_Unbound,default).


/**
 * Add `id` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Value)`.
 *
 * This `id` isn't used yet as there is no equivalent in
 *   JSON Schema.
 */
node(IName,Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Attribute_ID,id,_Unbound,default).


/**
 * Add `type` attribute to all `xs:attribute` XSD nodes.
 *   Default is not set, i.e. `var(Type)`.
 */
node(IName,Namespace,attribute,Attribute_ID,_Attribute_Children,_Parent_ID)
  ==>
    xsd_namespace(Namespace)
  |
    node_attribute(IName,Attribute_ID,type,_Unbound,default).


/**
 * ##########  XS:EXTENSION  ##########
 */

/**
 * Extension base is a self defined data type, but base has no
 *   children, i.e. it's only an alias.
 */
transform(IName),
    node(IName,Namespace,extension,Extension_ID,[],_Parent_ID),
    node_attribute(IName,Extension_ID,base,Base,_)
  ==>
    xsd_namespace(Namespace),
    no_xsd_namespace(Base)
  |
    string_concat('#/definitions/',Base,Base_Reference),
    JSON = [
      '$ref'= Base_Reference
    ],
    json(IName,Extension_ID,json(JSON)).


transform(IName),
    node(IName,Namespace,extension,Extension_ID,Extension_Children,_Parent_ID),
    node_attribute(IName,Extension_ID,base,Base,_)
  ==>
    xsd_namespace(Namespace),
    no_xsd_namespace(Base),
    Extension_Children \= []
  |
    string_concat('#/definitions/',Base,Base_Reference),
    JSON = [
      facets= json([]),
      base= json([ '$ref'= Base_Reference ])
    ],
    json(IName,Extension_ID,json(JSON)).


transform(IName),
    node(IName,NS1,extension,Extension_ID,_Extension_Children,_Parent_ID),
    node(IName,NS2,all,All_ID,_All_Children,Extension_ID),
    json(IName,All_ID,All_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    JSON = [
      facets= All_JSON
    ],
    json(IName,Extension_ID,json(JSON)).

transform(IName),
    node(IName,NS1,simpleContent,SimpleContent_ID,_SimpleContent_Children,_SimpleContent_Parent_ID),
    node(IName,NS2,extension,Extension_ID,_Extension_Children,SimpleContent_ID),
    node_attribute(IName,Extension_ID,base,Base,_)
  ==>
    xsd_namespaces([NS1,NS2]),
    reference_type(Base,json(Base_JSON))
  |
    json(IName,Extension_ID,json([
      facets= json([]),
      base= json(Base_JSON)
    ])).


/**
 * ##########  XS:RESTRICTION  ##########
 */

/**
 * Every restriction has at least its base type in an `base=Base`
 *   attribute.
 */
transform(IName),
    node(IName,Namespace,restriction,ID,Restriction_Children,_Parent_ID),
    node_attribute(IName,ID,base,Base,_)
  ==>
    xsd_namespace(Namespace),
    namespace(Base,Base_Namespace,Base_Type),
    xsd_namespace(Base_Namespace),
    convert_xsd_type(Base_Type,json(Base_Type_JSON))
  |
    (
      % type is JSON primitive like 'integer', 'string'
      Base_Type_JSON = [type= Primitive_JSON_Type],
      JSON = [
        type= Primitive_JSON_Type
      ]
    ;
      % type is not primitive but restriction has no further facets
      Base_Type_JSON \= [type= Primitive_JSON_Type],
      Restriction_Children = [],
      JSON = Base_Type_JSON
    ;
      % type is not primitive and with further facets, so result
      %   should be `allOf=[ ... ]`
      Base_Type_JSON \= [type=_Primitive_JSON_Type],
      string_concat([xs,':',Base_Type], XS_Prefixed_Base_Type),
      schema_definition(IName,XS_Prefixed_Base_Type,ID,json(Base_Type_JSON)),
      string_concat('#/definitions/',XS_Prefixed_Base_Type,XS_Prefixed_Base_Type_Reference),
      JSON = [
        facets= json([]),
        base= json([ '$ref'= XS_Prefixed_Base_Type_Reference ])
      ]
    ),
    json(IName,ID,json(JSON)).


/**
 * Restriction base is a self defined data type.
 */
transform(IName),
    node(IName,Namespace,restriction,ID,Restriction_Children,_Parent_ID),
    node_attribute(IName,ID,base,Base,_)
  ==>
    xsd_namespace(Namespace),
    no_xsd_namespace(Base)
  |
    string_concat('#/definitions/',Base,Base_Reference),
    (
      % no constraining facets, so it's only an alias
      Restriction_Children = [],
      JSON = [
        '$ref'= Base_Reference
      ]
    ;
      % Add `allOf=[ ... ]` because of constraining facets
      Restriction_Children \= [],
      JSON = [
        facets= json([]),
        base= json([ '$ref'= Base_Reference ])
      ]
    ),
    json(IName,ID,json(JSON)).


/**
 * ##########  XS:ENUMERATION  ##########
 */

/**
 * `xs:enumeration` element as child of a `xs:restriction` node.
 */
transform(IName),
    node(IName,NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID),
    node(IName,NS2,enumeration,Enumeration_ID,_Enumeration_Children,Restriction_ID),
    node_attribute(IName,Enumeration_ID,value,Value,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,Restriction_ID,json([enum=[Value]])).


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
transform(IName),
    node(IName,NS1,restriction,Restriction_ID,_Restriction_Children,_Restriction_Parent_ID),
    node(IName,NS2,Facet_Name_XSD,Facet_ID,_Facet_Children,Restriction_ID),
    node_attribute(IName,Facet_ID,value,Value,_)
  ==>
    xsd_namespaces([NS1,NS2]),
    convert_xsd_restriction(Facet_Name_XSD,Value,Facet_JSON)
  |
    JSON = [
      facets=Facet_JSON
    ],
    json(IName,Restriction_ID,json(JSON)).


/**
 * ##########  XS:ELEMENT  ##########
 */

/**
 * `xs:element` with `fixed` attribute set.
 */
transform(IName),
    node(IName,Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(IName,ID,fixed,Fixed,_)
  ==>
    xsd_namespace(Namespace)
  |
    json(IName,ID,json([enum=[Fixed]])).


/**
 * `xs:element` with `type` attribute set which can be
 *   translated to a primitive JSON Schema type.
 */
transform(IName),
    node(IName,Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(IName,ID,type,Type_With_NS,_)
  ==>
    xsd_namespace(Namespace),
    valid_xsd_type(Type_With_NS,Type)
  |
    convert_xsd_type(Type,JSON),
    json(IName,ID,JSON).


/**
 * `xs:element` with `type` attribute set which must be
 *   self defined.
 */
transform(IName),
    node(IName,Namespace,element,ID,_Children,_Element_Parent_ID),
    node_attribute(IName,ID,type,Type_With_NS,_)
  ==>
    xsd_namespace(Namespace),
    no_xsd_namespace(Type_With_NS)
  |
    reference_type(Type_With_NS,JSON),
    json(IName,ID,JSON).


/**
 * Nested `xs:element/{xs:complexType,xs:simpleType}` structure with `name`
 *   attribute set.
 */
transform(IName),
    node(IName,NS1,element,Element_ID,_Element_Children,_Element_Parent_ID),
    node(IName,NS2,InnerType,InnerType_ID,_InnerType_Children,Element_ID),
    node_attribute(IName,Element_ID,name,_Name,_),
    json(IName,InnerType_ID,InnerType_JSON)
  ==>
    xsd_namespaces([NS1,NS2]),
    member(InnerType,[complexType,simpleType])
  |
    json(IName,Element_ID,InnerType_JSON).



/**
 * ##########  XS:ANNOTATION  ##########
 * ##########  XS:DOCUMENTATION  ##########
 */

/**
 * Convert a `xs:annotation/xs:documentation`.
 */
transform(IName),
    node(IName,NS1,_Some,Some_ID,_Some_Children,_Some_Parent_ID),
    node(IName,NS2,annotation,Annotation_ID,_Annotation_Children,Some_ID),
    node(IName,NS3,documentation,Documentation_ID,_Documentation_Children,Annotation_ID),
    text_node(IName,_Text_Node_ID,Text,Documentation_ID)
  ==>
    xsd_namespaces([NS1,NS2,NS3])
  |
    json(IName,Some_ID,json([description=Text])).


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
transform(IName),
    node(IName,NS1,all,All_ID,_All_Children,_All_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,All_ID),
    json(IName,Element_ID,Element_JSON),
    node_attribute(IName,Element_ID,minOccurs,MinOccurs,_),
    node_attribute(IName,Element_ID,maxOccurs,MaxOccurs,_),
    node_attribute(IName,Element_ID,name,Element_Name,_)
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
    json(IName,All_ID,json(Full_JSON)).


/**
 * ##########  XS:CHOICE  ##########
 */

/**
 * `xs:element` within a `xs:choice` with the `maxOccurs` attribute
 *   set to '1' and `minOccurs` to '0' or '1'.
 */
transform(IName),
    node(IName,NS1,choice,Choice_ID,_Choice_Children,_Choice_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Choice_ID),
    json(IName,Element_ID,Element_JSON),
    node_attribute(IName,Element_ID,minOccurs,MinOccurs,_),
    node_attribute(IName,Element_ID,maxOccurs,'1',_),
    node_attribute(IName,Element_ID,name,Element_Name,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    Properties = [Element_Name=Element_JSON],
    JSON = [type=object,properties=json(Properties)],
    % add `required=[...]` if `minOccurs` = 1
    (MinOccurs = '0', Full_JSON = JSON;
      MinOccurs = '1', Full_JSON = [required=[Element_Name]|JSON]),
    json(IName,Choice_ID,json(Full_JSON)).

/**
 * `xs:element` within a `xs:choice` with the `maxOccurs` attribute
 *   set to 'unbounded' or >= 2.
 */
transform(IName),
    node(IName,NS1,choice,Choice_ID,_Choice_Children,_Choice_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Choice_ID),
    json(IName,Element_ID,Element_JSON),
    node_attribute(IName,Element_ID,minOccurs,MinOccurs,_),
    node_attribute(IName,Element_ID,maxOccurs,MaxOccurs,_),
    node_attribute(IName,Element_ID,name,Element_Name,_)
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
    json(IName,Choice_ID,json(Full_JSON)).


/**
 * ##########  XS:SEQUENCE  ##########
 */

/**
 * `xs:element` within a `xs:sequence` with the `maxOccurs` attribute
 *   set to '1' and `minOccurs` to '0' or '1'.
 */
transform(IName),
    node(IName,NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Sequence_ID),
    json(IName,Element_ID,Element_JSON),
    node_attribute(IName,Element_ID,minOccurs,MinOccurs,_),
    node_attribute(IName,Element_ID,maxOccurs,'1',_),
    node_attribute(IName,Element_ID,name,Element_Name,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    Properties = [Element_Name=Element_JSON],
    JSON = [type=object,properties=json(Properties)],
    % add `required=[...]` if `minOccurs` = 1
    (MinOccurs = '0', Full_JSON = JSON;
      MinOccurs = '1', Full_JSON = [required=[Element_Name]|JSON]),
    json(IName,Sequence_ID,json(Full_JSON)).


/**
 * `xs:element` within a `xs:sequence` with the `maxOccurs` attribute
 *   set to 'unbounded' or >= 2.
 */
transform(IName),
    node(IName,NS1,sequence,Sequence_ID,_Sequence_Children,_Sequence_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Sequence_ID),
    json(IName,Element_ID,Element_JSON),
    node_attribute(IName,Element_ID,minOccurs,MinOccurs,_),
    node_attribute(IName,Element_ID,maxOccurs,MaxOccurs,_),
    node_attribute(IName,Element_ID,name,Element_Name,_)
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
    json(IName,Sequence_ID,json(Full_JSON)).


/**
 * ##########  XS:COMPLEXTYPE  ##########
 */

/**
 * `xs:complexType` which has a `xs:all` child.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(IName,NS2,all,All_ID,_All_Children,ComplexType_ID),
    json(IName,All_ID,All_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,ComplexType_ID,All_JSON).


/**
 * `xs:complexType` which has a `xs:choice` child.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(IName,NS2,choice,Choice_ID,_Choice_Children,ComplexType_ID),
    json(IName,Choice_ID,Sequence_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,ComplexType_ID,Sequence_JSON).


/**
 * `xs:complexType` which has a `xs:sequence` child.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(IName,NS2,sequence,Sequence_ID,_Sequence_Children,ComplexType_ID),
    json(IName,Sequence_ID,Sequence_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,ComplexType_ID,Sequence_JSON).


/**
 * `xs:complexType` which has `xs:complexContent/xs:extension` children.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(IName,NS2,complexContent,ComplexContent_ID,_ComplexContent_Children,ComplexType_ID),
    node(IName,NS3,extension,Extension_ID,_Extension_Children,ComplexContent_ID),
    json(IName,Extension_ID,Extension_JSON)
  ==>
    xsd_namespaces([NS1,NS2,NS3])
  |
    json(IName,ComplexType_ID,Extension_JSON).


/**
 * `xs:complexType` which has `xs:simpleContent/xs:extension` children.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    node(IName,NS2,simpleContent,SimpleContent_ID,_SimpleContent_Children,ComplexType_ID),
    node(IName,NS3,extension,Extension_ID,_Extension_Children,SimpleContent_ID),
    json(IName,Extension_ID,json(Extension_JSON_List))
  ==>
    xsd_namespaces([NS1,NS2,NS3]),
    lookup(base,Extension_JSON_List,json(Base),Extension_JSON_List_Wo_Base),
    lookup(facets,Extension_JSON_List_Wo_Base,json(Facets),Extension_JSON_List_Wo_Base_Facets),
    lookup(properties,Facets,Facets_Properties)
  |
    json(IName,ComplexType_ID,json([
      type= object,
      properties=json([
        value= json(Base)
      ]),
      required= [value]
    ])),
    json(IName,ComplexType_ID,json([
      properties=Facets_Properties
    ])),
    json(IName,ComplexType_ID,json(Extension_JSON_List_Wo_Base_Facets)).


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
transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Type_With_NS,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2]),
    reference_type(Type_With_NS,json(Attribute_JSON)),
    mark_attribute(Mark),
    string_concat(Mark,Attribute_Name,Attribute_Name2)
  |
    JSON = [
      type=object,
      properties=json([
        Attribute_Name2=json(Attribute_JSON)
      ])
    ],
    (
      ParentEl == complexType,
      json(IName,ParentEl_ID,json(JSON))
    ;
      ParentEl == extension,
      JSON_Extension = [facets= json(JSON)],
      json(IName,ParentEl_ID,json(JSON_Extension))
    ).

transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Type_With_NS,_),
    node_attribute(IName,Attribute_ID,use,required,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2])
  |
    json(IName,ParentEl_ID,json([
      required= [Attribute_Name]
    ])).

transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Type_With_NS,_),
    node_attribute(IName,Attribute_ID,fixed,Fixed,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2]),
    reference_type(Type_With_NS,json(Attribute_JSON)),
    \+var(Fixed),
    mark_attribute(Mark),
    string_concat(Mark,Attribute_Name,Attribute_Name2)
  |
    cast_by_json(Attribute_JSON,Fixed,Fixed_Casted),
    json(IName,ParentEl_ID,json([
      properties=json([
        Attribute_Name2=json([
          enum=[Fixed_Casted]
        ])
      ])
    ])).

transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Type_With_NS,_),
    node_attribute(IName,Attribute_ID,fixed,Fixed,_),
    node_attribute(IName,Attribute_ID,default,Default,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
    \+var(Type_With_NS),
    xsd_namespaces([NS1,NS2]),
    reference_type(Type_With_NS,json(Attribute_JSON)),
    /**
     * As mentioned in the XSD specification not both
     *   `fixed` and `enum` can be set.
     */
    \+var(Default),
    var(Fixed),      % see explanation above
    mark_attribute(Mark),
    string_concat(Mark,Attribute_Name,Attribute_Name2)
  |
    cast_by_json(Attribute_JSON,Default,Default_Casted),
    json(IName,ParentEl_ID,json([
      properties=json([
        Attribute_Name2=json([
          default=Default_Casted
        ])
      ])
    ])).


/**
 * `xs:complexType` which has an attribute with an
 *   inline type definition, i.e. `var(Type)` and an
 *   `xs:simpleType` child node.
 *
 * This rule will be called once the JSON for the inner
 *   `xs:simpleType` has been generated.
 */
transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node(IName,NS3,simpleType,SimpleType_ID,_SimpleType_Children,Attribute_ID),
    json(IName,SimpleType_ID,json(SimpleType_JSON)),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Unbound_Type,_),
    node_attribute(IName,Attribute_ID,use,Use,_),
    node_attribute(IName,Attribute_ID,fixed,Fixed,_),
    node_attribute(IName,Attribute_ID,default,Default,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
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
    (
      ParentEl == complexType,
      json(IName,ParentEl_ID,json(JSON2))
    ;
      ParentEl == extension,
      JSON_Extension = [facets= json(JSON2)],
      json(IName,ParentEl_ID,json(JSON_Extension))
    ).


/**
 * `xs:complexType` which has an attribute with `@ref`
 *   attribute being set.
 *
 * As specified, `@name` and `@type` can not be both
 *   present.
 */
transform(IName),
    node(IName,NS1,ParentEl,ParentEl_ID,_ParentEl_Children,_ParentEl_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,ParentEl_ID),
    node_attribute(IName,Attribute_ID,ref,Ref,_),
    node_attribute(IName,Attribute_ID,use,Use,_),
    node_attribute(IName,Attribute_ID,fixed,_Fixed,_),
    node_attribute(IName,Attribute_ID,default,_Default,_)
  ==>
    (
      ParentEl == complexType
    ;
      ParentEl == extension
    ),
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
    (
      ParentEl == complexType,
      json(IName,ParentEl_ID,json(JSON2))
    ;
      ParentEl == extension,
      JSON_Extension = [facets= json(JSON2)],
      json(IName,ParentEl_ID,json(JSON_Extension))
    ).


/**
 * `xs:complexType` within a `xs:schema` node and having
 *   an attribute with `@type` attribute being set, i.e.
 *   `\+var(Type)`. This is a global defined type and
 *   therefore propagates a new `schema_definition`
 *   constraint with @`Name` as name.
 */
transform(IName),
    node(IName,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName,NS2,attribute,Attribute_ID,_Attribute_Children,Schema_ID),
    node_attribute(IName,Attribute_ID,name,Attribute_Name,_),
    node_attribute(IName,Attribute_ID,type,Type_With_NS,_),
    node_attribute(IName,Attribute_ID,fixed,Fixed,_),
    node_attribute(IName,Attribute_ID,default,Default,_)
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
    schema_definition(IName,Definition_Name,Attribute_ID,json(Attribute_JSON3)).


/**
 * ##########  XS:SIMPLETYPE  ##########
 */

/**
 * `xs:simpleType` which has a `xs:restriction` child.
 */
transform(IName),
    node(IName,NS1,simpleType,SimpleType_ID,_SimpleType_Children,_SimpleType_Parent_ID),
    node(IName,NS2,restriction,Restriction_ID,_Restriction_Children,SimpleType_ID),
    json(IName,Restriction_ID,Restriction_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,SimpleType_ID,Restriction_JSON).


/**
 * ##########  XS:INCLUDE  ##########
 */

/**
 * Call xsd2json for included schemas.
 */
node(IName_Parent,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName_Parent,NS2,include,Include_ID,_Include_Children,Schema_ID),
    node_attribute(IName_Parent,Include_ID,schemaLocation,Schema_Location,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    /**
     * Add empty json([]) to hold for schemas that only contain
     * includes.
     */
    json(IName_Parent,Schema_ID,json([])),
    relative_input(IName_Parent,Schema_Location,Location),
    xsd2json(Location,_).


/**
 * Merge included sub-schema.
 */
transform(IName_Parent),
    node(IName_Parent,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName_Parent,NS2,include,Include_ID,_Include_Children,Schema_ID),
    node_attribute(IName_Parent,Include_ID,schemaLocation,Schema_Location,_)
  \
    json(IName_Parent,Schema_ID,Schema_JSON),
    xsd2json_result(IName_Include,Include_JSON)
  <=>
    xsd_namespaces([NS1,NS2]),
    relative_input(IName_Parent,Schema_Location,Location),
    input_name(Location,IName_Include)
  |
    merge_json(Schema_JSON,Include_JSON,JSON),
    json(IName_Parent,Schema_ID,JSON).


/**
 * ##########  XS:IMPORT  ##########
 */

/**
 * Call xsd2json for imported schemas.
 */
node(IName_Parent,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName_Parent,NS2,import,Import_ID,_Import_Children,Schema_ID),
    node_attribute(IName_Parent,Import_ID,schemaLocation,Schema_Location,_)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    /**
     * Add empty json([]) to hold for schemas that only contain
     * imports.
     */
    json(IName_Parent,Schema_ID,json([])),
    relative_input(IName_Parent,Schema_Location,Location),
    xsd2json(Location,_).


/**
 * Merge imported sub-schema.
 */
transform(IName_Parent),
    node(IName_Parent,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName_Parent,NS2,import,Import_ID,_Import_Children,Schema_ID),
    node_attribute(IName_Parent,Import_ID,schemaLocation,Schema_Location,_),
    node_attribute(IName_Parent,Import_ID,namespace,_Namespace_URI,_)
  \
    json(IName_Parent,Schema_ID,Schema_JSON),
    xsd2json_result(IName_Import,Import_JSON)
  <=>
    xsd_namespaces([NS1,NS2]),
    relative_input(IName_Parent,Schema_Location,Location),
    input_name(Location,IName_Import)
  |
    % add_namespace(Import_JSON,Import_NS,Import_JSON_With_NS),
    Import_JSON_With_NS = Import_JSON,
    merge_json(Schema_JSON,Import_JSON_With_NS,JSON),
    json(IName_Parent,Schema_ID,JSON).



/**
 * ##########  XS:SCHEMA  ##########
 */

:- chr_constraint single_root_element/2.
:- chr_constraint multiple_root_elements/1.

node(IName,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Schema_ID)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    single_root_element(IName,Element_ID).

single_root_element(IName,_),
    single_root_element(IName,_)
  <=> multiple_root_elements(IName).

multiple_root_elements(IName) \
    single_root_element(IName,_)
  <=>
    true.

transform(IName),
    node(IName,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    single_root_element(IName,Element_ID),
    node(IName,NS2,element,Element_ID,_Element_Children,Schema_ID),
    json(IName,Element_ID,Element_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,Schema_ID,Element_JSON).

transform(IName),
    node(IName,NS1,schema,Schema_ID,_Schema_Children,_Schema_Parent_ID),
    multiple_root_elements(IName),
    node(IName,NS2,element,Element_ID,_Element_Children,Schema_ID),
    node_attribute(IName,Element_ID,name,Element_Name,_),
    json(IName,Element_ID,Element_JSON)
  ==>
    xsd_namespaces([NS1,NS2])
  |
    json(IName,Schema_ID,json([
      oneOf= [
        json([
          type= object,
          properties= json([
            Element_Name=Element_JSON
          ]),
          additionalProperties= @(false),
          required= [Element_Name]
        ])
      ]
    ])).



/**
 * Create `schema_definition/3` constraint for each
 *   `complexType` node which has the `@name` attribute
 *   set.
 */
transform(IName),
    node(IName,NS1,complexType,ComplexType_ID,_ComplexType_Children,_ComplexType_Parent_ID),
    json(IName,ComplexType_ID,ComplexType_JSON),
    node_attribute(IName,ComplexType_ID,name,Definition_Name,_)
  ==>
    xsd_namespaces([NS1])
  |
    schema_definition(IName,Definition_Name,ComplexType_ID,ComplexType_JSON).


/**
 * Create `schema_definition/3` constraint for each
 *   `simpleType` node which has the `@name` attribute
 *   set.
 */
transform(IName),
    node(IName,NS1,simpleType,SimpleType_ID,_SimpleType_Children,_SimpleType_Parent_ID),
    json(IName,SimpleType_ID,SimpleType_JSON),
    node_attribute(IName,SimpleType_ID,name,Definition_Name,_)
  ==>
    xsd_namespaces([NS1])
  |
    schema_definition(IName,Definition_Name,SimpleType_ID,SimpleType_JSON).


/**
 * ##########  ON BUILD_SCHEMA  ##########
 */

build_schema(IName),
    schema_definition(IName,Name,_ID,Inline_Schema),
    node(IName,Namespace,schema,Schema_ID,_Schema_Children,_)
  ==>
    xsd_namespace(Namespace),
    first_id(Schema_ID)
  |
    json(IName,Schema_ID,json([definitions=json([Name=Inline_Schema])])).


/**
 * ##########  ON FINISH  ##########
 */

/**
 * get_json/3
 * get_json(IName,ID,Unbound_Var)
 *
 * Bind a variable to the content of the json Constraint
 * with the same ID.
 * Usually used when finished to get the root's JSON.
 */
:- chr_constraint get_json/3.

json(IName,ID,JSON)
  \
    get_json(IName,ID,Result)
  <=>
    JSON = Result.
