:- use_module(xsd2json).
:- use_module(library(http/json)).

opts_spec([
  [
    opt(space),
    type(atom),
    default(remove),
    shortflags([ s ]),
    longflags([ 'whitespace' ]),
    help([
      'whitespace handling, one of',
      '  remove   :  clean whitespace  [default]',
      '  preserve :  keep whitespace'
    ])
  ],
  [
    opt(indentation),
    type(integer),
    default(2),
    shortflags([ i ]),
    longflags([ 'indentation' ]),
    help([
      'set indentation level',
      'use 0 for single-line output'
    ])
  ],
  [
    opt(trace),
    type(boolean),
    default(false),
    shortflags([ t ]),
    longflags([ 'trace' ]),
    help([
      'run with activated tracing'
    ])
  ],
  [
    opt(version),
    type(boolean),
    default(false),
    shortflags([ v ]),
    longflags([ 'version' ]),
    help([
      'show xsd2json version information'
    ])
  ],
  [
    opt(debuginfo),
    type(boolean),
    default(false),
    longflags([ 'debug-info' ]),
    help([
      'show information usually needed for debugging'
    ])
  ],
  [
    opt(help),
    type(boolean),
    default(false),
    shortflags([ h ]),
    longflags([ 'help' ]),
    help([
      'display this help'
    ])
  ]
]).

main :-
  opts_spec(OptsSpec),
  opt_arguments(OptsSpec,Opts,PositionalArgs),
  main(Opts,PositionalArgs).

main(Opts,_PositionalArgs) :-
  memberchk(version(true),Opts), !,
  xsd2json:version(Version),
  writeln(Version),
  halt(0).

main(Opts,_PositionalArgs) :-
  memberchk(debuginfo(true),Opts), !,
  xsd2json:version(Version),
  writeln('# xsd2json'),
  writeln(Version),
  writeln('# SWI-Prolog'),
  shell('swipl --version'),
  writeln('# npm'),
  shell('npm -version'),
  writeln('# node'),
  shell('node -v'),
  halt(0).

main(Opts,PositionalArgs) :-
  (
    memberchk(help(true),Opts)
  ;
    PositionalArgs = []
  ), !,
  opts_spec(OptsSpec),
  opt_help(OptsSpec,Help),
  writeln('USAGE: xsd2json [options] <path>'), nl,
  writeln('convert a XSD file into equivalent JSON schema'), nl,
  writeln('Options:'),
  writeln(Help),
  halt(0).

main(Opts,PositionalArgs) :-
  memberchk(trace(Trace),Opts),
  (
    PositionalArgs = [Filename]
  ;
    PositionalArgs = [_CLI, Filename]
  ),
  (
    Trace = true,
    chr_leash(none),
    chr_trace
  ;
    Trace = false
  ),
  xsd2json(Filename,Opts,JSON),
  % output
  memberchk(indentation(Indentation),Opts),
  json_write(user_output,JSON,[width(1),step(Indentation)]), nl,
  % finished
  halt(0).

main(_,_) :- halt(1).
