-module(day4).

%% API exports
-export([main/1]).

-define(INPUT_PATH, <<"./input/">>).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  Assigments = read_input(hd(Args)),
  OverlappedAssigments = lists:filter(fun are_assignments_contained/1, Assigments),
  io:format("Number of overlapped assigments: ~p~n", [length(OverlappedAssigments)]),
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
read_input(File) ->
  Path = iolist_to_binary([?INPUT_PATH, File]),
  io:format("Reading from path: ~p~n", [Path]),
  {ok, RawInput0} = file:read_file(Path),
  RawInput = binary:bin_to_list(RawInput0),
  lists:map(fun parse_pair/1,
    string:split(RawInput, "\n", all)
  ).

parse_pair(StringPair) ->
  lists:map(
    fun(Pair) ->
      [P1_0, P2_0] = string:split(Pair, "-"),
      {P1, _} = string:to_integer(P1_0),
      {P2, _} = string:to_integer(P2_0),
      {P1, P2}
    end,
    % "2-4,6-8" -> ["2-4", "6-8"]
    string:split(StringPair, ",")
  ).

% ========================
% TIL and wanted to use it
% https://www.erlang.org/doc/reference_manual/expressions.html#short-circuit-expressions
% ========================
are_assignments_contained([Assigment1, Assigment2]) ->
  are_assignments_contained(Assigment1, Assigment2) 
  orelse
  are_assignments_contained(Assigment2, Assigment1).

are_assignments_contained({A1, A2}, {B1, B2}) ->
  Seq = lists:seq(A1, A2),
  % For part 1 
  lists:member(B1, Seq) andalso lists:member(B2, Seq).
  % For part 2
  % lists:member(B1, Seq) orelse lists:member(B2, Seq).



