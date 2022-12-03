-module(day2).

%% API exports
-export([main/1]).

-define(INPUT_PATH, <<"./input/">>).

% {win, draw, lose)
-define(RULES, #{
  "A" => {"C", "A", "B"},
  "B" => {"A", "B", "C"},
  "C" => {"B", "C", "A"}
  }).

-define(POINTS, #{
    "A" => {rock, 1},
    "B" => {paper, 2},
    "C" => {scissors, 3}
  }).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  io:format("Args: ~p~n", [Args]),
  RoundOfPlays = read_input(hd(Args)),
  RoundSum = get_round_sum(RoundOfPlays),
  io:format("The game score is: ~p~n", [RoundSum]),
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
get_round_sum(RoundOfPlays) -> get_round_sum(RoundOfPlays, 0).
get_round_sum([], Sum) -> Sum;
get_round_sum([[P1, Instruction] | Rest], Sum) ->
  % for part 2
  P2 = replace_input(Instruction, P1),
  % for part 1
  % P2 = replace_input(Instruction),
  RoundScore = judge(P2, P1),
  get_round_sum(Rest, Sum + RoundScore).

read_input(File) ->
  Path = iolist_to_binary([?INPUT_PATH, File]),
  io:format("Reading from path: ~p~n", [Path]),
  {ok, RawInput0} = file:read_file(Path),
  RawInput = binary:bin_to_list(RawInput0),
  RawList = string:split(RawInput, "\n", all),
  lists:map(fun(RE) -> string:split(RE, " ", all) end, RawList).

judge(P1, P2) ->
  io:format("Round of play ~p vs ~p~n", [P1, P2]),
  PlayScore = get_round_score(P1),
  case play(P1, P2) of
    winner ->
      % io:format("Round score: ~p~n", [PlayScore + 6]),
      PlayScore + 6;
    draw ->
      % io:format("Round score: ~p~n", [PlayScore + 1]),
      PlayScore + 3;
    _ ->
      % io:format("Round score: ~p~n", [PlayScore]),
      PlayScore
  end.

get_round_score(Option) ->
  {_, Score} = maps:get(Option, ?POINTS),
  Score.

% For part 1
replace_input("Y") -> "B";
replace_input("X") -> "A";
replace_input("Z") -> "C".

% For part2
replace_input(Instruction, OpponentPlay) ->
  case Instruction of
    "X" ->
      {WillLose, _, _} = maps:get(OpponentPlay, ?RULES),
      WillLose;
    "Y" ->
      {_, Draw, _} = maps:get(OpponentPlay, ?RULES),
      Draw;
    "Z" ->
      {_, _, WillWin} = maps:get(OpponentPlay, ?RULES),
      WillWin
  end.

play("A", "B") -> looser;
play("A", "C") -> winner;
play("B", "A") -> winner;
play("B", "C") -> looser;
play("C", "A") -> looser;
play("C", "B") -> winner;
play(_, _) -> draw.