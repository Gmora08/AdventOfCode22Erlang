-module(day1).

%% API exports
-export([main/1]).

-define(INPUT_PATH, <<"./input/">>).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([File]) ->
  main([File, "1"]);

main([_ , X] = Args) ->
  io:format("Args: ~p~n", [Args]),
  Path = hd(Args),
  {PosElfs, _} = string:to_integer(X),
  Input = read_input(Path),
  ElfsCalories = parse_and_order_elfs_calories(Input),
  Result = lists:sublist(ElfsCalories, PosElfs),
  io:format("Bigger number of calories: ~p~n", [lists:last(Result)]),
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
read_input(File) ->
  Path = iolist_to_binary([?INPUT_PATH, File]),
  io:format("Reading from path: ~p~n", [Path]),
  {ok, RawInput} = file:read_file(Path),
  RawInput.

parse_and_order_elfs_calories(Input) ->
  RawCalories = binary:split(Input, <<"\n\n">>, [global]),
  CaloriesByElf = lists:map(
    fun(ElfRawCalories) ->
      BinaryListOfCalories = binary:split(ElfRawCalories, <<"\n">>, [global]),
      ListOfCalories = lists:map(fun binary_to_integer/1, BinaryListOfCalories),
      lists:sum(ListOfCalories)
    end,
    RawCalories
  ),
  lists:reverse(
    lists:sort(CaloriesByElf)
  ).
