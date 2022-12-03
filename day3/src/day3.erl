-module(day3).

%% API exports
-export([main/1]).

-define(INPUT_PATH, <<"./input/">>).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
  io:format("Args: ~p~n", [Args]),
  RuckSacks = read_input(hd(Args)),
  GroupedRuckSacks = group_ruck_sacks_by(RuckSacks),
  % ========= For day 1
  % RuckSacksRepeatedItems = lists:map(fun process_rucksacks/1, RuckSacks),
  RuckSacksRepeatedItems = lists:map(
    fun([G1, G2, G3]) ->
      find_repeated_item(G1, G2, G3)
    end,
    GroupedRuckSacks
  ),
  ItemPriorities = lists:map(fun get_item_priority/1, RuckSacksRepeatedItems),
  io:format("Sum of priorities is: ~p~n", [lists:sum(ItemPriorities)]),
  erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
group_ruck_sacks_by(RuckSacks) -> group_ruck_sacks_by(RuckSacks, {[], []}).
group_ruck_sacks_by([], {[], FinalGroup}) ->
  FinalGroup;
group_ruck_sacks_by([RuckSack | Rest], {CurrentGroup, FinalGroup}) ->
  Group = CurrentGroup ++ [RuckSack],
  case length(Group) of
    3 ->
      NewFinalGroup = FinalGroup ++ [Group],
      group_ruck_sacks_by(Rest, {[], NewFinalGroup});
    _ ->
      group_ruck_sacks_by(Rest, {Group, FinalGroup})
  end.


read_input(File) ->
  Path = iolist_to_binary([?INPUT_PATH, File]),
  io:format("Reading from path: ~p~n", [Path]),
  {ok, RawInput0} = file:read_file(Path),
  RawInput = binary:bin_to_list(RawInput0),
  string:split(RawInput, "\n", all).

% Find the repeated items for both compartments
process_rucksacks(RuckSack) ->
  % Erlang always returns a float with / , needs to be converted to an integer
  SplitBy = round(length(RuckSack) / 2),
  {Compartment1, Compartment2} = lists:split(SplitBy, RuckSack),
  RepeatedItem = find_repeated_item(Compartment1, Compartment2),
  % io:format("Repeated item is: ~p~n", [RepeatedItem]),
  RepeatedItem.

find_repeated_item([Item | Rest], Compartment2) ->
  case lists:member(Item, Compartment2) of
    false -> find_repeated_item(Rest, Compartment2);
    true -> Item
  end.

find_repeated_item([Item | Rest], Group2, Group3) ->
  case lists:member(Item, Group2) andalso lists:member(Item, Group3) of
    false -> find_repeated_item(Rest, Group2, Group3);
    true -> Item
  end.

get_item_priority($a) -> 1;
get_item_priority($z) -> 26;
get_item_priority($A) -> 27;
get_item_priority($Z) -> 52;
get_item_priority(Item) when Item > $a andalso Item < $z -> (Item - $a) + 1;
get_item_priority(Item) -> (Item - $A) + 27.




