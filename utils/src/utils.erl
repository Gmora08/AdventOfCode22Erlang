-module(utils).

%% API exports
-export([main/1]).

-define(INPUT_PATH, <<"./input/">>).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    erlang:halt(0).

read_input(File) ->
    Path = iolist_to_binary([?INPUT_PATH, File]),
    io:format("Reading from path: ~p~n", [Path]),
    {ok, RawInput} = file:read_file(Path),
    RawInput.

%%====================================================================
%% Internal functions
%%====================================================================
