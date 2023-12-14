%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka6).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0, task2/0]).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).


last_winn(_, HoldTime) when HoldTime =< 0 -> true;
last_winn(Game = {game, TotalTime, DistanceToWinn}, HoldTime = Speed) ->
    Time = TotalTime - Speed,
    if
        (Time * Speed) > DistanceToWinn ->
            HoldTime;
        true ->
            last_winn(Game, HoldTime-1)
        end.    

first_winn({_, TotalTime, _}, HoldTime) when HoldTime >= TotalTime -> true;
first_winn(Game = {game, TotalTime, DistanceToWinn}, HoldTime = Speed) ->
    Time = TotalTime - Speed,
    if
        (Time * Speed) > DistanceToWinn ->
            HoldTime;
        true ->
            first_winn(Game, HoldTime+1)
        end.

ways_of_winning(Game = {_, TotalTime, _}) ->
    NoOfWinns = last_winn(Game, TotalTime) - first_winn(Game, 1) + 1,
    NoOfWinns.

calculate_games([]) -> 1;
calculate_games([Game | Games]) ->
    ways_of_winning(Game) * calculate_games(Games).

parse_file(IO) ->
    [_,_T] = string:split(io:get_line(IO, ""), ":"),
    [_,_D] = string:split(io:get_line(IO, ""), ":"),
    Times = [ list_to_integer(X) || X <- string:tokens(string:trim(_T), " ")],
    Distances = [ list_to_integer(X) || X <- string:tokens(string:trim(_D), " ")],
    % Game = [game || _ <- Times],
    lists:zipwith(fun(X,Y) -> {game, X, Y} end, Times, Distances).
    % lists:zip3(Game, Times, Distances).

task1() ->
    {ok, IO} = file:open('./src/input', [read]),
    Games = parse_file(IO),
    calculate_games(Games).
    % ?TBI(to_do).


parse_file_task2(IO) ->
    [_,_T] = string:split(io:get_line(IO, ""), ":"),
    [_,_D] = string:split(io:get_line(IO, ""), ":"),
    Time = list_to_integer(lists:flatten(string:tokens(string:trim(_T), " "))),
    Distance = list_to_integer(lists:flatten(string:tokens(string:trim(_D), " "))),
    {game, Time, Distance}.

task2() ->
    {ok, IO} = file:open('./src/input', [read]),
    Game = parse_file_task2(IO),
    ways_of_winning(Game).



%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
