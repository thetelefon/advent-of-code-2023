%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka2).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0, task2/0]).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).
-define(RED, 12).
-define(GRN, 13).
-define(BLU, 14).


get_game_num(Line) ->
    Game = re:run(Line, "Game [0-9]+"),
    case Game of
        {match, [{0, Len}]} ->
            NumLen = Len - 5,
            {GameNum, _} = string:to_integer(string:slice(Line, 5, NumLen)),
            GameNum;
        _ ->
            Game
        end.

get_highest(_, [], Highest) -> Highest;
get_highest(Line, [[{Pos, Length}]|RestList], Highest) ->
    {Num, _} = string:to_integer(string:slice(Line, Pos, Length)), 
    if
        Num < Highest ->
            get_highest(Line, RestList, Highest);
        true ->
            get_highest(Line, RestList, Num)
        end.

get_highest(Line, List) ->
    get_highest(Line, List, 0).

get_red(Line) ->
    FilteredLine = re:run(Line, "[0-9]+ red", [global]),
    case FilteredLine of
        {match, List} ->
            get_highest(Line, List);
        _ ->
            FilteredLine
        end.

get_green(Line) ->
    FilteredLine = re:run(Line, "[0-9]+ green", [global]),
    case FilteredLine of
        {match, List} ->
            get_highest(Line, List);
        _ ->
            FilteredLine
        end.

get_blue(Line) ->
    FilteredLine = re:run(Line, "[0-9]+ blue", [global]),
    case FilteredLine of
        {match, List} ->
            get_highest(Line, List);
        _ ->
            FilteredLine
        end.


calculate_game2(Line) ->
    Red = get_red(Line),
    Green = get_green(Line),
    Blue = get_blue(Line),
    Sum = Red*Green*Blue,
    Sum.

calculate_game(Line) ->
    Game = get_game_num(Line),
    Red = get_red(Line),
    Green = get_green(Line),
    Blue = get_blue(Line),
    
    if 
        Red > ?RED ->
            0;
        Green > ?GRN ->
            0;
        Blue > ?BLU ->
            0;
        true ->
            Game
        end.




parse_lines(IO, Acc) ->
    Line = io:get_line(IO, ""),
    case Line of
        eof ->
            Acc;
        Err = {error, _} ->
            Err;
        _ ->
            Value = calculate_game(Line),
            parse_lines(IO, (Acc + Value))
        end.


parse_lines_task2(IO, Acc) ->
    Line = io:get_line(IO, ""),
    case Line of
        eof ->
            Acc;
        Err = {error, _} ->
            Err;
        _ ->
            Value = calculate_game2(Line),
            parse_lines_task2(IO, (Acc + Value))
        end.

task2() ->
    {ok, IO} = file:open("./src/input", [read, {encoding, utf8}]),
    parse_lines_task2(IO, 0).



task1() ->
    {ok, IO} = file:open("./src/input", [read, {encoding, utf8}]),
    parse_lines(IO, 0).

%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
