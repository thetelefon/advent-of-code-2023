%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka9).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0]).

-compile(export_all).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

get_line(IO) -> io:get_line(IO, "").

parse_file(IO) ->
    case get_line(IO) of
        eof ->
            [];
        Err = {error, _} ->
            Err;
        _Line ->
            [ {line, [list_to_integer(X) || X <- string:tokens(string:trim(_Line), " ")], {}} | parse_file(IO) ]
        end.


check_if_last_line([]) -> true;
check_if_last_line([0 | Line]) -> check_if_last_line(Line);
check_if_last_line(_) -> false.

calculate_next_line([_ | []]) -> [];
calculate_next_line([First, Second | Rest]) ->
    [ (Second - First) | calculate_next_line([Second | Rest])].


calculate_line({line, Curr, {}}) ->
    NextLine = calculate_next_line(Curr),
    case check_if_last_line(NextLine) of
        true ->
            {line, Curr, {line, NextLine, {}}};
        false ->
            {line, Curr, calculate_line({line, NextLine, {}}) }
        end;
calculate_line({line, _Curr, Next}) -> calculate_line(Next).

calculate_all_lines([]) -> [];
calculate_all_lines([Line | Lines]) ->
    [ calculate_line(Line) | calculate_all_lines(Lines) ].


get_expected_pos_value({line, _, {}}) -> 0;
get_expected_pos_value({line, Curr, Next}) ->
    lists:last(Curr) + get_expected_pos_value(Next).

get_expected_neg_value({line, _, {}}) -> 0;
get_expected_neg_value({line, [First | _], Next}) ->
    First - get_expected_neg_value(Next).


get_all_expected_pos_values([]) -> 0;
get_all_expected_pos_values([Line | Lines]) ->
    get_expected_pos_value(Line) + get_all_expected_pos_values(Lines).

get_all_expected_neg_values([]) -> 0;
get_all_expected_neg_values([Line | Lines]) ->
    get_expected_neg_value(Line) + get_all_expected_neg_values(Lines).

task1() ->
    {ok, IO} = file:open('./src/input', [read]),
    Lines = parse_file(IO),
    CalculatedLines = calculate_all_lines(Lines),
    get_all_expected_pos_values(CalculatedLines).


task2() ->
    {ok, IO} = file:open('./src/input', [read]),
    Lines = parse_file(IO),
    CalculatedLines = calculate_all_lines(Lines),
    get_all_expected_neg_values(CalculatedLines).


% {line, [ ListOfNumbers ], { LowerLineStruct }}


%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
