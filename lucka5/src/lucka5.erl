%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka5).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0, task2/0]).

-compile(export_all).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).


get_line(IO) ->
    case io:get_line(IO, "") of
        eof ->
            [];
        Err = {error, _} ->
            Err;
        Line ->
            Line
        end.


parse_transition_help(IO, List) ->
    case get_line(IO) of
        "\n" ->
            [];
        [] ->
            [];
        Line ->
            [ list_to_tuple( [ list_to_integer(X) || X <- string:tokens(string:trim(Line), " ")] ) | parse_transition_help(IO, List) ]
        end.

parse_transition(IO) ->
    case get_line(IO) of
        [] ->
            [];
        Name ->
            Transitions = parse_transition_help(IO, []),
            {string:trim(Name), Transitions}
        end.

parse_transitions(IO) ->
    case parse_transition(IO) of
        [] ->
            [];
        Transition ->
            [Transition | parse_transitions(IO)]
        end.

parse_seeds(IO) ->
    Line = get_line(IO),
    get_line(IO),
    [_, _Seeds] = string:split(Line, ":"),
    TrimedSeeds = string:trim(_Seeds),
    [ {seed, list_to_integer(X)} || X <- string:tokens(TrimedSeeds, " ")].


parse_seeds_task2_help([], Seeds) -> Seeds;
parse_seeds_task2_help([Start | [Len | Rest]], Seeds) ->
    [{seeds, list_to_integer(Start), list_to_integer(Len)} | parse_seeds_task2_help(Rest, Seeds)].

parse_seeds_task2(IO) ->
    Line = get_line(IO),
    get_line(IO),
    [_, _Seeds] = string:split(Line, ":"),
    TrimedSeeds = string:trim(_Seeds),
    ListedSeeds = string:tokens(TrimedSeeds, " "),
    % [ {seed, list_to_integer(X), list_to_integer(Y)} || [X | [Y]] <- string:tokens(TrimedSeeds, " ")].
    parse_seeds_task2_help(ListedSeeds, []).

parse_file(IO) ->
    Seeds = parse_seeds(IO),
    Transitions = parse_transitions(IO),
    {Seeds, Transitions}.

calculate_next_transition(Value, []) -> Value;
calculate_next_transition(Value, [{DstSrt, SrcSrt, Len} | Transitions]) ->
    if
        (SrcSrt =< Value) and ((SrcSrt+Len) >= Value) ->
            DstSrt + (Value - SrcSrt);
        true ->
            calculate_next_transition(Value, Transitions)
        end.


calculate_seed_to_destination(Value, []) -> Value;
calculate_seed_to_destination(Value, [ {_Name, Transition} | Transitions]) ->
    NewValue = calculate_next_transition(Value, Transition),

    calculate_seed_to_destination(NewValue, Transitions).


calculate_lowest_destination_help({[], _}, Lowest) -> Lowest;
calculate_lowest_destination_help({[{seed, Seed} | Seeds], Transitions}, Lowest) ->
    Low = calculate_seed_to_destination(Seed, Transitions),
    if
        (Low > Lowest) and (Lowest =/= 0) ->
            calculate_lowest_destination_help({Seeds, Transitions}, Lowest);
        true ->
            calculate_lowest_destination_help({Seeds, Transitions}, Low)
        end.

calculate_lowest_destination(Info = {_Seeds, _Transitions}) ->
    calculate_lowest_destination_help(Info, 0).

calculate_seed_range({_, _, Len}, _, Lowest) when (Len < 0) ->
    io:fwrite("Hello darkness my old friend~n"),
    Lowest;
calculate_seed_range({seeds, Start, Len}, Transitions, Lowest) ->
    Low = calculate_seed_to_destination(Start, Transitions),
    io:fwrite("Len: ~p~n", [Len]),

    if
        (Low > Lowest) and (Lowest =/= 0) ->
            calculate_seed_range({seeds, Start+1, Len-1}, Transitions, Lowest);
        true ->
            calculate_seed_range({seeds, Start+1, Len-1}, Transitions, Low)
        end.

calculate_lowest_destination_task2_help({[], _}, Lowest) -> Lowest;
calculate_lowest_destination_task2_help({[SeedRange | Seeds], Transitions}, Lowest) ->
    Low = calculate_seed_range(SeedRange, Transitions, 0),

    if
        (Low > Lowest) and (Lowest =/= 0) ->
            calculate_lowest_destination_task2_help({Seeds, Transitions}, Lowest);
        true ->
            calculate_lowest_destination_task2_help({Seeds, Transitions}, Low)
        end.

calculate_lowest_destination_task2(Info = {_Seeds, _Transitions}) ->
    calculate_lowest_destination_task2_help(Info, 0).

task1() ->
    {ok, IO} = file:open("./src/input", [read]),
    Info = parse_file(IO),
    calculate_lowest_destination(Info).

task2() ->
    {ok, IO} = file:open("./src/input", [read]),
    Seeds = parse_seeds_task2(IO),
    Transitions = parse_transitions(IO),
    calculate_lowest_destination_task2({Seeds, Transitions}).



%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
