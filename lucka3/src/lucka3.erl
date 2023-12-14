%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka3).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0]).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).


setup_line(Line) ->
    % {match, Numbers} = re:run(Line, "[0-9]+", [global]),
    case re:run(Line, "[0-9]+", [global]) of
        {match, Numbers} ->
            FlattenNo = lists:flatten(Numbers),
            case re:run(Line, "(?![0-9\\.])[\\S]", [global]) of
                nomatch ->
                    {FlattenNo, []};
                {match, Symbols} ->
                    FlattenSym = lists:flatten(Symbols),
                    {FlattenNo, FlattenSym}
                end;
        nomatch ->
            {};
        E ->
            E
        end.


get_next_line(IO) ->
    case io:get_line(IO, "") of
        eof ->
            [];
        Err = {error, _} ->
            Err;
        Line ->
            Line
        end.

check_index(_, []) -> false;
check_index(I = {Start, Len}, [{SymStart, SymLen} | Rest]) ->
    % If I is erlier than Symbol
    % if Left <= 0 SymStart is erlier than I end.
    Left = SymStart - (Start+Len),

    % if I is later than Symbol
    % if Right <= 0Start is erlier than Sym
    Right = Start - (SymStart + SymLen),
    
    if
        (Left =< 0) and (Right =< 0) ->
            true;
        true ->
            check_index(I, Rest)
        end.

get_num(Line, {Start, Len}) ->
    case string:to_integer(string:slice(Line, Start, Len)) of
        {Num, _} ->
            Num;
        Num ->
            Num
        end.


check_indexies(Index, {_,Prev}, {_,Curr}, {}) ->
    PrevBool = check_index(Index, Prev),
    CurrBool = check_index(Index, Curr),
    if
        PrevBool or CurrBool ->
            true;
        true ->
            false
        end;
check_indexies(Index, {}, {_,Curr},{_,Next}) ->
    CurrBool = check_index(Index, Curr),
    NextBool = check_index(Index, Next),
    if
        CurrBool or NextBool ->
            true;
        true ->
            false
        end;
check_indexies(Index, {_, Prev}, {_,Curr}, {_,Next}) ->
    P = check_index(Index, Prev),
    C = check_index(Index, Curr),
    N = check_index(Index, Next),
    if
        P or C or N ->
            true;
        true ->
            false
        end.


check_line(_, {_, {[], _}, _}) -> 0;
check_line(CurrentLine, { P, Curr = {[Index | Rest], Sym}, N }) ->
    Bool = check_indexies(Index, P, Curr, N),

    if
        Bool ->
            get_num(CurrentLine, Index) +  check_line(CurrentLine, {P, {Rest, Sym}, N});
        true ->
            0 + check_line(CurrentLine, {P, {Rest, Sym}, N})
        end.

parse_lines(_, {_, [], _}) -> 0;
parse_lines(IO, {Previous, Current, Next}) ->
    PrevIndex = setup_line(Previous),
    CurrIndex = setup_line(Current),
    NextIndex = setup_line(Next),

    Sum = check_line(Current, {PrevIndex, CurrIndex, NextIndex}),

    Sum + parse_lines(IO, {Current, Next, get_next_line(IO)}).

task1() ->
    {ok, IO} = file:open("./src/input",[read]),
    Current = get_next_line(IO),
    Next = get_next_line(IO),
    parse_lines(IO, {[], Current, Next}).
    % ?TBI(to_do).


%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
