%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka7).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0]).
-compile(export_all).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

get_line(IO) -> io:get_line(IO, "").


get_number_of_elem(Check, List) ->
    length([ X || X <- List, X =:= Check]).

uniq([]) -> [];
uniq([H|T]) ->
    [H | [X || X <- uniq(T), H /= X]].

no_of_uniqs(T) -> length(uniq(T)).

count(_, []) -> 0;
count(No, [No | Rest]) -> 1 + count(No, Rest);
count(No, [_ | Rest]) -> 0 + count(No, Rest).

has_at_least(_, []) -> false;
has_at_least(Least, Hand = [First | Rest]) ->
    case count(First, Hand) of
        Least ->
            true;
        _ ->
            has_at_least(Least, Rest)
        end.

no_of_card(_, []) -> 0;
no_of_card(Card, [Card | Hand]) -> 1 + no_of_card(Card, Hand);
no_of_card(Card, [_ | Hand]) -> no_of_card(Card, Hand).

has_pair(Hand) ->
    Bool = has_at_least(2, Hand),
    if
        Bool ->
            {true, 2};
        true ->
            {false, 0}
        end.

has_pair2(Hand) ->
    1.

has_two_pair(Hand) ->
    Bool = has_at_least(2, Hand),
    Uniqs = no_of_uniqs(Hand),

    if
        Bool and (Uniqs =:= 3) ->
            {true, 3};
        true ->
            {false, 0}
        end.

has_two_pair2(Hand) ->
    1.

has_three(Hand) ->
    Bool = has_at_least(3, Hand),
    if
        Bool ->
            {true, 4};
        true ->
            {false, 0}
        end.

has_three2(Hand) ->
    1.

has_house(Hand) ->
    Uniqs = no_of_uniqs(Hand),
    Has4 = has_at_least(4, Hand),
    Bool = has_at_least(3, Hand),
    if
        Bool and (Has4 =:= false) and (Uniqs =:= 2) ->
            {true, 5};
        true ->
            {false, 0}
        end.

has_house2(Hand) ->
    1.

has_four(Hand) ->
    Bool = has_at_least(4, Hand),
    if
        Bool ->
            {true, 6};
        true ->
            {false, 0}
        end.

has_four2(Hand) ->
    1.

has_five(Hand) ->
    Bool = has_at_least(5, Hand),
    if
        Bool ->
            {true, 7};
        true ->
            {false, 0}
        end.

has_five2(Hand) ->
    Bool = has_at_least(3, Hand),
    NoOfJ = no_of_card(1, Hand),
    if
        Bool and (NoOfJ =:= 2) ->
            {true, 7};
        true ->
            {false, 0}
        end.

calculate_type(_, []) -> 1;
calculate_type(Hand, [Fun | Rest]) ->
    {Bool, Result} = Fun(Hand),
    if
        Bool ->
            % io:fwrite("==~nHand: ~p~nResult: ~p~n~n", [Hand, Result]),
            Result;
        true ->
            calculate_type(Hand, Rest)
        end.


calculate_type(Hand) ->
    Func = [fun has_five/1, fun has_four/1, fun has_house/1, fun has_three/1, fun has_two_pair/1, fun has_pair/1],
    calculate_type(Hand, Func).

calculate_type_task2(Hand) ->
    Func = [fun has_five2/1, fun has_four2/1, fun has_house2/1, fun has_three2/1, fun has_two_pair2/1, fun has_pair2/1],
    calculate_type(Hand, Func).


get_values([]) -> [];
get_values([_Value | Values]) ->
    case [_Value] of
        "A" ->
            [ 14 | get_values(Values)];
        "K" ->
            [ 13 | get_values(Values)];
        "Q" ->
            [ 12 | get_values(Values)];
        "J" ->
            [ 1 | get_values(Values)];
        "T" ->
            [ 10 | get_values(Values)];
        _Num ->
            {Num, _} = string:to_integer(_Num),
            [Num | get_values(Values)]
        end.

parse_hand(IO, CalcFun) ->
    case get_line(IO) of
        eof ->
            eof;
        _Line ->
            Line = string:trim(_Line),
            [Hand, _Bid] = string:tokens(Line, " "),
            Values = get_values(Hand),
            Type = CalcFun(Values),
            {Bid, _} = string:to_integer(_Bid), 
            {Type, Values, Bid}
        end.

parse_file(IO, CalcFun) ->
    case parse_hand(IO, CalcFun) of
        eof ->
            [];
        Hand ->
            [ Hand | parse_file(IO, CalcFun) ]
        end.

get_rank(_, []) -> [];
get_rank(No, [Hand={No, _,_} | Rest]) -> [Hand | get_rank(No, Rest)];
get_rank(No, [_ | Rest]) -> get_rank(No, Rest).

sort_rank_by_cards(List) ->
    lists:keysort(2, List).

sort_hands(_, 8) -> [];
sort_hands(Hands, No) ->
    [ sort_rank_by_cards(get_rank(No, Hands)) | sort_hands(Hands, No+1)].

sort_hands(Hands) ->
    lists:flatten(sort_hands(Hands, 1)).
    % One = sort_rank_by_cards(get_rank(1, Hands)),
    % Two = sort_rank_by_cards(get_rank(2, Hands)),
    % Three = sort_rank_by_cards(get_rank(3, Hands)),
    % Four = sort_rank_by_cards(get_rank(4, Hands)),
    % Five = sort_rank_by_cards(get_rank(5, Hands)),
    % Six = sort_rank_by_cards(get_rank(6, Hands)),
    % lists:merge([One | [Two | [Three | [Four | [Five | [Six]]]]]]).


calculate_winnings([], _) -> 0;
calculate_winnings([_Hand = {_,_,Bid} | Hands], Rank) ->
    % io:fwrite("==~nHand: ~p~nRank: ~p~nResult: ~p~n~n", [_Hand, Rank, (Bid * Rank)]),
    Bid * Rank + calculate_winnings(Hands, Rank+1).


iterate_and_set_rank([], _) -> [];
iterate_and_set_rank([Hand = {_Type, _Cards, Bid} | Hands], Rank) ->
    Value = Bid * Rank,
    [ {Hand, Rank, Value} | iterate_and_set_rank(Hands, Rank+1) ].

iterate_and_set_rank(Hands) -> iterate_and_set_rank(Hands, 1).

task1() ->
    {ok, IO} = file:open('./src/input',[read]),
    Hands = lists:sort(parse_file(IO, fun calculate_type/1)),
    SortedHands = sort_hands(Hands),
    % iterate_and_set_rank(SortedHands),
    calculate_winnings(SortedHands, 1).


task2() ->
    {ok, IO} = file:open('./src/input', [read]),
    Hands = lists:sort(parse_file(IO, fun calculate_type_task2/1)),
    SortedHands = sort_hands(Hands),
    calculate_winnings(SortedHands, 1).

%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
