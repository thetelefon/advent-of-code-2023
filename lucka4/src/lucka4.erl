%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka4).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0, task2/0]).


-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

calculate_result(Num) when Num =< 0 -> 0;
calculate_result(1) -> 1;
calculate_result(Num) -> 2 * calculate_result(Num-1).


winnings(Played, WinningNo) ->
    [ X || X <- Played, Y <- WinningNo, X == Y].

no_of_winnings(Played, WinningNo) ->
    length(winnings(Played, WinningNo)).

get_played_no(Line) ->
    {match, [{_Start, _Len}]} = re:run(Line, "\\|[0-9\\s]*"),
    Start = _Start + 2,
    Len = _Len - 3,
    StringOfNum = string:slice(Line, Start, Len),
    [ list_to_integer(X) || X <- string:tokens(StringOfNum, " ")].

get_winning_no(Line) ->
    {match, [{_Start, _Len}]} = re:run(Line, ":[\\s\\S]*\\|"),
    Start = _Start + 2,
    Len = _Len - 4,
    StringOfNum = string:slice(Line, Start, Len),
    [ list_to_integer(X) || X <- string:tokens(StringOfNum, " ")].

parse_lines(IO) ->
    case io:get_line(IO, "") of
        eof ->
            0;
        Err = {error,_} ->
            Err;
        Line ->
            WinningNo = get_winning_no(Line),
            PlayedNo = get_played_no(Line),
            NoOfWinnings = no_of_winnings(PlayedNo,WinningNo),
            calculate_result(NoOfWinnings) + parse_lines(IO)
        end.


task1() ->
    {ok, IO} = file:open('./src/input', [read]),
    parse_lines(IO).
    % ?TBI(to_do).


add_tickets(TicketList, _, 0) -> TicketList;
add_tickets([], NumberOfTickets, NoOfWinnings) -> [ NumberOfTickets | add_tickets([], NumberOfTickets, NoOfWinnings - 1) ];
add_tickets([First | TicketList], NumberOfTickets, NoOfWinnings) -> [First + NumberOfTickets | add_tickets(TicketList, NumberOfTickets, NoOfWinnings - 1)].



game_help(Line, []) ->
    WinningNo = get_winning_no(Line),
    PlayedNo = get_played_no(Line),
    NoOfWinnings = no_of_winnings(PlayedNo, WinningNo),
    NewTicketList = add_tickets([], 1, NoOfWinnings),
    {1, NewTicketList};

game_help(Line, [Extra | TicketList]) ->
    WinningNo = get_winning_no(Line),
    PlayedNo = get_played_no(Line),
    NoOfWinnings = no_of_winnings(PlayedNo, WinningNo),
    NewTicketList = add_tickets(TicketList, Extra + 1, NoOfWinnings),
    {Extra + 1, NewTicketList}.


game(IO, TicketList) ->
    case io:get_line(IO, "") of
        eof ->
            0;
        Err = {error, _} ->
            Err;
        Line ->
            {NoOfTickets, NewTicketList} = game_help(Line, TicketList),
            NoOfTickets + game(IO, NewTicketList)
        end.

task2() ->
    {ok, IO} = file:open('./src/input', [read]),
    game(IO, []).

%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
