%% @doc AoC
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka1).

-export([task1/0, task2/0]).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

get_line(IO) -> io:get_line(IO, "").
setup() -> file:open('/src/input', [read]).

parse_file(IO) ->
    ?TBI(to_do).


task1() ->
    {ok, IO} = setup(),
    parse_file(IO),
    ?TBI(to_do),
    
    file:close(IO).

task2() ->
    {ok, IO} = setup(),
    parse_file(IO),
    ?TBI(to_do),
    
    file:close(IO).