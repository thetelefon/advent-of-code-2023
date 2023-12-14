%% @doc A OTP library.
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka8).
-include_lib("eunit/include/eunit.hrl").

-export([task1/0, task2/0, '_step_task2'/4]).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

get_line(IO) -> io:get_line(IO, "").

parse_nodes(IO, Map) ->
    case get_line(IO) of
        eof ->
            Map;
        Err = {error, _} ->
            Err;
        Line ->
            [Node, LR] = string:split(Line, " = "),
            [Left, Right] = string:split(string:slice(LR, 1, 8), ", "),
            NewMap = maps:put(Node, {Node, {Left, Right}}, Map),
            parse_nodes(IO, NewMap)
        end.

parse_file(IO) ->
    Directions = string:trim(get_line(IO)),
    get_line(IO),
    Nodes = parse_nodes(IO, #{}),
    {Directions, Nodes}.

step({[_,_,90], {_,_}}, _, _, Steps) -> {done, Steps};
step(CurrentNode, [], _, Steps) -> {CurrentNode, Steps};
% Left 76
% Right 82
step({_, {Left, Right}}, [Direction | Directions], Nodes, Steps) ->
    case Direction of
        76 ->
            NewNode = get_node(Nodes, Left),
            step(NewNode, Directions, Nodes, Steps+1);
        82 ->
            NewNode = get_node(Nodes, Right),
            step(NewNode, Directions, Nodes, Steps+1);
        _ ->
            {error, unknown_way}
        end.


get_steps(Directions, Nodes, CurrentNode, AccSteps) ->
    case step(CurrentNode, Directions, Nodes, AccSteps) of
        {done, Steps} ->
            Steps;
        {NewNode, NewSteps} ->
            get_steps(Directions, Nodes, NewNode, NewSteps)
        end.

get_steps(Directions, Nodes) ->
    FirstNode = get_node(Nodes, "AAA"),
    get_steps(Directions, Nodes, FirstNode, 0).

get_node(Nodes, Key) -> maps:get(Key, Nodes).

task1() ->
    {ok, IO} = file:open('./src/input', [read]),
    {Directions, Nodes} = parse_file(IO),
    get_steps(Directions, Nodes).


get_starting_nodes(Nodes) ->
    Keys = maps:keys(Nodes),
    [ maps:get(X, Nodes) || X <- [ X || X = [_,_,65] <- Keys]].


'_step_task2'(PID, Start, Directions, Nodes) ->
    Steps = get_steps(Directions, Nodes, Start, 0),
    PID ! {self(), Steps}.

start_child([], _, _) -> ok;
start_child([Start | StaringNodes], Directions, Nodes) ->
    spawn_link(lucka8, '_step_task2', [self(), Start, Directions, Nodes]),
    start_child(StaringNodes, Directions, Nodes).

count_steps(Acc) ->
    receive
        {_, Steps} ->
            if
                Acc < Steps ->
                    count_steps(Steps);
                true ->
                    count_steps(Acc)
                end
        after 100 ->
            Acc
        end.

run_concurrent(Directions, Nodes) ->
    StartingNodes = get_starting_nodes(Nodes),

    start_child(StartingNodes, Directions, Nodes),

    count_steps(0).

run_sequential(_,_,[]) -> 0;
run_sequential(Directions, Nodes, [Start | StartingNodes]) ->
    get_steps(Directions, Nodes, Start, 0) + run_sequential(Directions, Nodes, StartingNodes).


get_node(_,_,[]) -> {error, not_in_list};
get_node(_Node = {_, {Left, _}}, 76, [ N = {Left, _} | _Nodes]) -> N;
get_node(Node , 76, [ _ | Nodes]) -> get_node(Node, Nodes);
get_node(_Node = {_, {_, Right}}, 82, [ N = {Right, _} | _Nodes]) -> N;
get_node(Node , 82, [ _ | Nodes]) -> get_node(Node, Nodes).


step_task({_, {Left, _}}, 76, Nodes) -> maps:get(Left, Nodes);
step_task({_, {_, Right}}, 82, Nodes) -> maps:get(Right, Nodes).


step_all_tasks(_,_, []) -> [];
step_all_tasks(Direction, Nodes, [Node | NodeStarts]) ->
    [step_task(Node, Direction, Nodes) | step_all_tasks(Direction, Nodes, NodeStarts) ].


check_node_end({[_,_,90], _}) -> true;
check_node_end(_) -> false.


check_all_node_ends([]) -> true;
check_all_node_ends([Node | Nodes]) ->
    case check_node_end(Node) of
        false ->
            false;
        _ ->
            check_all_node_ends(Nodes)
        end.

run(D, [], N, P, S) -> run(D, D, N, P, S);
run(D, [Direction | Directions], Nodes, Pos, Steps) ->
    NewPos = step_all_tasks(Direction, Nodes, Pos),

    case check_all_node_ends(NewPos) of
        true ->
            Steps+1;
        _ ->
            run(D, Directions, Nodes, NewPos, Steps+1)
        end.

task2() ->
    {ok, IO} = file:open('./src/input2', [read]),
    {Directions, Nodes} = parse_file(IO),
    StartingNodes = get_starting_nodes(Nodes),
    run(Directions, Directions, Nodes, StartingNodes, 0).
    % {run_concurrent(Directions, Nodes),run_sequential(Directions, Nodes, StartingNodes)}.

%% ==================================== %%
%% =~~= Unit and integration tests =~~= %%
%% ==================================== %%
