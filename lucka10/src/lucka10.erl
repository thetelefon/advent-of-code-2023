%% @doc AoC
%% @author Robert Lasu
%% @copyright 2023 Robert Lasu <robert.lasu@gmail.com>

-module(lucka10).

-export([task1/0, task2/0]).

-compile(export_all).

-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).
-define(TBI(Msg), {tbi, {?MODULE, ?LINE, Msg}}).

% | is a vertical pipe connecting north and south.
%   124

% - is a horizontal pipe connecting east and west.
%   45

% L is a 90-degree bend connecting north and east.
%   76

% J is a 90-degree bend connecting north and west.
%   74

% 7 is a 90-degree bend connecting south and west.
%   55

% F is a 90-degree bend connecting south and east.
%   70

% . is ground; there is no pipe in this tile.
%   46

% S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
%   83


% Datastruct
% A Tile
%   {Left, Up, Right, Down}
% A Line
%   [Tiles]
% File
%   [Lines]


get_line(IO) -> io:get_line(IO, "").
setup() -> file:open('./src/input', [read]).

parse_line([]) -> [];
parse_line([124 | Lines]) -> [ {false, true, false, true} | parse_line(Lines)];
parse_line([45 | Lines]) ->  [ {true, false, true, false} | parse_line(Lines)];
parse_line([76 | Lines]) ->  [ {false, true, true, false} | parse_line(Lines)];
parse_line([74 | Lines]) ->  [ {true, true, false, false} | parse_line(Lines)];
parse_line([55 | Lines]) ->  [ {true, false, false, true} | parse_line(Lines)];
parse_line([70 | Lines]) ->  [ {false, false, true, true} | parse_line(Lines)];
parse_line([46 | Lines]) ->  [ {false, false, false, false} | parse_line(Lines)];
parse_line([83 | Lines]) ->  [ {true, true, true, true} | parse_line(Lines)];
parse_line(_) -> {error, unknown_symbol}.

parse_file(IO) ->
    case get_line(IO) of
        eof ->
            [];
        Err = {error, _} ->
            Err;
        Line ->
            [parse_line(string:trim(Line)) | parse_file(IO)]
        end.

search_line([], _, _) -> {false, 0};
search_line([{West, North, East, South} | _Lines], {West, North, East, South}, Acc) -> {true, Acc};
search_line([_ | Lines], SearchTile, Acc) -> search_line(Lines, SearchTile, Acc+1).

find_start([], _) -> {error, start_not_found};
find_start([Line | Lines], Row) ->
    case search_line(Line, {true, true, true, true}, 0) of
        {true, Column} ->
            {Row, Column};
        {false, _} ->
            find_start(Lines, Row+1)
        end.

find_start(Lines) -> find_start(Lines, 0).

connect_x_to({true, _, _, _}, {true, _, _, _}) -> {true, east};
connect_x_to({_, true, _, _}, {_, true, _, _}) -> {true, south};
connect_x_to({_, _, true, _}, {_, _, true, _}) -> {true, west};
connect_x_to({_, _, _, true}, {_, _, _, true}) -> {true, north};
connect_x_to(_,_) -> {false, no_connction}.


connect_x({true, _, _, _}, {Row, Column}, Labyrint) ->
    lists:nth(Column, lists:nth(Row+1, Labyrint));
connect_x({_, true, _, _}, {Row, Column}, Labyrint) ->
    lists:nth(Column+1, lists:nth(Row, Labyrint));
connect_x({_, _, true, _}, {Row, Column}, Labyrint) ->
    lists:nth(Column+2, lists:nth(Row+1, Labyrint));
connect_x({_, _, _, true}, {Row, Column}, Labyrint) ->
    lists:nth(Column+1, lists:nth(Row+2, Labyrint)).


get_next_tile({Row, Column}, Labyrint) when (Row > length(Labyrint)) or (Row < 1) or (Column < 1) -> {};
get_next_tile({Row, Column}, Labyrint) ->
    case lists:nth(Row, Labyrint) of
        [] ->
            {};
        Line ->
            if
                Column > length(Line) ->
                    {};
                true ->
                    case lists:nth(Column, Line) of
                        [] ->
                            {};
                        Tile ->
                            Tile
                        end
                    end
        end.

check_next_tiles({West, North, East, South}, {Row, Column}, Labyrint) ->
    WestConnector =  connect_x_to({false, false, West, false}, get_next_tile({Row+1, Column}, Labyrint)),      %lists:nth(Column, lists:nth(Row+1, Labyrint))), % lists:nth(Column, lists:nth(Row+1, Labyrint))
    NorthConnector = connect_x_to({false, false, false, North}, get_next_tile({Row, Column+1}, Labyrint)),     % lists:nth(Column+1, lists:nth(Row, Labyrint))), % lists:nth(Column+1, lists:nth(Row, Labyrint))
    EastConnector =  connect_x_to({East, false, false, false}, get_next_tile({Row+1, Column+2}, Labyrint)),      %lists:nth(Column+2, lists:nth(Row+1, Labyrint))), % lists:nth(Column+1, lists:nth(Row+2, Labyrint))
    SouthConnector = connect_x_to({false, South, false, false}, get_next_tile({Row+2, Column+1}, Labyrint)),     % lists:nth(Column+1, lists:nth(Row+2, Labyrint))), % lists:nth(Column+2, lists:nth(Row+1, Labyrint))

    [ X || X = {true, _} <- [WestConnector,NorthConnector,EastConnector,SouthConnector]].

search_valid_connections_for_tile({Row, Column}, Labyrint) ->
    Line = lists:nth(Row+1, Labyrint),
    {West, North, East, South} = lists:nth(Column+1, Line),
    check_next_tiles({West, North, East, South}, {Row, Column}, Labyrint).


check_reasonable_path([], _) -> {error, tile_not_connected};
check_reasonable_path([{_, D1}, {_,D2}], Direction) ->
    if
        D1 =:= Direction ->
            D2;
        D2 =:= Direction ->
            D1;
        true ->
            {error, tile_not_connected}
        end.

step({Row, Column}, LastDirection, Labyrint) ->
    ValidPaths = search_valid_connections_for_tile({Row, Column}, Labyrint),
    case check_reasonable_path(ValidPaths, LastDirection) of
        west ->
            NewCoordinate = {Row, Column-1},
            % NewTile = lists:nth(NewColumn+1, lists:nth(NewRow+1,Labyrint)),
            {east, NewCoordinate};
        north ->
            NewCoordinate = {Row-1, Column},
            % NewTile = lists:nth(NewColumn+1, lists:nth(NewRow+1, Labyrint)),
            {south, NewCoordinate};
        east ->
            NewCoordinate = {Row, Column+1},
            % NewTile = lists:nth(NewColumn+1, lists:nth(NewRow+1, Labyrint)),
            {west, NewCoordinate};
        south ->
            NewCoordinate = {Row+1, Column},
            % NewTile = lists:nth(NewColumn+1, lists:nth(NewRow+1, Labyrint)),
            {north, NewCoordinate}
        end.


step_through(Labyrint, Coordinates, LastDirection) ->
    case step(Coordinates, LastDirection, Labyrint) of
        {_, {102,118}} -> % Starting tile
            1;
        {ComesFrom, NewCoordinates} ->
            1 + step_through(Labyrint, NewCoordinates, ComesFrom)
        end.


step_through(Labyrint) ->
    Coordinates = find_start(Labyrint),
    step_through(Labyrint, Coordinates, west).

task1() ->
    {ok, IO} = setup(),
    Labyrint = parse_file(IO),
    step_through(Labyrint) div 2.
    % {Row, Column} = find_start(Labyrint),
    % step({Row, Column}, west, Labyrint).
    % search_valid_connections_for_tile({Row, Column}, Labyrint).

task2() ->
    {ok, IO} = setup(),
    parse_file(IO),
    ?TBI(to_do),
    
    file:close(IO).