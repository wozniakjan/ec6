-module(cli).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private functions                                                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_pixel(error, _Players) -> " ";
get_pixel(Player, Players) ->
    {ok, {P, _Id}} = Player,
    {_, Sign} = lists:keyfind(P, 1, Players),
    Sign.

get_pixel(C, R, Board, Players) ->
    Val = dict:find({C, R}, Board),
    get_pixel(Val, Players).

draw_columns(0, _R, _Board, _Players) -> 
    io:format("|~n");
draw_columns(C, R, Board, Players) ->
    Pixel = get_pixel(C, R, Board, Players),
    io:format("~s",[Pixel]),
    draw_columns(C-1, R, Board, Players).

draw_rows(0, _Board, _Players) ->
    Columns = arbiter:board_size(),
    io:format("~s~n~n",[lists:map(fun(_)->"=" end, lists:seq(0,Columns))]);
draw_rows(R, Board, Players) ->
    Columns = arbiter:board_size(),
    draw_columns(Columns, R, Board, Players),
    draw_rows(R-1, Board, Players).

draw_board(Board, Players) ->
    Rows = arbiter:board_size(), 
    draw_rows(Rows, Board, Players).


loop(Players) -> 
    receive
        {draw_board, Board} -> 
            draw_board(Board, Players),
            loop(Players);
        exit ->
            ok;
        {finished, {winners, L}} ->
            [FirstWinner|_] = L,
            {_, Winner} = lists:keyfind(FirstWinner, 1, Players),
            io:format("Player ~s won~n",[Winner]),
            loop(Players)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client API                                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Players) ->
    Signs = ["X", "O"] ++ lists:seq(3, length(Players)),
    P = lists:zip(Players, Signs),
    {ok, spawn(?MODULE, loop, [P])}.

set_board(Board) ->
    gen_server:cast({draw_board, Board}).
