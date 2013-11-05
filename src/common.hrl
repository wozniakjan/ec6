-record(stone, {player, x, y, id}).
-record(game_state, {state = just_started, winners=[], losers=[]}).
-record(arbiter_state, {active_player, players, board=dict:new(), 
        game=#game_state{}, supervisor, time_check}).
