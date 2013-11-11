-record(stone, {player, x, y, id}).
-record(game_state, {state = just_started, winners=[], losers=[]}).
-record(arbiter_state, {active_player, players, board=dict:new(), 
        game=#game_state{}, observers=[], time_check}).

% #game_state.state = { just_started | playing | finished }
