% intersection.erl
-module(intersection).
-import(rand, []).
-import(timer, []).
-import(lists, []).
-compile([export_all]).

% Used intersection layout / numeration:
%   | 1 |
% --+   +--
% 4       2
% --+   +--
%   | 3 |

% 1 -> car going "down"
% 2 -> car going "left"
% 3 -> car going "up"
% 4 -> car going "right"

car_generator(N, Manager, LigthsCount) when N > 0 -> % N - Number of cars to generate
    Dir = rand:uniform(LigthsCount), % randomly choose direction according to the layout
    Manager ! {car, Dir}, % send a car to the intersection
    Wait = rand:uniform(5) * 200, % randomly choose period between generation: 200, 400, 600, 800 or 1000 ms
    timer:sleep(Wait), % wait with further generation - 200 miliseconds
    car_generator(N-1, Manager, LigthsCount); % generate rest of the N cars
car_generator(N, Manager, _) when N == 0 -> % N - Number of cars to generate
    Manager ! terminate; % send termination signal to the intersection
car_generator(N, Manager, LigthsCount) when N < 0 -> % N - Number of cars to generate
    Dir = rand:uniform(LigthsCount), % randomly choose direction according to the layout
    Manager ! {car, Dir}, % send a car to the intersection
    Wait = rand:uniform(5) * 200, % randomly choose period between generation: 200, 400, 600, 800 or 1000 ms
    timer:sleep(Wait), % wait with further generation - 200 miliseconds
    car_generator(N, Manager, LigthsCount). % generate cars forever - N never changes

traffic_light(Name, Light, Queue) ->
    print_light(Name, Light, length(Queue)),
    receive
        % Lights change signal
        change ->
            change_light(Name, Light, Queue);

        % Car approaching singal
        car ->
            traffic_light(Name, Light, Queue ++ [car]); % add cars to the end of the queue
        terminate -> terminate
    end.

% Temporary helper to print a light
print_light(Name, Light, Waiting) ->
    io:format("~p: Light: ~p; Waiting: ~p~n", [Name, Light, Waiting]).

% Helper function to change lights from red to green and back
change_light(Name, Light, Queue) when Light == red ->
    traffic_light(Name, green, Queue); % create a traffic light with changed light
change_light(Name, _, Queue) ->
    traffic_light(Name, red, Queue). % create a traffic light with changed light

manager(Lights) -> 
    receive
        {car, Dir} ->
            lists:nth(Dir, Lights) ! car, % send received car to the correct road - Lights[Dir] (lists:nth uses 1-based indexing)
            manager(Lights);
        terminate ->
            terminate_lights(Lights),
            terminate
    end.

% Helper functions to terminate lights every
terminate_lights([H|T]) ->
    H ! terminate,
    terminate_lights(T);
% when termianated every light - stop processing with an ok.
% needed to avoid signature mismatch errors.
terminate_lights([]) -> ok.

% Helper functions to send a signal to change lights every interval
change_interval([H|T], Interval) ->
    timer:send_interval(Interval, H, change),
    change_interval(T, Interval);
% when initialized sending to every light - stop processing with an ok.
% needed to avoid signature mismatch errors.
change_interval([], _) -> ok. 


% N - Number of cars to generate
% Interval - Interval of traffic lights change in ms
main(N, Interval) ->
    L1 = spawn(?MODULE, traffic_light, [l1, red, []]),
    % L2 = spawn(?MODULE, traffic_light, [l2, green, []]),
    % L3 = spawn(?MODULE, traffic_light, [l3, red]),
    % L4 = spawn(?MODULE, traffic_light, [l4, green]),
    Lights = [L1],
    change_interval(Lights, Interval), % enable changing lights once Interval (in ms) passes
    Manager = spawn(?MODULE, manager, [Lights]),
    spawn(?MODULE, car_generator, [N, Manager, length(Lights)]).
