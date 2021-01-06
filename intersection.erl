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

% Process generating cars
car_generator(N, Printer, Lights, Coeff) when N > 0 -> % N - Number of cars to generate
    Dir = rand:uniform(length(Lights)), % randomly choose direction according to the layout
    lists:nth(Dir, Lights) ! car, % send a car to the intersection 
    Wait = rand:uniform(5) * Coeff, % randomly choose period between generation
    timer:sleep(Wait), % wait with further generation
    car_generator(N-1, Printer, Lights, Coeff); % generate rest of the N cars

car_generator(N, Printer, Lights, _) when N == 0 -> % N - Number of cars to generate
    Printer ! terminate, % send termination signal to the intersection printer
    terminate_lights(Lights); % send termination signal to the intersection

car_generator(N, Printer, Lights, Coeff) when N < 0 -> % N - Number of cars to generate
    Dir = rand:uniform(length(Lights)), % randomly choose direction according to the layout
    lists:nth(Dir, Lights) ! car, % send a car to the intersection
    Wait = rand:uniform(5) * Coeff, % randomly choose period between generation
    timer:sleep(Wait), % wait with further generation
    car_generator(N, Printer, Lights, Coeff). % generate cars forever - N never changes

% Helper functions to terminate lights every
terminate_lights([H|T]) ->
    H ! terminate,
    terminate_lights(T);
% when termianated every light - stop processing with an ok.
% needed to avoid signature mismatch errors.
terminate_lights([]) -> ok.

% Process that's allowing cars to pass through the intersection
car_runner(Light) ->
    Wait = rand:uniform(3) * 200, % randomly choose period between check: 200, 400, 600 milliseconds
    timer:sleep(Wait), % wait with checking light
    Light ! {self(), check},
    receive
        g -> % if light is green send a signal that car went through
            Light ! car_went;
        _ -> ok % else continue without doing anything
    end,
    car_runner(Light).

% Individual traffic light process
traffic_light(Name, Light, Queue) ->
    receive
        % Answer to light runner check
        {From, check} ->
            From ! Light, 
            traffic_light(Name, Light, Queue);

        % Answer to intersection printer check
        {From, get_info} -> 
            From ! {Name, Light, length(Queue)},
            traffic_light(Name, Light, Queue);

        % Delete car from queue if it went through 
        car_went ->
            if
                length(Queue) > 0 -> % if queue isn't empty
                    [_|T] = Queue,
                    traffic_light(Name, Light, T);
                true -> % else go on with your life like nothing happened
                    traffic_light(Name, Light, Queue)
            end;

        % Lights change signal
        change ->
            change_light(Name, Light, Queue);

        % Car approaching singal
        car ->
            traffic_light(Name, Light, Queue ++ [car]); % add cars to the end of the queue
        % TODO: queue should be emptied before terminating lights
        terminate -> terminate 
    end.

% Helper function to change lights from red to green and back
change_light(Name, Light, Queue) when Light == r ->
    traffic_light(Name, g, Queue); % create a traffic light with changed light
change_light(Name, _, Queue) ->
    traffic_light(Name, r, Queue). % create a traffic light with changed light


% Process messaging about light changes
lights_changer(Lights, Interval) -> lights_changer(Lights, Interval, 0).
lights_changer(Lights, Interval, N) ->
    Index = N rem 2 + 1, % calculate index for this iteration - 1 or 2
    CurrentInterval = lists:nth(Index, Interval), % get element from Interval list - 1 based indexing
    timer:sleep(CurrentInterval), % wait for Interval ms to pass
    message_change(Lights), % send change notification to every light
    lights_changer(Lights, Interval, N+1). % go again with incremented N

% Helper function to message every light about the change
message_change([H|T]) ->
    H ! change,
    message_change(T);
% when initialized sending to every light - stop processing with an ok.
% needed to avoid signature mismatch errors.
message_change([]) -> ok.

% Process printing the intersection
intersection_printer(Lights) ->
    check_lights(Lights, self()), % send a query to every light to get Queue length and Light
    Data = receive_info(length(Lights)), % Parse received data
    if
        Data == terminate -> 
            io:format("=====================================~nAll cars have been generated. Ending.~n"),
            terminate; % if received a terminate signal - end
        true ->
            io:format("~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n~n"), % "clear" the console
            io:format("##| ~p |##~n##| ~p |##~n--+   +--~n~p|~p   ~p|~p~n--+   +--~n##| ~p |##~n##| ~p |##~n", Data), % print the intersection
            timer:sleep(50), % wait before next print-out
            intersection_printer(Lights)
    end.

% Helper function to check lights and queue lengths
check_lights([H|T], From) ->
    H ! {From, get_info},
    check_lights(T, From);
% when initialized sending to every light - stop processing with an ok.
% needed to avoid signature mismatch errors.
check_lights([], _) -> ok.

% Helper function to receive data from every light
receive_info(N) ->
    receive_info(N, [x, x, x, x, x, x, x, x]). % start with a symbol for closed road - x
receive_info(N, List) when N > 0 ->
    receive % Each light changes correct spots in List
        {l1, Light, Length} ->
            Tmp = replace(List, 1, Length),
            New = replace(Tmp, 2, Light),
            receive_info(N-1, New); % Make sure tor receive from every light
        {l2, Light, Length} ->
            Tmp = replace(List, 5, Light),
            New = replace(Tmp, 6, Length),
            receive_info(N-1, New); % Make sure tor receive from every light
        {l3, Light, Length} ->
            Tmp = replace(List, 7, Light),
            New = replace(Tmp, 8, Length),
            receive_info(N-1, New); % Make sure tor receive from every light
        {l4, Light, Length} ->
            Tmp = replace(List, 3, Length),
            New = replace(Tmp, 4, Light),
            receive_info(N-1, New); % Make sure tor receive from every light
        terminate -> terminate
    end; 
receive_info(_, List) ->
    List. % when received info from every light - return the list

% Helper function to replace Element of a List on a given Index (1-based indexing)
replace(List, Index, Element) ->
    lists:sublist(List,Index-1) ++ [Element] ++ lists:nthtail(Index,List).

% Helper function to spawn traffic lights processes
spawn_lights(LightsCount) ->
    if
        LightsCount == 1 ->
            L1 = spawn(?MODULE, traffic_light, [l1, r, []]),
            spawn(?MODULE, car_runner, [L1]),
            Lights = [L1];

        LightsCount == 2 ->
            L1 = spawn(?MODULE, traffic_light, [l1, r, []]),
            spawn(?MODULE, car_runner, [L1]),
            L2 = spawn(?MODULE, traffic_light, [l2, g, []]),
            spawn(?MODULE, car_runner, [L2]),
            Lights = [L1, L2];

        LightsCount == 3 ->
            L1 = spawn(?MODULE, traffic_light, [l1, r, []]),
            spawn(?MODULE, car_runner, [L1]),
            L2 = spawn(?MODULE, traffic_light, [l2, g, []]),
            spawn(?MODULE, car_runner, [L2]),
            L3 = spawn(?MODULE, traffic_light, [l3, r, []]),
            spawn(?MODULE, car_runner, [L3]),
            Lights = [L1, L2, L3];

        LightsCount == 4 ->
            L1 = spawn(?MODULE, traffic_light, [l1, r, []]),
            spawn(?MODULE, car_runner, [L1]),
            L2 = spawn(?MODULE, traffic_light, [l2, g, []]),
            spawn(?MODULE, car_runner, [L2]),
            L3 = spawn(?MODULE, traffic_light, [l3, r, []]),
            spawn(?MODULE, car_runner, [L3]),
            L4 = spawn(?MODULE, traffic_light, [l4, g, []]),
            spawn(?MODULE, car_runner, [L4]),
            Lights = [L1, L2, L3, L4];

        true ->
            Lights = invalid
    end,
    Lights.


% various Quality of Life functions - with predetermined parameters
main() -> main(-1, 2000, 150, 4).
main(Interval) -> main(-1, Interval, 150, 4).
main(N, Interval) -> main(N, Interval, 150, 4).
main(N, Interval, LightsCount) -> main(N, Interval, 150, LightsCount).
% N - Number of cars to generate
% Interval - Interval of traffic lights change in ms
% Coeff - coefficient of random car generation delay
% LightsCount - number of working traffic lights (between 1 and 4, otherwise program won't execute properly)

% To support different lengths of red and green lights check if interval is an integer (same duration of red and green)
main(N, Interval, Coeff, LightsCount) when is_integer(Interval) -> main(N, [Interval, Interval], Coeff, LightsCount);
% or if interval is a list (different durations)
% Treating first element of the list as duration of red, second as duration of green light on traffic light 1 & 3 
main(N, Interval, Coeff, LightsCount) ->
    Lights = spawn_lights(LightsCount),
    if
        Lights == invalid -> % if number of traffic lights was invalid display appropriate message
            io:format("This number of lights (~p) is invalid. Please use number between 1 and 4.~n", [LightsCount]);
        true -> % else start the intersection
            spawn(?MODULE, lights_changer, [Lights, Interval]), % enable changing lights once Interval (in ms) passes
            Printer = spawn(?MODULE, intersection_printer, [Lights]),
            spawn(?MODULE, car_generator, [N, Printer, Lights, Coeff]) % start car generator
    end.
