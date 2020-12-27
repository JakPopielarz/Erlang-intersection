% intersection.erl
-module(intersection).
-import(rand, []).
-import(timer, []).
-compile([export_all]).

% Used intersection layout / numeration:
%   1
% 4   2
%   3
% 1 -> car going "down"
% 2 -> car going "left"
% 3 -> car going "up"
% 4 -> car going "right"

car_generator(N) when N > 0 -> % N - Number of cars to generate
    Dir = rand:uniform(4), % randomly choose direction according to the layout
    send_car(Dir), % send a car to the intersection
    timer:sleep(200), % wait with further generation - 200 miliseconds
    car_generator(N-1); % generate rest of the N cars
car_generator(N) when N == 0 -> % N - Number of cars to generate
    send_car(terminate); % send termination signal to the intersection
car_generator(N) when N < 0 -> % N - Number of cars to generate
    Dir = rand:uniform(4), % randomly choose direction according to the layout
    send_car(Dir), % send a car to the intersection
    timer:sleep(200), % wait with further generation - 200 miliseconds
    car_generator(N). % generate cars forever - N never changes

send_car(Dir) when Dir == 4 ->
    io:format("Car going: right ~n");
send_car(Dir) when Dir == 3 ->
    io:format("Car going: up ~n");
send_car(Dir) when Dir == 2 ->
    io:format("Car going: left ~n");
send_car(Dir) when Dir == 1 ->
    io:format("Car going: down ~n");
send_car(Dir) ->
    io:format("Invalid direction ~p!  ~n", [Dir]).

traffic_light(Name, Light) when Light == red ->
    receive
        change ->
            io:format("~p is now ~p ~n", [Name, green]),
            traffic_light(Name, green)
    end;
traffic_light(Name, Light) ->
    receive
        change ->
            io:format("~p is now ~p ~n", [Name, red]),
            traffic_light(Name, red)
    end.

manager(Lights, Interval) ->
    change_interval(Lights, Interval).

% Helper functions to send a signal to change lights every interval
change_interval([H|T], Interval) ->
    timer:send_interval(Interval, H, change),
    change_interval(T, Interval);
change_interval([], _) -> ok.


% N - Number of cars to generate
% Interval - Interval of traffic lights change in ms
main(N, Interval) ->
    L1 = spawn(?MODULE, traffic_light, [l1, red]),
    L2 = spawn(?MODULE, traffic_light, [l2, green]),
    % L3 = spawn(?MODULE, traffic_light, [l3, red]),
    % L4 = spawn(?MODULE, traffic_light, [l4, green]),
    spawn(?MODULE, manager, [[L1, L2], Interval]).
