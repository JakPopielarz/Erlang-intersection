# Intersection
A simple project written in Erlang as part of an assignment.

Calling it a simulation is an overstatement, but I'll do it anyway.
## Running
To run simpy compile the module and run it's `main` function. It accepts four arguments, but exists in various versions to make life easier:
* `main/4`: run the simulation and define all parameters by yourself (in order of passing to function):
    * `N` - Number of cars to generate
    * `Interval` - Interval of traffic lights change in ms
    * `Coeff` - coefficient of random car generation delay. Each car will be generated after *`random number between 1 and 5 (inclusive)`* * `Coeff`
    * `LightsCount` - number of working traffic lights (between 1 and 4, otherwise program won't execute properly)
* `main/3`: run the simulation with predetermined `Coeff = 150`
* `main/2`: run the simulation with predetermined `Coeff = 150, LightsCount = 4`
* `main/1`: run the simulation with predetermined `N = -1, Coeff = 150, LightsCount = 4`
* `main/0`: run the simulation with predetermined `N = -1, Interval = 2000, Coeff = 150, LightsCount = 4`

The arguments which are predetermined (and their values) have been chosen on "what's convenient for me" basis.

## What does it do?
Well, it "simulates" an intersection. After running the intersection will be printed in console every 50 ms. Basic print-out looks like this:
```
##| 0 |##   
##| r |##   
--+   +--   
x|x   g|7   
--+   +--   
##| r |##   
##| 1 |##
```
It's not much, but it's kind-of-simple to read. Every `r` or `g` symbolises a traffic light - for cars going from that direction. Every light is numbered internally, and used when specifying number of working lights:
```
  | 1 |
--+   +--
4       2
--+   +--
  | 3 |

1 -> car going "down"
2 -> car going "left"
3 -> car going "up"
4 -> car going "right"
```
So in the example print-out above we can see that:
* Lights 1 and 3 are red, 3 is green
* Light 4 is disabled (road closed symbolised by the `x`s)
* There are 7 cars behind light 2, 1 waiting behind light 3

And that's pretty much it. 
