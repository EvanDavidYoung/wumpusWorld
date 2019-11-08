%my_agent.pl

%   this procedure requires the external definition of two procedures:
%
%     init_agent: called after new world is initialized.  should perform
%                 any needed agent initialization.
%
%     run_agent(percept,action): given the current percept, this procedure
%                 should return an appropriate action, which is then
%                 executed.
%
% This is what should be fleshed out

% ok
% safe 
:- load_files([utils]).  % Basic utilities
:- dynamic(peekForward/0).
:- dynamic(peekForward/1).
:- dynamic(currentPos/1).
:- dynamic(prevPos/1).
:- dynamic(orientation/1).
:- dynamic(wumpusAlive/0).
:- dynamic(hasArrow/1).
:- dynamic(visited/1).
:- dynamic(safeUnvisited/2).
:- dynamic(updateSafe/0).
:- dynamic(noPit/1).
:- dynamic(isNotPit/1).
:- dynamic(breeze/1).
:- dynamic(hasGold/0).
:- dynamic(updateOrientation/1).
:- dynamic(updateCoordinate/1).
:- dynamic(safe/1).
:- dynamic(pit/2).
:- dynamic(neighbors/2).
init_agent:-
  resetAgent(),
  assert(pit(_,_)),
  retract(pit(1,1)),
  assert(safe([1,1])),
  assert(wumpusAlive),
  assert(safe([1,2])),
  assert(safe([2,1])),
	assert(currentPos([1,1])),
  assert(visited([1,1])),
	assert(prevPos([1,1])),
	assert(orientation(0)),
  assert(path([goforward,turnleft])),
	format('\n=====================================================\n'),
	format('This is init_agent:\n\tIt gets called once, use it for your initialization\n\n'),
	format('=====================================================\n\n').
%------------------------------------------------------------------------
% execute(Action,Percept): executes Action and returns Percept
%
%   Action is one of:
%     goforward: move one square along current orientation if possible
%     turnleft:  turn left 90 degrees
%     turnright: turn right 90 degrees
%     grab:      pickup gold if in square
%     shoot:     shoot an arrow along orientation, killing wumpus if
%                in that direction
%     climb:     if in square 1,1, leaves the cave and adds 1000 points
%                for each piece of gold
%
%   Percept = [Stench,Breeze,Glitter,Bump,Scream]
%             The five parameters are either 'yes' or 'no'.


% [goforward,turnleft,turnright]
%  [Stench, Breeze, Glitter, Bump, Scream]
%  sassert 

next_action([H|T], H):- 
  retract(path([H|T])),
  assert(path(T)).
%run_agent(Percept,Action):-
% Percept : [_,_,_,_,_]

% if you are in (1,1) and have the gold:  
run_agent([no,no,no,no,no], climb):-
  currentPos([1,1]),
  hasGold,
  display_world,   
  updateSafe.

% if you bump into something: 
  % currentPos is an invalid spot 
  % previousPos is actual currentPosition 
run_agent([_,_,_,yes,_], turnright):-
  currentPos(C),
  prevPos(P),
  retract(safe(C)),
  updateOrientation(turnright),
  retract(safe(C)),
  retractall(currentPos(C)),
  assert(currentPos(P)),
  display_world.

% if the spot is safe: assert cells next to it are safe 
run_agent([no,no,no,no,no], goforward):-
  currentPos([X,Y]),
  display_world,   
  updateSafe,
  updateCoordinate(goforward).


% if the spot has a breeze: 
%   if you can move forward and you won't die, move forward. 
run_agent([no,yes,no,no,no], goforward):-
  currentPos([X,Y]),
  peekForward,
  display_world,   
  updateCoordinate(goforward).
% if the spot has a breeze: 
%   if you die if you move forward, turn. 
run_agent([no,yes,no,no,no], Action):-
  currentPos([X,Y]),
  \+peekForward,
  random_turn(Action),
  display_world,   
  updateOrientation(Action),
  updateCoordinate(Action).

% if the spot has a stench and wumpus is alive: 
%   if you can move forward and you won't die, move forward. 
run_agent([yes,_,no,_,_], Action):-
  wumpusAlive,
  currentPos([X,Y]),
  \+peekForward,
  random_move(Action),
  display_world,   
  updateOrientation(Action),
  updateCoordinate(Action).
% if the spot has a stench and wumpus is alive: 
%   if you can't move forward, turn.  
run_agent([yes,_,no,_,_], Action):-
  wumpusAlive,
  currentPos([X,Y]),
  peekForward,
  random_turn(Action),
  display_world,   
  updateOrientation(Action),
  updateCoordinate(Action).

% if there is gold, grab the gold. 
run_agent([_,_,yes,_,_], grab):-
  currentPos([X,Y]),
  display_world,   
  assertOnce(hasGold).



updateAgent(Percept,Action) :- 
  updateOrientation(NextAction),
  updateCoordinate(NextAction).






% if coordinate is not a pit: assert that it is not a pit 
isNotPit(P1) :- 
  visited(P1).
isNotPit(P1) :- 
  neighbors(P1,P2),
  \+breeze(P2),
  visited(P2).



peekForward:-
  orientation(O),
  O =:= 0, 
  currentPos([X,Y]),
  X1 is X+1,
  safe([X1,Y]).
peekForward:-
  orientation(O),
  O =:= 90 ,
  currentPos([X,Y]),
  Y1 is Y+1,
  safe([X,Y1]).
peekForward:-
  orientation(O),
  O =:= 180, 
  currentPos([X,Y]),
  Y1 is Y-1,
  safe([X,Y1]).
peekForward:-
  orientation(O),
  O =:= 270, 
  currentPos([X,Y]),
  X1 is X-1,
  safe([X1,Y]).
%% peekForward([X,Y],180):-
%%   safe([X-1,Y]).
%% peekForward([X,Y],270) :-
%%   safe([X,Y-1]).

updateOrientation(NextAction) :- 
  orientation(Orient),
  format("orientation is ~d \n", [Orient]),
  (NextAction = turnright -> NewOrientation is (Orient - 90) mod 360;
    (NextAction = turnleft -> NewOrientation is (Orient + 90) mod 360;
   NewOrientation = Orient)),
  format("NextAction is ~w \n", [NextAction]), 
  format("New orientation is ~d \n", [NewOrientation]),
  retractall(orientation(Orient)),
  assert(orientation(NewOrientation)).

neighbors([X,Y],[X1,Y]) :-
  X1 is X-1.
neighbors([X,Y],[X1,Y]) :-
  X1 is X+1.
neighbors([X,Y],[X,Y1]) :-
  Y1 is Y-1.
neighbors([X,Y],[X,Y1]) :-
  Y1 is Y+1.

updateSafe :- 
  currentPos([X,Y]),
  neighbors([X,Y],Z),
  assertOnce(safe(Z)).

updateCoordinate(NextAction) :- 
  orientation(Orient),
  currentPos([X,Y]),
  (NextAction = goforward -> updateCoordinateAux(Orient,NextAction); format("statement \n")),
  (retractall(prevPos(Z)); format("")),
  (assert(prevPos([X,Y])); format("")).

updateCoordinateAux(Orientation,NextAction) :- 
  Orientation =:= 0,
  currentPos([X,Y]),
  (NextAction = goforward -> NewX is X+1, NewY is Y; 
    NewX = X, NewY = Y),
  retractall(currentPos([X,Y])),
  assert(currentPos([NewX,NewY])),
  coordPrint(NewX,NewY,NextAction,0).
updateCoordinateAux(Orientation,NextAction) :- 
  Orientation =:= 90,
  currentPos([X,Y]),
  (NextAction = goforward -> NewX is X, NewY is Y+1; 
    NewX = X, NewY = Y),
  retractall(currentPos([X,Y])),
  assert(currentPos([NewX,NewY])),
  coordPrint(NewX,NewY,NextAction,90).
updateCoordinateAux(Orientation,NextAction) :- 
  Orientation =:= 180, 
  currentPos([X,Y]),
  (NextAction = goforward -> NewX is X-1, NewY is Y; 
    NewX = X, NewY = Y),
  retractall(currentPos([X,Y])),
  assert(currentPos([NewX,NewY])),
  coordPrint(NewX,NewY,NextAction,180).
updateCoordinateAux(Orientation,NextAction) :- 
  Orientation =:= 270,
  currentPos([X,Y]),
  (NextAction = goforward -> NewX is X, NewY is Y-1; 
    NewX = X, NewY = Y),
  retractall(currentPos([X,Y])),
  assert(currentPos([NewX,NewY])),
  coordPrint(NewX,NewY,NextAction,270).
coordPrint(X,Y,NextAction,Orientation) :- 
  format("Next Action is ~w \n", [NextAction]),
  format("Orientation is ~d \n", [Orientation]),
  format("New coordinate is (~d,~d)", [X,Y]).
resetAgent() :- 
  retractall(hasGold),
  retractall(currentPos([X,Y])),
  retractall(prevPos(A,B)),
  retractall(orientation(C)).

assertOnce(Fact):-
    \+( Fact ),!,         % \+ is a NOT operator.
    assert(Fact).
assertOnce(_).

random_turn(E) :- 
  random2(100,Value),
  (
    (E = turnleft , Value>50);
    (E = turnright , Value<50)
  ).
random_move(E) :- 
  random2(100,Value),
  (
    (E = goforward , Value>50);
    (E = turnleft , Value<50)
  ).

%% manhattan(X1,Y1,X2,Y2,Result) :- 
