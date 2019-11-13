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
:- dynamic fact/1.
:- dynamic(updateBump/0).
:- dynamic(peekForward/0).
:- dynamic(peekForward/1).
:- dynamic(xMax/1).
:- dynamic(yMax/1).
:- dynamic(currentPos/1).
:- dynamic(prevPos/1).
:- dynamic(orientation/1).
:- dynamic(wumpusAlive/0).
:- dynamic(hasArrow/0).
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
  assert(xMax(10000)),
  assert(yMax(10000)),
  assert(wumpusAlive),
  assert(safe([1,2])),
  assert(safe([2,1])),
  assert(hasArrow),
	assert(currentPos([1,1])),
  assert(visited([1,1])),
	assert(prevPos([1,1])),
	assert(orientation(0)),
  assert(path([turnleft,goforward,turnright])),
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
updateBump:- 
  orientation(O),
  retractall(currentPos(_)), 
  prevPos([X,Y]),
  (O = 90 -> format("O is 90 \n"), YMAX is Y+1, retractall(ymax(WYES)), asserta(ymax(YMAX)); format("O is not 90 \n")),
  (O = 0 -> format("O is 0 \n"), XMAX is X+1, retractall(xmax(EXES)), asserta(xmax(XMAX)); format("O is not 0 \n")),
  assert(currentPos([X,Y])).




% if you are in (1,1) and have the gold:  
run_agent([no,no,no,no,no], climb):-
  format("Get out of here \n"),
  currentPos([1,1]),
  hasGold,   
  ignore((updateSafe,false)),
  display_world,!.

run_agent([_,_,yes,_,_], grab):-
  format("got the gold \n"),
  %% updateOrientation(Action),
  updateCoordinate(grab),
  assert(hasGold),
  display_world,!.
% updat
run_agent([_,no,_,_,yes], goforward):-
  format("Wumpus is dead! \n"),
  retract(wumpusAlive),
  ignore((updateSafe,false)),
  updateOrientation(goforward),
  updateCoordinate(goforward),
  display_world,
  !.

% if the spot has a stench and wumpus is alive: 
  % take the shot  
run_agent([yes,_,_,_,_], shoot):-
  format("take shot at wumpus if alive \n"),
  wumpusAlive,
  hasArrow,
  updateOrientation(shoot),
  updateCoordinate(shoot),
  retract(hasArrow),
  display_world,!.
run_agent([no,no,no,no,no], Action):-
  %% format("Safe has been updated \n"),
  ignore((updateSafe,false)),
  next_action(Actions,Action),
  updateOrientation(Action),
  updateCoordinate(Action),
  display_world,!.

run_agent([_,_,_,yes,_], Action):-
  format("bumped into wall \n"),
  updateBump,
  next_action(Actions,Action),
  updateOrientation(Action),
  updateCoordinate(Action),
  display_world.
run_agent([_,_,_,no,_], Action):-
  display_world,
  next_action(Actions,Action),
  updateOrientation(Action),
  updateCoordinate(Action).


updateOrientation(NextAction) :- 
  orientation(Orient),
  format("orientation is ~d \n", [Orient]),
  (NextAction = turnright -> NewOrientation is (Orient - 90) mod 360;
    (NextAction = turnleft -> NewOrientation is (Orient + 90) mod 360;
   NewOrientation is Orient)),
  format("NextAction is ~w \n", [NextAction]), 
  format("New orientation is ~d \n", [NewOrientation]),
  retractall(orientation(Orient)),
  assert(orientation(NewOrientation)).

updateCoordinate(NextAction) :- 
  orientation(Orient),
  currentPos([X,Y]),
  updateCoordinateAux(X,Y,NewX,NewY,NextAction),
  retractall(currentPos(P)),
  retractall(prevPos(P)),
  assert(prevPos([X,Y])),
  assert(currentPos([NewX,NewY])),
  format("UpdatedCoordinate is [~d,~d]\n",[NewX,NewY]).

updateCoordinateAux(X,Y,NewX,NewY,NextAction) :- 
  NextAction \= goforward,
  NewX is X, 
  NewY is Y,!.
updateCoordinateAux(X,Y,NewX,NewY,NextAction) :- 
  orientation(O),
  O =:= 0,
  NextAction = goforward,
  NewX is X + 1,
  NewY is Y,!.
updateCoordinateAux(X,Y,NewX,NewY,NextAction) :- 
  orientation(O),
  O =:= 90,
  NextAction = goforward,
  NewX is X,
  NewY is Y + 1,!.
updateCoordinateAux(X,Y,NewX,NewY,NextAction) :- 
  orientation(O),
  O =:= 180,
  NextAction = goforward,
  NewX is X - 1,!,
  NewY is Y.
updateCoordinateAux(X,Y,NewX,NewY,NextAction) :- 
  orientation(O),
  O =:= 270,
  NextAction = goforward,
  NewX is X,
  NewY is Y - 1,!.

peekForward:-
  orientation(O),
  xMax(XMAX),
  yMax(YMAX),
  O =:= 0, 
  currentPos([X,Y]),
  X1 is X+1,
  X+1 > 0, 
  X+1 < XMAX,
  Y+1 > 0, 
  Y+1 < YMAX,
  safe([X1,Y]).
peekForward:-
  orientation(O),
  xMax(XMAX),
  yMax(YMAX),
  O =:= 90 ,
  currentPos([X,Y]),
  Y1 is Y+1,
  X+1 > 0, 
  X+1 < XMAX,
  Y+1 > 0, 
  Y+1 < YMAX,
  safe([X,Y1]).
peekForward:-
  orientation(O),
  xMax(XMAX),
  yMax(YMAX),
  O =:= 180, 
  currentPos([X,Y]),
  Y1 is Y-1,
  X+1 > 0, 
  X+1 < XMAX,
  Y+1 > 0, 
  Y+1 < YMAX,
  safe([X,Y1]).
peekForward:-
  orientation(O),
  xMax(XMAX),
  yMax(YMAX),
  O =:= 270, 
  currentPos([X,Y]),
  X1 is X-1,
  X+1 > 0, 
  X+1 < XMAX,
  Y+1 > 0, 
  Y+1 < YMAX,
  safe([X1,Y]).

random_turn(E) :- 
  random2(100,Value),
  (
    (E = turnleft , Value>50);
    (E = turnright , Value<50)
  ),!.
random_move(E) :- 
  random2(100,Value),
  (
    (E = goforward , Value>50);
    (E = turnleft , Value<50)
  ),!.

neighbors([X,Y],[X1,Y]) :-
  X1 is X-1.
neighbors([X,Y],[X1,Y]) :-
  X1 is X+1.
neighbors([X,Y],[X,Y1]) :-
  Y1 is Y-1.
neighbors([X,Y],[X,Y1]) :-
  Y1 is Y+1.

updateSafe :- 
  format("Update Safe \n"),
  currentPos([X,Y]),
  neighbors([X,Y],[X1,Y1]),
  format("Asserting [~d,~d] \n",[X1,Y1]),
  assertOnce(safe([X1,Y1])).

assertOnce(Fact):-
    \+( Fact ),!,         % \+ is a NOT operator.
    assert(Fact).
assertOnce(_).

resetAgent() :- 
  retractall(hasGold),
  retractall(currentPos([X,Y])),
  retractall(prevPos(A,B)),
  retractall(orientation(C)).
