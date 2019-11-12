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
:- dynamic(updateBump/0).
:- dynamic(peekForward/0).
:- dynamic(peekForward/1).
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
  assert(wumpusAlive),
  assert(safe([1,2])),
  assert(safe([2,1])),
  assert(hasArrow),
	assert(currentPos([1,1])),
  assert(visited([1,1])),
	assert(prevPos([1,1])),
	assert(orientation(0)),
  assert(path([goforward,turnleft,goforward,goforward,goforward,goforward,goforward,goforward,goforward,goforward,goforward,goforward,turnleft,goforward])),
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
  retractall(currentPos(_)), 
  prevPos(X),
  assert(currentPos(X)).

run_agent([_,_,yes,_,_], grab):-
  format("got the gold \n"),
  %% updateOrientation(Action),
  updateCoordinate(grab),
  assert(hasGold),
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
   NewOrientation = Orient)),
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
resetAgent() :- 
  retractall(hasGold),
  retractall(currentPos([X,Y])),
  retractall(prevPos(A,B)),
  retractall(orientation(C)).

assertOnce(Fact):-
    \+( Fact ),!,         % \+ is a NOT operator.
    assert(Fact).
assertOnce(_).

