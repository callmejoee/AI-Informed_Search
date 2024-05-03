% Cell definitions (each cell has a color)
cell(0, 0, red).
cell(0, 1, red).
cell(1, 0, red).
cell(1, 1, blue).
cell(2, 0, red).
cell(2, 1, red).
cell(2, 2, red).
cell(2, 3, yellow).
cell(0, 2, red).
cell(1, 2, red).
cell(1, 3, red).
cell(3, 0, blue).
cell(0, 3, blue).
cell(3, 1, yellow).
cell(3, 2, blue).
cell(3, 3, yellow).

% Start and goal definitions
start(0, 0).
goal(1, 3).

% Heuristic function using Manhattan distance
calculateH((X, Y), (GoalX, GoalY), H) :-
    DX is abs(X - GoalX),
    DY is abs(Y - GoalY),
    H is DX + DY.

% A* Search Entry
search :-
    start(SX, SY),
    goal(GX, GY),
    calculateH((SX, SY), (GX, GY), H),
    search([[(SX, SY, [(SX, SY)]), null, 0, H, H]], [], (GX, GY)).

% A* search processing
search(Open, _, Goal) :-
    getBestState(Open, [(X, Y, Path), _, G, _, _], _),
    (X, Y) = Goal,
    reverse(Path, RevPath),
    write('Search is complete! Path found: '), write(RevPath), nl, !.

search(Open, Closed, Goal) :-
    getBestState(Open, CurrentNode, RestOpen),
    CurrentNode = [(X, Y, Path), _, G, _, _],
    findall(Child,
            getNextState(CurrentNode, Goal, Child),
            Children),
    addChildren(Children, RestOpen, NewOpen),
    append(Closed, [CurrentNode], NewClosed),
    search(NewOpen, NewClosed, Goal).

% Generate the next state based on valid moves
getNextState([(X, Y, Path), _, G, _, _], Goal, [(NextX, NextY, NewPath), (X, Y), NewG, NewH, NewF]) :-
    move(X, Y, NextX, NextY),
    \+ member((NextX, NextY), Path),  % This checks for cycles in the path
    calculateH((NextX, NextY), Goal, NewH),
    NewG is G + 1,
    NewF is NewG + NewH,
    append(Path, [(NextX, NextY)], NewPath).


% Add children to open list
addChildren(Children, Open, NewOpen) :-
    append(Open, Children, NewOpen).

% Get the state with the minimum F value
getBestState(Open, BestChild, Rest) :-
    findMin(Open, BestChild),
    delete(Open, BestChild, Rest).

% Find the minimum based on F value
findMin([X], X) :- !.
findMin([Head|Tail], Min) :-
    findMin(Tail, TailMin),
    Head = [_,_,_,_,HeadF],
    TailMin = [_,_,_,_,TailF],
    (   TailF < HeadF -> Min = TailMin ; Min = Head ).

move(X, Y, NewX, Y) :-
    NewX is X + 1, NewX >= 0, cell(X, Y, Color), cell(NewX, Y, Color).
move(X, Y, NewX, Y) :-
    NewX is X - 1, NewX >= 0, cell(X, Y, Color), cell(NewX, Y, Color).
move(X, Y, X, NewY) :-
    NewY is Y + 1, NewY >= 0, cell(X, Y, Color), cell(X, NewY, Color).
move(X, Y, X, NewY) :-
    NewY is Y - 1, NewY >= 0, cell(X, Y, Color), cell(X, NewY, Color).
