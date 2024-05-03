% Define the colors
color(red).
color(yellow).
color(blue).

adjacent((X1,Y1), (X2,Y2)) :-
    integer(X1), integer(Y1), integer(X2), integer(Y2), % Check if both coordinates are instantiated
    abs(X1 - X2) + abs(Y1 - Y2) =:= 1.

cell_color(Grid, (X,Y), Color) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, Color).

valid_cell(Grid, (X,Y)) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, _),
    length(Grid, N),
    length(Row, M),
    X >= 0, X < N,
    Y >= 0, Y < M.


dfs_cycle(Grid, Start, Current, Visited, Cycle) :-
    adjacent(Current, Start),     % Found cycle, return
    member(Start, Visited),
    Cycle = [Start|Visited].
dfs_cycle(Grid, Start, Current, Visited, Cycle) :-
    valid_cell(Grid, Current),
    \+ member(Current, Visited), % Ensure we haven't visited this cell yet
    cell_color(Grid, Start, Color),
    cell_color(Grid, Current, Color),
    (   length(Visited, Len),
        Len >= 3 % Check if we have visited at least 3 cells (minimum to form a cycle)
    ->  Cycle = [Current|Visited] % If yes, form a cycle
    ;   dfs_cycle(Grid, Start, Next, [Current|Visited], Cycle), % Otherwise, continue DFS
        adjacent(Current, Next)
    ).

% Entry point to find color cycles
find_color_cycle(Grid, Cycle) :-
    input_grid(Grid), % Directly incorporate the input grid
    valid_cell(Grid, Start),
    cell_color(Grid, Start, Color),
    dfs_cycle(Grid, Start, Start, [], Cycle),
    length(Cycle, Len),
    Len >= 4.

% Sample Input
% Replace this with your actual input grid
input_grid([
    [blue, red, blue],
    [yellow, yellow, yellow],
    [blue, yellow, yellow]
]).


%example input - input_grid(Grid), find_color_cycle(Grid, Cycle), write(Cycle).





















