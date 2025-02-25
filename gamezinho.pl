:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).


% Define the opposing players: Black vs White
opposite(black, white).
opposite(white, black).


% Main predicate to start the game. Displays the menu and processes user input
play :- 
    display_menu,
    read(MenuOption),
    process_menu_option(MenuOption).

% Displays the main game menu with options for different game modes
display_menu :-
    nl,
    write('+---------------------------------------+'), nl,
    write('|          Welcome to Minefield!        |'), nl,
    write('|---------------------------------------|'), nl,
    write('| 1. Human vs Human                     |'), nl,
    write('| 2. Human vs PC                        |'), nl,
    write('| 3. PC vs Human                        |'), nl,
    write('| 4. PC vs PC                           |'), nl,
    write('| 5. Quit                               |'), nl,
    write('+---------------------------------------+'), nl,
    write('Select an option (1-5): ').

% Processes the user's menu selection and initializes the game based on the choice
process_menu_option(1) :- 
    set_board_size(Size),
    initial_state(Size, hh, GameState),
    game_cycle(GameState).
process_menu_option(2) :- 
    set_board_size(Size),
    choose_difficulty(Difficulty),
    initial_state(Size, hp(Difficulty), GameState),
    game_cycle(GameState).
process_menu_option(3) :- 
    set_board_size(Size),
    choose_difficulty(Difficulty),
    initial_state(Size, ph(Difficulty), GameState),
    game_cycle(GameState).
process_menu_option(4) :- 
    set_board_size(Size),
    write('Choose difficulty for Player 1 (Black):'), nl,
    choose_difficulty(Difficulty1),
    write('Choose difficulty for Player 2 (White):'), nl,
    choose_difficulty(Difficulty2),
    initial_state(Size, pp(Difficulty1, Difficulty2), GameState),
    game_cycle(GameState).
process_menu_option(5) :- 
    write('Goodbye!'), nl.
process_menu_option(_) :- 
    write('Invalid option. Please try agaain.'), nl,
    play.

% Prompts the user to input the desired board size, ensuring a minimum size of 3
set_board_size(Size) :-
    nl,
    write('+---------------------------------------+'), nl,
    write('Enter the desired board size (minimum 3 and maximum 15): '),
    read(InputSize),
    write('+---------------------------------------+'), nl,
    validate_board_size(InputSize, Size).

% Validates the board size input
validate_board_size(InputSize, Size) :-
    integer(InputSize),
    InputSize >= 3,
    InputSize =< 15,
    Size = InputSize.

% Validates the input of the user, and manages the cases where the input is incorrect
validate_board_size(_, Size) :-
    write('Invalid size. Please enter an integer >= 3 and =<15.'), nl,
    set_board_size(Size).


% Initializes the game state by creating an empty board of the specified size.
% Inputs:
%   - Size: The size of the board (an integer indicating the number of rows and columns in the board).
%   - GameConfig: The configuration parameters for the game (not used in this predicate, but passed along to the game state).
%   - game_state(Board, black, GameConfig, Size): The resulting game state with the initialized board and the starting player (black).
% ---------------------------------------------------------------------------
initial_state(Size, GameConfig, game_state(Board, black, GameConfig, Size)) :- 
    % Create the board by generating a list of lists, where each list represents a row of the board.
    % The number of rows is equal to 'Size'.
    length(Board, Size),  % Length of the 'Board' is set to 'Size'. 'Board' is a list of 'Size' number of rows.
    
    % Initialize each row of the board to be a list of 'Size' empty cells.
    % 'init_row/2' is a predicate that fills each row with 'empty' values.
    % The 'Board' is a list of 'Size' rows, and each row is initialized with the 'init_row/2' predicate.
    maplist(init_row(Size), Board).  % For each row in the board, call 'init_row/2' to initialize the row to 'empty'.


% Initializes a single row of the board with all cells set to 'empty'
init_row(Size, Row) :- 
    length(Row, Size),
    maplist(=(empty), Row).

% Main game cycle that alternates turns until the game ends
game_cycle(GameState) :- 
    display_game(GameState),
    (   game_over(GameState, Result),
        (   Result = draw,
            write('The game is a draw!'), nl
        ;   announce_winner(Result)  % Announce the winner
        )
    ;   play_turn(GameState, NextGameState),  % Play the next turn
        game_cycle(NextGameState)  % Continue the game
    ).


% Prompts the user to choose the difficulty level
choose_difficulty(Difficulty) :-
    write('+---------------------------------------+'), nl,
    write('Choose difficulty: 1. Easy  2. Hard'), nl,
    write('+---------------------------------------+'), nl,
    read(DifficultyOption),
    validate_difficulty(DifficultyOption, Difficulty).

% Validates the user's difficulty choice
validate_difficulty(1, easy).  % Easy difficulty
validate_difficulty(2, hard).  % Hard difficulty

validate_difficulty(_, Difficulty) :-
    write('Invalid option. Please try again.'), nl,
    choose_difficulty(Difficulty).  % Recursively prompt the user again


% Executes a turn for the current player in Human vs Human configuration
play_turn(game_state(Board, Player, hh, Size), game_state(NewBoard, NextPlayer, hh, Size)) :-
    human_move(game_state(Board, Player, hh, Size), game_state(NewBoard, NextPlayer, hh, Size)).

% Executes a turn for the current player in Human vs PC configuration when the player is black
play_turn(game_state(Board, black, hp(Difficulty), Size), game_state(NewBoard, NextPlayer, hp(Difficulty), Size)) :-
    human_move(game_state(Board, black, hp(Difficulty), Size), game_state(NewBoard, NextPlayer, hp(Difficulty), Size)).

% Executes a turn for the current player in Human vs PC configuration when the player is white
play_turn(game_state(Board, white, hp(Difficulty), Size), game_state(NewBoard, NextPlayer, hp(Difficulty), Size)) :-
    computer_move(Difficulty, game_state(Board, white, hp(Difficulty), Size), game_state(NewBoard, NextPlayer, hp(Difficulty), Size)).

% Executes a turn for the current player in PC vs Human configuration when the player is black
play_turn(game_state(Board, black, ph(Difficulty), Size), game_state(NewBoard, NextPlayer, ph(Difficulty), Size)) :-
    computer_move(Difficulty, game_state(Board, black, ph(Difficulty), Size), game_state(NewBoard, NextPlayer, ph(Difficulty), Size)).

% Executes a turn for the current player in PC vs Human configuration when the player is white
play_turn(game_state(Board, white, ph(Difficulty), Size), game_state(NewBoard, NextPlayer, ph(Difficulty), Size)) :-
    human_move(game_state(Board, white, ph(Difficulty), Size), game_state(NewBoard, NextPlayer, ph(Difficulty), Size)).

% Executes a turn for the current player in PC vs PC configuration when the player is black
play_turn(game_state(Board, black, pp(Difficulty1, Difficulty2), Size), game_state(NewBoard, NextPlayer, pp(Difficulty1, Difficulty2), Size)) :-
    computer_move(Difficulty1, game_state(Board, black, pp(Difficulty1, Difficulty2), Size), game_state(NewBoard, NextPlayer, pp(Difficulty1, Difficulty2), Size)).

% Executes a turn for the current player in PC vs PC configuration when the player is white
play_turn(game_state(Board, white, pp(Difficulty1, Difficulty2), Size), game_state(NewBoard, NextPlayer, pp(Difficulty1, Difficulty2), Size)) :-
    computer_move(Difficulty2, game_state(Board, white, pp(Difficulty1, Difficulty2), Size), game_state(NewBoard, NextPlayer, pp(Difficulty1, Difficulty2), Size)).




% Evaluates the current game state and computes its value for the given player.
% The value is determined by the difference between the number of connected paths
% (potential winning paths) for the current player and their opponent.
% 
% Inputs:
%   - game_state(Board, Player, _, Size): 
%       - Board: The current state of the game board (a 2D list).
%       - Player: The player for whom the game state is being evaluated ('black' or 'white').
%       - _: Unused configuration parameter.
%       - Size: The size of the game board (e.g., 5 for a 5x5 board).
%   - Player: The player whose perspective is being used for evaluation ('black' or 'white').
% Outputs:
%   - Value: A numeric value representing the favorability of the game state for the player.
%       - Positive values indicate an advantage for the player.
%       - Negative values indicate an advantage for the opponent.
%       - Zero indicates a neutral or balanced state.
% ---------------------------------------------------------------------------
value(game_state(Board, Player, _, Size), Player, Value) :-
    % Calculate the number of connected paths for the given player
    count_connections(Board, Player, Size, PlayerConnections),
    
    % Determine the opponent and calculate their connected paths
    switch_player(Player, Opponent),
    count_connections(Board, Opponent, Size, OpponentConnections),
    
    % Compute the value as the difference between the player's and opponent's connections
    Value is PlayerConnections - OpponentConnections.


% Counts the number of connected paths on the board for a given player.
% A connected path is defined as a sequence of cells occupied by the player
% that connect one edge of the board to the opposite edge (e.g., top-to-bottom for 'black').
% 
% Inputs:
%   - Board: The current state of the game board (a 2D list where each cell contains 
%            either 'empty', 'black', or 'white').
%   - Player: The player whose connected paths are being counted ('black' or 'white').
%   - Size: The size of the board (e.g., 5 for a 5x5 board).
% Outputs:
%   - Count: The total number of connected paths found for the given player.
% 
% Process:
%   - Uses the `connected_path/5` predicate to find all paths for the player 
%     that connect the appropriate edges of the board.
%   - Aggregates all such paths into a list and calculates its length to determine the count.
% ---------------------------------------------------------------------------
count_connections(Board, Player, Size, Count) :-
    % Find all connected paths for the given player
    findall((Start, End), 
            connected_path(Board, Player, Start, End, Size), 
            Paths),
    
    % Count the total number of paths by determining the length of the list
    length(Paths, Count).


% Displays the current game state, including the board and the current player.
% Inputs:
%   - game_state(Board, Player, _, Size): The current game state containing the game board, 
%     the current player ('black' or 'white'), and the board size ('Size').
% ---------------------------------------------------------------------------
display_game(game_state(Board, Player, _, Size)) :- 
    nl,  % Start with a new line for clean separation before displaying the game state.
    % Display the top border of the game state display (a simple line for separation).
    write('+---------------------------------------+'), nl,
    % Display the board size label and the actual size of the board.
    write('| Board Size: '), 
    write(Size),  
    write('                         |'), nl, 
    % Display the current player label and the actual current player.
    write('| Current Player: '), 
    write(Player), 
    write('                 |'), nl,      
    nl,  
    % Calculate and display the value of the current game state.
    value(game_state(Board, Player, _, Size), Player, Value),
    write('| Game Value: '), 
    write(Value), 
    write('                         |'), nl, 
    
    nl,  
    % Call the predicate to display the actual game board. This will print the board layout.
    display_board(Board), nl,  
    % Display the bottom border of the game state display (closing the section).
    write('+---------------------------------------+'), nl. 

% Helper predicate to display the board by iterating over its rows.
% Inputs:
%   - Board: The board to display, a list of lists representing rows.
% ---------------------------------------------------------------------------
display_board(Board) :- 
    length(Board, Size),  % Get the size of the board (e.g., 5 for a 5x5 board).    
    % Display the column numbers on top of the board to label the columns.
    display_column_numbers(Size),    
    % Reverse the order of the rows so that the bottom row is printed first.
    reverse(Board, ReversedBoard),    
    display_rows(ReversedBoard, Size, 1).  % Iterate through the reversed board rows and display them with appropriate row numbers starting from 1.


% Display the column numbers above the board
display_column_numbers(Size) :-
    write('    '),  % Padding for alignment
    print_column_numbers(1, Size),  % Print the column numbers
    nl.

% Print the column numbers from 1 to Size
print_column_numbers(Current, Size) :-
    Current =< Size,
    write(Current), write(' '),
    Next is Current + 1,
    print_column_numbers(Next, Size).
print_column_numbers(_, _).

% Displays each row of the board starting from the bottom row with row numbers
display_rows([], _, _).  % Base case when there are no more rows
display_rows([Row | Rest], Size, RowNum) :-
    % Display the row number (starting from the bottom)
    RowIndex is Size - RowNum + 1,
    write(RowIndex), write(' | '),  % Row number and separator
    display_row(Row),  % Display the row
    nl,
    NextRowNum is RowNum + 1,  % Move to the next row
    display_rows(Rest, Size, NextRowNum).

% Displays the content of a single row (unchanged)
display_row([]).
display_row([Cell | Rest]) :- 
    display_cell(Cell),
    display_row(Rest).

% Displays a single cell
display_cell(empty) :- write('. ').
display_cell(black) :- write('B ').
display_cell(white) :- write('W ').


% Defines a move and applies it to the game state if it is valid.
% Inputs:
%   - game_state(Board, Player, Config, Size): Current game state containing the board, the current player, configuration, and size.
%   - (Row, Col): The desired move location, represented as a tuple (row, column).
%   - game_state(NewBoard, NextPlayer, Config, Size): The resulting game state after the move, with the updated board and the next player.
% ---------------------------------------------------------------------------
move(game_state(Board, Player, Config, Size), (Row, Col), game_state(NewBoard, NextPlayer, Config, Size)) :- 
    valid_moves(game_state(Board, Player, Config, Size), Moves), 
    member((Row, Col), Moves),
    make_move(Board, Row, Col, Player, NewBoard),  % Modify the board based on the move
    switch_player(Player, NextPlayer).  % Determine who the next player will be (switch between black and white)


% Checks if the board is full: no valid moves or all cells are filled
board_full(game_state(Board, Player, GameConfig, Size)) :-
    valid_moves(game_state(Board, Player, GameConfig, Size), Moves),
    (length(Moves, 0); % No valid moves left
     (append(Board, FlatBoard), 
      \+ member(empty, FlatBoard))). % No empty cells remain

% Checks if the game has ended and determines the result (winner or draw).
% Inputs:
%   - game_state(Board, _, _, Size): The current game state containing the board, the current player, and the size of the board.
%   - Result: The result of the game, which could be either 'black', 'white', or 'draw'.
% ---------------------------------------------------------------------------
game_over(game_state(Board, _, _, Size), black) :-
    connected_path(Board, black, (1, _), (Size, _), Size).

game_over(game_state(Board, _, _, Size), white) :-
    connected_path(Board, white, (_, 1), (_, Size), Size).

game_over(game_state(Board, _, _, Size), draw) :-
    board_full(game_state(Board, _, _, Size)).

% Alternates the current player after each turn
switch_player(black, white).
switch_player(white, black).


% This predicate checks whether placing a move at a given position on the board 
% would create a "hard corner" pattern, which is a strategic disadvantage in the game. 
% It temporarily places the player's move, validates the adjacent positions, 
% and ensures that no hard corner patterns are created. The move is reverted after the check.
% Inputs:
%   - Board: Current game board.
%   - Row, Col: Position of the move being considered.
%   - Player: Current player making the move.
%   - Size: Size of the game board.
% ---------------------------------------------------------------------------
creates_hard_corner(Board, Row, Col, Player, Size) :-
    % Temporarily set the position to the current player's move using make_move
    make_move(Board, Row, Col, Player, TempBoard),
    % Get the opponent
    opposite(Player, Opponent),
    % Get adjacent positions
    adjacent_positions((Row, Col), Size, AdjacentPositions),
    % Generate the 4 2x2 areas by combining (Row, Col) with each adjacent position
    findall((R1, C1, R2, C2, R3, C3, R4, C4), 
        (
            % Check all four 2x2 blocks
            valid_2x2_block(R1, C1, R2, C2, R3, C3, R4, C4, Row, Col)
        ),
        Areas),
    % Check each of the 2x2 areas for had corner patterns
    (   check_hard_corner_pattern(TempBoard, Areas, Player, Opponent)
    ;
        % Revert the position after the check using make_move
        make_move(TempBoard, Row, Col, empty, TempBoard)
    ).

% This predicate generates the coordinates of all valid 2x2 blocks that can be formed 
% around a given position. These blocks are used to check for patterns like "hard corners".
% Inputs:
%   - R1, C1, ..., R4, C4: Coordinates of the 2x2 block.
%   - Row, Col: Position being checked.
% ---------------------------------------------------------------------------
valid_2x2_block(R1, C1, R2, C2, R3, C3, R4, C4, Row, Col) :-
    (   % Top-left block
        R1 is Row - 1, C1 is Col - 1,
        R2 is Row - 1, C2 is Col,
        R3 is Row, C3 is Col - 1,
        R4 is Row, C4 is Col
    ;
        % Top-right block
        R1 is Row - 1, C1 is Col,
        R2 is Row - 1, C2 is Col + 1,
        R3 is Row, C3 is Col,
        R4 is Row, C4 is Col + 1
    ;
        % Bottom-left block
        R1 is Row, C1 is Col - 1,
        R2 is Row, C2 is Col,
        R3 is Row + 1, C3 is Col -1,
        R4 is Row + 1, C4 is Col
    ;
        % Bottom-right block
        R1 is Row, C1 is Col,
        R2 is Row, C2 is Col+1,
        R3 is Row +1, C3 is Col,
        R4 is Row + 1, C4 is Col + 1
    ).

% Updates the value at a specified position on the game board.
% Inputs:
%   - Board: The original game board.
%   - (Row, Col): Position to be updated.
%   - Value: New value to set.
%   - NewBoard: Updated game board.
% ---------------------------------------------------------------------------
set_position(Board, (Row, Col), Value, NewBoard) :-
    nth_position(Board, (Row, Col), _), % Check if the position exists
    replace(Board, (Row, Col), Value, NewBoard).  % Replace the position with Value

% Checks whether any of the specified 2x2 areas contain a "hard corner" pattern 
% based on the positions of the player's and opponent's pieces.
% Inputs:
%   - Board: Current game board.
%   - Areas: List of 2x2 areas to check.
%   - Player: Current player.
%   - Opponent: Opponent player.
% ---------------------------------------------------------------------------
check_hard_corner_pattern(Board, Areas, Player, Opponent) :-
    member((R1, C1, R2, C2, R3, C3, R4, C4), Areas),
    (
        % Pattern 1: Player occupies (R1, C1) and (R3, C3), Opponent occupies (R2, C2), Empty at (R4, C4)
        nth_position(Board, (R1, C1), Player),
        nth_position(Board, (R4, C4), Player),
        nth_position(Board, (R2, C2), Opponent),
        nth_position(Board, (R3, C3), empty)
    ;
        % Pattern 2: Player occupies (R1, C1) and (R3, C3), Opponent occupies (R4, C4), Empty at (R2, C2)
        nth_position(Board, (R1, C1), Player),
        nth_position(Board, (R4, C4), Player),
        nth_position(Board, (R2, C2), empty),
        nth_position(Board, (R3, C3), Opponent)
    ;
        % Pattern 3: Opponent occupies (R1, C1) and (R3, C3), Player occupies (R2, C2), Empty at (R4, C4)
        nth_position(Board, (R1, C1), Opponent),
        nth_position(Board, (R4, C4), Opponent),
        nth_position(Board, (R2, C2), Player),
        nth_position(Board, (R3, C3), empty)
    ;
        % Pattern 4: Opponent occupies (R1, C1) and (R3, C3), Player occupies (R4, C4), Empty at (R2, C2)
        nth_position(Board, (R1, C1), Opponent),
        nth_position(Board, (R4, C4), Opponent),
        nth_position(Board, (R4, C4), Player),
        nth_position(Board, (R3, C3), empty)
    ;
        % Pattern 5: Player occupies (R2, C2) and (R4, C4), Opponent occupies (R1, C1), Empty at (R3, C3)
        nth_position(Board, (R2, C2), Player),
        nth_position(Board, (R3, C3), Player),
        nth_position(Board, (R1, C1), Opponent),
        nth_position(Board, (R4, C4), empty)
    ;
        % Pattern 6: Player occupies (R2, C2) and (R4, C4), Opponent occupies (R3, C3), Empty at (R1, C1)
        nth_position(Board, (R2, C2), Player),
        nth_position(Board, (R3, C3), Player),
        nth_position(Board, (R4, C4), Opponent),
        nth_position(Board, (R1, C1), empty)
    ;
        % Pattern 7: Opponent occupies (R1, C1) and (R3, C3), Player occupies (R2, C2), Empty at (R4, C4)
        nth_position(Board, (R2, C2), Opponent),
        nth_position(Board, (R1, C1), Player),
        nth_position(Board, (R3, C3), Opponent),
        nth_position(Board, (R4, C4), empty)
    ;
        % Pattern 8: Opponent occupies (R2, C2) and (R4, C4), Player occupies (R1, C1), Empty at (R3, C3)
        nth_position(Board, (R2, C2), Opponent),
        nth_position(Board, (R3, C3), Opponent),
        nth_position(Board, (R4, C4), Player),
        nth_position(Board, (R1, C1), empty)
    ).

% Generates a list of valid moves for the current player, ensuring that 
% no "hard corners" or switches (patterns leading to disadvantage) are created.
% Inputs:
%   - game_state(Board, Player, _, Size): Current game state.
%   - Moves: List of valid moves (Row, Col).
% ---------------------------------------------------------------------------
valid_moves(game_state(Board, Player, _, Size), Moves) :- 
    findall((Row, Col),
        (between(1, Size, Row),
         between(1, Size, Col),
         nth1(Row, Board, RowList),
         nth1(Col, RowList, empty),
         \+ creates_hard_corner(Board, Row, Col, Player, Size),
         \+ creates_switches_2_x_3(Board, Row, Col, Player),
         \+ creates_switches_2_x_4(Board, Row, Col, Player)), 
        Moves).


% Checks if placing a move at a given position creates a "switch" pattern 
% in any 2x3 areas around the move. These patterns can result in a strategic disadvantage.
% Inputs:
%   - Board: Current game board.
%   - Row, Col: Position of the move being checked.
%   - Player: Current player.
% ---------------------------------------------------------------------------
creates_switches_2_x_3(Board, Row, Col, Player) :-
    % Make the temporary move
    make_move(Board, Row, Col, Player, TempBoard),    
    % Get the opposite player
    opposite(Player, Opponent),    
    % Check for switches
    find_switch(TempBoard, Row, Col, Player, Opponent),
    % Revert the move if a switch is created
    make_move(TempBoard, Row, Col, empty, _).

% Finds all 2x3 blocks around the given position and checks if any of them 
% form a "switch" pattern with the current player's move.
% Inputs:
%   - Board: Current game board.
%   - Row, Col: Position of the move being checked.
%   - Player: Current player.
%   - Opponent: Opponent player.
% ---------------------------------------------------------------------------
find_switch(Board, Row, Col, Player, Opponent) :-
    % Find all valid 2x3 blocks around the move
    findall((R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6), 
        (
            valid_2x3_block(R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, Row, Col)
        ),
        Areas),
    % Check if any of the blocks form a switch pattern
    member((R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6), Areas),
    check_switch_pattern(Board, R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, Player, Opponent).

% Generates the coordinates of all valid 2x3 blocks around a given position.
% These blocks are used to check for switch patterns.
% Inputs:
%   - R1, C1, ..., R6, C6: Coordinates of the 2x3 block.
%   - Row, Col: Position being checked.
% ---------------------------------------------------------------------------
valid_2x3_block(R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, Row, Col) :-
    (  
        R1 is Row - 2, C1 is Col - 1, R2 is Row - 2, C2 is Col, R3 is Row - 1, C3 is Col -1,
        R4 is Row -1, C4 is Col, R5 is Row, C5 is Col-1, R6 is Row, C6 is Col
    ;
        R1 is Row - 2, C1 is Col, R2 is Row - 2, C2 is Col + 1, R3 is Row - 1, C3 is Col,
        R4 is Row -1, C4 is Col+1, R5 is Row, C5 is Col , R6 is Row, C6 is Col + 1
    ;
        R1 is Row - 1, C1 is Col+2, R2 is Row, C2 is Col+2, R3 is Row - 1, C3 is Col+1,
        R4 is Row, C4 is Col + 1, R5 is Row -1, C5 is Col, R6 is Row, C6 is Col
    ;
        R1 is Row, C1 is Col+2, R2 is Row + 1, C2 is Col+2, R3 is Row, C3 is Col +1,
        R4 is Row +1, C4 is Col + 1, R5 is Row, C5 is Col, R6 is Row + 1, C6 is Col + 1
    ;
        R1 is Row+2, C1 is Col+1, R2 is Row +2, C2 is Col, R3 is Row+1, C3 is Col +1,
        R4 is Row + 1, C4 is Col, R5 is Row + 1, C5 is Col + 1, R6 is Row, C6 is Col
    ;
        R1 is Row+2, C1 is Col, R2 is Row+2, C2 is Col-1, R3 is Row+1, C3 is Col,
        R4 is Row + 1, C4 is Col - 1, R5 is Row, C5 is Col, R6 is Row, C6 is Col - 1
    ;
        R1 is Row - 1, C1 is Col - 2, R2 is Row, C2 is Col-2, R3 is Row -1, C3 is Col - 1,
        R4 is Row, C4 is Col-1, R5 is Row-1, C5 is Col, R6 is Row, C6 is Col
    ;
        R1 is Row, C1 is Col - 2, R2 is Row + 1, C2 is Col - 2, R3 is Row, C3 is Col - 1,
        R4 is Row +1, C4 is Col-1, R5 is Row, C5 is Col, R6 is Row + 1, C6 is Col
    ).


% Validates whether a given 2x3 block forms a "switch" pattern, 
% which is defined as specific arrangements of player and opponent pieces.
% Inputs:
%   - Board: Current game board.
%   - R1, C1, ..., R6, C6: Coordinates of the 2x3 block.
%   - Player: Current player.
%   - Opponent: Opponent player.
% ---------------------------------------------------------------------------
check_switch_pattern(Board, R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, Player, Opponent) :-
    nth_position(Board, (R1, C1), P1),
    nth_position(Board, (R2, C2), P2),
    nth_position(Board, (R3, C3), P3),
    nth_position(Board, (R4, C4), P4),
    nth_position(Board, (R5, C5), P5),
    nth_position(Board, (R6, C6), P6),
    (   
        P1 = Player, P6 = Player, P2 = Opponent, P5 = Opponent, P3 = empty, P4 = empty
    ;
        P1 = Opponent, P6 = Opponent, P2 = Player, P5 = Player, P3 = empty, P4 = empty
    ).

% Checks if placing a move at a given position creates a "switch" pattern 
% in any 2x4 areas around the move.
% Inputs:
%   - Board: Current game board.
%   - Row, Col: Position of the move being checked.
%   - Player: Current player.
% ---------------------------------------------------------------------------
creates_switches_2_x_4(Board, Row, Col, Player) :-
    % Make the temporary move
    make_move(Board, Row, Col, Player, TempBoard),    
    % Get the opposite player
    opposite(Player, Opponent),    
    % Check for switches
    find_switch_2x4(TempBoard, Row, Col, Player, Opponent),
    % Revert the move if a switch is created
    make_move(TempBoard, Row, Col, empty, _).


% Generates the coordinates of all valid 2x4 blocks around a given position.
% These blocks are used to check for switch patterns.
% Inputs:
%   - R1, C1, ..., R8, C8: Coordinates of the 2x4 block.
%   - Row, Col: Position being checked.
% ---------------------------------------------------------------------------
% Finds all 2x4 blocks around the given position and checks if any of them 
% form a "switch" pattern with the current player's move.
% Inputs:
%   - Board: Current game board.
%   - Row, Col: Position of the move being checked.
%   - Player: Current player.
%   - Opponent: Opponent player.
% ---------------------------------------------------------------------------
find_switch_2x4(Board, Row, Col, Player, Opponent) :-
    % Find all valid 2x4 blocks around the move
    findall((R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8), 
        (
            valid_2x4_block(R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8, Row, Col)
        ),
        Areas),
    % Check if any of the blocks form a switch pattern
    member((R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8), Areas),
    check_switch_pattern_2x4(Board, R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8, Player, Opponent).

% Generates the coordinates of all valid 2x4 blocks around a given position.
% These blocks are used to check for switch patterns.
% Inputs:
%   - R1, C1, ..., R8, C8: Coordinates of the 2x4 block.
%   - Row, Col: Position being checked.
% ---------------------------------------------------------------------------
valid_2x4_block(R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8, Row, Col) :-
    (   % Horizontal 2x4 block to the left
        R1 is Row+1, C1 is Col - 3, R2 is Row, C2 is Col - 3, R3 is Row+1, C3 is Col - 2, R4 is Row, C4 is Col-2,
        R5 is Row+1, C5 is Col - 1, R6 is Row, C6 is Col - 1, R7 is Row + 1, C7 is Col, R8 is Row, C8 is Col
    ;   % Horizontal 2x4 block to the right
        R1 is Row, C1 is Col-3, R2 is Row-1, C2 is Col -3, R3 is Row, C3 is Col - 2, R4 is Row-1, C4 is Col -2,
        R5 is Row, C5 is Col-1, R6 is Row - 1, C6 is Col - 1, R7 is Row, C7 is Col, R8 is Row - 1, C8 is Col
    ;   % Vertical 2x4 block above
        R1 is Row - 3, C1 is Col-1, R2 is Row -3, C2 is Col, R3 is Row - 2, C3 is Col-1, R4 is Row-1, C4 is Col,
        R5 is Row - 1, C5 is Col - 1, R6 is Row - 1, C6 is Col, R7 is Row, C7 is Col - 1, R8 is Row, C8 is Col
    ;   % Vertical 2x4 block below
        R1 is Row -3, C1 is Col, R2 is Row -3, C2 is Col+1, R3 is Row - 2, C3 is Col, R4 is Row -2, C4 is Col+1,
        R5 is Row-1, C5 is Col , R6 is Row - 1, C6 is Col + 1, R7 is Row, C7 is Col, R8 is Row, C8 is Col + 1
    ;
        R1 is Row-1, C1 is Col + 3, R2 is Row, C2 is Col + 3, R3 is Row-1, C3 is Col + 2, R4 is Row, C4 is Col+2,
        R5 is Row-1, C5 is Col + 1, R6 is Row, C6 is Col + 1, R7 is Row -1, C7 is Col, R8 is Row, C8 is Col
    ;
        R1 is Row, C1 is Col+3, R2 is Row+1, C2 is Col +3, R3 is Row, C3 is Col + 2, R4 is Row+1, C4 is Col +2,
        R5 is Row, C5 is Col+1, R6 is Row + 1, C6 is Col + 1, R7 is Row, C7 is Col, R8 is Row + 1, C8 is Col
    ;   % Vertical 2x4 block below
        R1 is Row + 3, C1 is Col+1, R2 is Row +3, C2 is Col, R3 is Row + 2, C3 is Col-1, R4 is Row+2, C4 is Col,
        R5 is Row + 1, C5 is Col + 1, R6 is Row + 1, C6 is Col, R7 is Row, C7 is Col + 1, R8 is Row, C8 is Col
    ;   % Vertical 2x4 block 
        R1 is Row +3, C1 is Col, R2 is Row +3, C2 is Col-1, R3 is Row + 2, C3 is Col, R4 is Row +2, C4 is Col-1,
        R5 is Row+1, C5 is Col , R6 is Row + 1, C6 is Col - 1, R7 is Row, C7 is Col, R8 is Row, C8 is Col - 1
    ).

% Validates whether a given 2x4 block forms a "switch" pattern, 
% which is defined as specific arrangements of player and opponent pieces.
% Inputs:
%   - Board: Current game board.
%   - R1, C1, ..., R8, C8: Coordinates of the 2x4 block.
%   - Player: Current player.
%   - Opponent: Opponent player.
% ---------------------------------------------------------------------------
check_switch_pattern_2x4(Board, R1, C1, R2, C2, R3, C3, R4, C4, R5, C5, R6, C6, R7, C7, R8, C8, Player, Opponent) :-
    nth_position(Board, (R1, C1), P1),
    nth_position(Board, (R2, C2), P2),
    nth_position(Board, (R3, C3), P3),
    nth_position(Board, (R4, C4), P4),
    nth_position(Board, (R5, C5), P5),
    nth_position(Board, (R6, C6), P6),
    nth_position(Board, (R7, C7), P7),
    nth_position(Board, (R8, C8), P8),
    (   % Player stones on the outermost positions, Opponent on the next positions, centers empty
        P1 = Player, P8 = Player, P2 = Opponent, P7 = Opponent,
        P3 = empty, P6 = empty, P4 = empty, P5 = empty
    ;   % Opponent stones on the outermost positions, Player on the next positions, centers empty
        P1 = Opponent, P8 = Opponent, P2 = Player, P7 = Player,
        P3 = empty, P6 = empty, P4 = empty, P5 = empty
    ).

% Make a move on the board
make_move(Board, Row, Col, Player, NewBoard) :- 
    nth1(Row, Board, RowList),
    replace(RowList, Col, Player, NewRow),
    replace(Board, Row, NewRow, NewBoard).

% Replace an element in a list
replace([_|T], 1, Elem, [Elem|T]).
replace([H|T], Pos, Elem, [H|NewT]) :- 
    Pos > 1,
    Pos1 is Pos - 1,
    replace(T, Pos1, Elem, NewT).

% Adjacent positions for a given cell
adjacent_positions((Row, Col), Size, AdjacentPositions) :- 
    findall((R, C),
        (between(1, Size, R),
         between(1, Size, C),
         abs(R - Row) =< 1,
         abs(C - Col) =< 1,
         \+ (R = Row, C = Col)),
        AdjacentPositions).

% Retrieve the value of a cell on the board
nth_position(Board, (Row, Col), Value) :- 
    nth1(Row, Board, BoardRow),
    nth1(Col, BoardRow, Value).

% Check if a player has a connected path (from one edge to the opposite edge)
connected_path(Board, Player, Start, End, Size) :-
    path_exists(Board, Start, End, Player, [], Size).

% Depth-first search to check for path
path_exists(Board, Current, End, Player, Visited, Size) :-
    Current = End,  % Reached the end
    nth_position(Board, Current, Player).  % End position must be occupied by the player

% Determine if path exists
path_exists(Board, (Row, Col), End, Player, Visited, Size) :-
    \+ member((Row, Col), Visited),  % Ensure we haven't visited this position yet
    nth_position(Board, (Row, Col), Player),  % Current position is occupied by the player
    adjacent_positions((Row, Col), Size, Neighbors),  % Get adjacent positions
    member(Neighbor, Neighbors),  % Explore each adjacent position
    path_exists(Board, Neighbor, End, Player, [(Row, Col) | Visited], Size).  % Recur with the new position

% Announce the winner
announce_winner(Winner) :- 
    nl, write('Game Over! Winner: '), write(Winner), nl.


% Handles the human player's move in the game. This predicate prompts the player to enter their move, 
% validates the move, checks if it adheres to specific game rules, and updates the game state accordingly. 
% If the move is invalid, it prompts the player to try again.
% Inputs:
%   - GameState: The current state of the game, represented as game_state(Board, Player, Config, Size).
%   - Board: The current game board.
%   - Player: The player making the current move.
%   - Config: Additional configuration for the game (not used explicitly here).
%   - Size: The size of the game board.
% ---------------------------------------------------------------------------
human_move(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size)) :-
    write('Enter your move (row, column): '),
    read(Move),  % Reads the move input as a tuple
    validate_move_format(Move, Row, Col),  % Validates the move format
    valid_move(Board, Row, Col, Size),  % Checks if the cell is empty
    (   creates_switches_2_x_3(Board, Row, Col, Player),
        write('Invalid move. This move creates a 2x3 switch. Please try again.\n'),
        human_move(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size))
    ;   creates_switches_2_x_4(Board, Row, Col, Player),
        write('Invalid move. This move creates a 2x4 switch. Please try again.\n'),
        human_move(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size))
    ;   creates_hard_corner(Board, Row, Col, Player, Size),
        write('Invalid move. This move creates a hard corner. Please try again.\n'),
        human_move(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size))
    ;   make_move(Board, Row, Col, Player, NewBoard),  % Executes the move if all checks pass
        switch_player(Player, NextPlayer)  % Switches the player
    )
    ;   write('Invalid move. The cell is not empty. Please try again.\n'),
        human_move(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size))  % Prompt again
    .

% Validates if the move format is right (Row, Col)
validate_move_format((Row, Col), Row, Col) :- 
    integer(Row), integer(Col), !.

% Validates if a move is valid
validate_move_format(_, _, _) :- 
    write('Invalid move format. Please enter in the format (Row, Col).\n'), fail.

% Validates if a move is valid for a given board and player
valid_move(Board, Row, Col, Size) :-
    Row >= 1, Row =< Size,  % Check if Row is within bounds
    Col >= 1, Col =< Size,  % Check if Col is within bounds
    nth1(Row, Board, RowList),
    nth1(Col, RowList, CellValue),  % Get the value of the selected cell
    CellValue = empty.  % Check if the selected cell is empty

% Computer's move based on the chosen difficulty
computer_move(Difficulty, GameState, NextGameState) :-
    (   Difficulty = easy,
        computer_move_easy(GameState, NextGameState)
    ;   Difficulty = hard,
        computer_move_hard(GameState, NextGameState)
    ).

% Computer's move avoiding hard corners and using strategic placement
computer_move_easy(game_state(Board, Player, _Config, Size), game_state(NewBoard, NextPlayer, _Config, Size)) :-
    valid_moves(game_state(Board, Player, _Config, Size), Moves),  % Get all valid moves for the current game state
    shuffle_list(Moves, ShuffledMoves),  % Shuffle the valid moves to add randomness
    member((Row, Col), ShuffledMoves),  % Select a potential move from the list of valid moves
    \+ creates_hard_corner(Board, Row, Col, Player, Size),  % Ensure the move does not create a hard corner for the opponent
    \+ creates_switches_2_x_3(Board, Row, Col, Player),  % Ensure the move does not create a 2x3 switch pattern for the opponent
    \+ creates_switches_2_x_4(Board, Row, Col, Player),  % Ensure the move does not create a 2x4 switch pattern for the opponent
    make_move(Board, Row, Col, Player, NewBoard),  % Apply the move to update the board
    switch_player(Player, NextPlayer).  % Switch to the next player after making the move

% Shuffles a list
shuffle_list([], []).
shuffle_list(List, Shuffled) :-
    random_permutation(List, Shuffled).

% Computer's move in hard mode, considering blocking and maximizing advantage
computer_move_hard(game_state(Board, Player, Config, Size), game_state(NewBoard, NextPlayer, Config, Size)) :-
    valid_moves(game_state(Board, Player, Config, Size), Moves),  % Get all valid moves
    choose_move(Board, Player, Moves, Size, (Row, Col)),  % Choose the best move
    make_move(Board, Row, Col, Player, NewBoard),  % Execute the move
    switch_player(Player, NextPlayer).  % Switch player

% Choose the best move (win-first approach, then blocking, then greedy)
choose_move(Board, Player, Moves, Size, BestMove) :-
    % Prioritize a winning move
    (   find_winning_move(Board, Player, Moves, Size, BestMove)
    ;   switch_player(Player, Opponent),
        % Otherwise, prioritize blocking the opponent
        find_blocking_move(Board, Opponent, Moves, Size, BestMove)
    ;   % Finally, evaluate the best move
        evaluate_best_move(Board, Player, Moves, Size, BestMove)
    ).


% Find a move that results in a win for the current player
find_winning_move(Board, Player, Moves, Size, WinningMove) :-
    member((Row, Col), Moves),
    make_move(Board, Row, Col, Player, NewBoard),
    (   Player = black, connected_path(NewBoard, black, (1, _), (Size, _), Size)  % Black wins top to bottom
    ;   Player = white, connected_path(NewBoard, white, (_, 1), (_, Size), Size)  % White wins left to right
    ),
    WinningMove = (Row, Col),
    !.  % Stop after finding the first winning move


% Find a move that blocks the opponent's winning path
% Find a move that blocks the opponent's winning path (both vertically and horizontally)
find_blocking_move(Board, Opponent, Moves, Size, BlockingMove) :-
    member((Row, Col), Moves),
    make_move(Board, Row, Col, Opponent, NewBoard),
    (   % Check if the move blocks a vertical winning path
        connected_path(NewBoard, Opponent, (1, _), (Size, _), Size)
    ;   % Check if the move blocks a horizontal winning path
        connected_path(NewBoard, Opponent, (_, 1), (_, Size), Size)
    ),
    BlockingMove = (Row, Col),
    !.  % Stop after finding the first valid blocking move

% Evaluate and select the best move (greedy approach)
evaluate_best_move(Board, Player, Moves, Size, BestMove) :-
    maplist(score_move(Board, Player, Size), Moves, ScoredMoves),  % Score each move
    max_score_move(ScoredMoves, BestMove).  % Select the move with the highest score

% Score a move based on its advantage
score_move(Board, Player, Size, (Row, Col), (Score, (Row, Col))) :-
    make_move(Board, Row, Col, Player, NewBoard),
    estimate_advantage(NewBoard, Player, Size, Score).

% Find the move with the highest score
max_score_move(ScoredMoves, BestMove) :-
    sort(ScoredMoves, Sorted),  % Sort moves by score
    last(Sorted, (_, BestMove)).  % Select the move with the highest score

% Estimate advantage by comparing player and opponent connections
estimate_advantage(Board, Player, Size, Score) :-
    count_connections(Board, Player, Size, PlayerConnections),
    switch_player(Player, Opponent),
    count_connections(Board, Opponent, Size, OpponentConnections),
    Score is PlayerConnections - OpponentConnections.
