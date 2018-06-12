/*
*  pacman.c
*
*  The Game
*  ~PP
*/

#include <ncurses.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include "pacman.h"

const char wall = '#';
const char empty = ' ';
const char point = '*';

short color_wall;
short color_point;
short color_pacman;
short color_ghost;

void exit_error() {
    perror("");
    exit(EXIT_FAILURE);
}

void sleep_nano(long nanoseconds){
    struct timespec req;
    req.tv_sec = 0, req.tv_nsec = nanoseconds;
    nanosleep(&req, NULL);
}

void create_board(board_screen *board) {
    int wall_size_x, wall_size_y, y, x, y_wall, x_wall;

    board->points = 0;
    board->field = malloc((board->ymax)*sizeof(char *));
    if(board->field == NULL)
        exit_error();

    for(y = 0; y < board->ymax; y++) {
        board->field[y] = malloc((board->xmax) * sizeof(char));
        if(board->field[y] == NULL)
            exit_error();
        for(x = 0; x < board->xmax; x++) {
            if(rand()%POINT_DENSITY == 0)
                board->field[y][x] = point;
            else
                board->field[y][x] = empty;
        }
    }

    for(y = 0; y < board->ymax; y++) {
        for(x = 0; x < board->xmax; x++) {
            if(board->field[y][x] != wall) {
                if(rand()%WALL_DENSITY == 0) { // maka a wall
                    wall_size_x = rand() % MAX_WALL_SIZE;
                    wall_size_y = rand() % MAX_WALL_SIZE;
                    for(y_wall = y; y_wall < y+wall_size_y && y_wall < board->ymax; y_wall++)
                        for(x_wall = x; x_wall < x+wall_size_x && x_wall < board->xmax; x_wall++)
                            board->field[y_wall][x_wall] = wall;
                }
            }
        }
    }
}


void display_board(board_screen *board) {
    for(int y = 0; y < board->ymax; y++)
        for(int x = 0; x < board->xmax; x++) {
            if(board->field[y][x] == wall)
                attron(COLOR_PAIR(color_wall));
            else
                attron(COLOR_PAIR(color_point));
            mvaddch(y+MENU_HEIGHT, x, board->field[y][x]);
        }
    refresh();
}


void create_player(board_screen *board) {
    int py, px;
    while(true) {
        py = rand()%board->ymax;
        px = rand()%board->xmax;
        if(board->field[py][px] != wall) {
            board->player_pos[0] = py;
            board->player_pos[1] = px;
            break;
        }
    }
    board->player_window = newwin(1, 1, py+MENU_HEIGHT, px);
    if(board->player_window == NULL)
        exit_error();
    wbkgd(board->player_window, 'Y' | COLOR_PAIR(color_pacman));
    wrefresh(board->player_window);
}

char check_move_player(board_screen *board, short direction, int *new_position, chtype *pacman_char) {
    if (direction == LEFT) {
        new_position[0] = 0,   new_position[1] = -1; *pacman_char = '>';
    } else if (direction == RIGHT) {
        new_position[0] = 0,   new_position[1] = 1; *pacman_char = '<';
    } else if (direction == UP) {
        new_position[0] = -1,  new_position[1] = 0; *pacman_char = 'Y';
    } else if (direction == DOWN) {
        new_position[0] = 1,   new_position[1] = 0; *pacman_char = '^';
    }
    new_position[0] = (board->player_pos[0] + new_position[0] + board->ymax) % board->ymax;
    new_position[1] = (board->player_pos[1] + new_position[1] + board->xmax) % board->xmax;
    return board->field[new_position[0]][new_position[1]];
}

char check_move_ghost(board_screen *board, short direction, int *new_position, int ghost_no) {
    if (direction == LEFT) {
        new_position[0] = 0,   new_position[1] = -1;
    } else if (direction == RIGHT) {
        new_position[0] = 0,   new_position[1] = 1;
    } else if (direction == UP) {
        new_position[0] = -1,  new_position[1] = 0; 
    } else if (direction == DOWN) {
        new_position[0] = 1,   new_position[1] = 0;
    }
    new_position[0] = (board->ghosts_pos[ghost_no][0] + new_position[0] + board->ymax) % board->ymax;
    new_position[1] = (board->ghosts_pos[ghost_no][1] + new_position[1] + board->xmax) % board->xmax;
    return board->field[new_position[0]][new_position[1]];
}

void get_best_directions(board_screen *board, int ghost_no, short best_directions[4]) {
    int distances[4]; // down, up, left, right
    best_directions[0] = DOWN;
    best_directions[1] = UP;
    best_directions[2] = LEFT;
    best_directions[3] = RIGHT;

    int *player_pos = board->player_pos;
    int *ghost_pos = board->ghosts_pos[ghost_no];

    if(player_pos[0] > ghost_pos[0]) {
        distances[0] = player_pos[0] - ghost_pos[0];
        distances[1] = board->ymax - distances[0];
    } else {
        distances[1] = ghost_pos[0] - player_pos[0];
        distances[0] = board->ymax - distances[1];
    }

    if(player_pos[1] > ghost_pos[1]) {
        distances[3] = player_pos[1] - ghost_pos[1];
        distances[2] = board->xmax - distances[3];
    } else {
        distances[2] = ghost_pos[1] - player_pos[1];
        distances[3] = board->xmax - distances[2];
    }


    for(int i=1; i<4; i++) {
        for(int j=i; j>0; j--) {
            if(distances[j] < distances[j-1]) {
                distances[j] ^= distances[j-1];
                distances[j-1] ^= distances[j];
                distances[j] ^= distances[j-1];

                best_directions[j] ^= best_directions[j-1];
                best_directions[j-1] ^= best_directions[j];
                best_directions[j] ^= best_directions[j-1];
            }
        }
    }
}

bool move_ghosts(board_screen *board) {
    short best_directions[4];
    int new_position[2];
    for(int i=0; i < GHOSTS_AMOUNT; i++) {
        get_best_directions(board, i, best_directions);
        for(short j=0; j<4; j++) {
            if( ((best_directions[j] == UP || best_directions[j] == DOWN) && board->ghosts_pos[i][0] != board->player_pos[0]) ||
                ((best_directions[j] == LEFT || best_directions[j] == RIGHT) && board->ghosts_pos[i][1] != board->player_pos[1])) {
                if(check_move_ghost(board, best_directions[j], new_position, i) != wall) {
                    board->ghosts_pos[i][0] = new_position[0];
                    board->ghosts_pos[i][1] = new_position[1];
                    break;
                }
            }
        }

        // delete points
        if(board->field[board->ghosts_pos[i][0]][board->ghosts_pos[i][1]] == point)
            board->field[board->ghosts_pos[i][0]][board->ghosts_pos[i][1]] = empty;

        // check if player failed
        if(board->ghosts_pos[i][0] == board->player_pos[0] && board->ghosts_pos[i][1] == board->player_pos[1])
            return false;

        //refresh now
        wbkgd(board->ghosts_window[i], ' ');
        wrefresh(board->ghosts_window[i]);
        delwin(board->ghosts_window[i]);

        board->ghosts_window[i] = newwin(1, 1, board->ghosts_pos[i][0]+MENU_HEIGHT, board->ghosts_pos[i][1]);
        wbkgd(board->ghosts_window[i], 'x' | COLOR_PAIR(color_ghost) | A_BOLD);
        wrefresh(board->ghosts_window[i]);
    }
    return true;
}

short make_move(board_screen *board, short change_direction, short move_direction, bool *failed) {
    int new_position[2];
    short new_direction = move_direction;
    chtype pacman_char;

    // try move player
    if(check_move_player(board, change_direction, new_position, &pacman_char) != wall) {
        board->player_pos[0] = new_position[0];
        board->player_pos[1] = new_position[1];
        new_direction = change_direction;
    } else if(check_move_player(board, move_direction, new_position, &pacman_char) != wall){
        board->player_pos[0] = new_position[0];
        board->player_pos[1] = new_position[1];
        new_direction = move_direction;
    }
    // check and move ghosts
    if(move_ghosts(board) == false){
        *failed = true;
    }

    // get points
    if(*failed == false)
        if(board->field[board->player_pos[0]][board->player_pos[1]] == point) {
            board->field[board->player_pos[0]][board->player_pos[1]] = empty;
            board->points++;
            update_menu(board);
        }

    //refresh now
    wbkgd(board->player_window, ' ');
    wrefresh(board->player_window);
    delwin(board->player_window);

    board->player_window = newwin(1, 1, board->player_pos[0]+MENU_HEIGHT, board->player_pos[1]);
    wbkgd(board->player_window, 'O' | COLOR_PAIR(color_pacman) | A_BOLD);
    wrefresh(board->player_window);
    sleep_nano(90000000);

    // refresh with ghosts
    wbkgd(board->player_window, pacman_char | COLOR_PAIR(color_pacman) | A_BOLD);
    wrefresh(board->player_window);
    for(int i=0; i < GHOSTS_AMOUNT; i++) {
        wbkgd(board->ghosts_window[i], 'X' | COLOR_PAIR(color_ghost) | A_BOLD);
        wrefresh(board->ghosts_window[i]);
    }
    sleep_nano(100000000);

   return new_direction;
}


void create_menu(board_screen *board) {
    board->menu = newwin(MENU_HEIGHT, board->xmax, 0, 0);
    box(board->menu, 0, 0);
    mvwprintw(board->menu, 1, 5, "Points: 0");
    mvwprintw(board->menu, 1, board->xmax-20, "esc - end game");
    wrefresh(board->menu);
}


void update_menu(board_screen *board) {
    wclear(board->menu);
    box(board->menu, 0, 0);
    mvwprintw(board->menu, 1, 5, "Points: %d", board->points);
    mvwprintw(board->menu, 1, board->xmax-20, "esc - end game");
    wrefresh(board->menu);
}


void create_ghosts(board_screen *board) {
    int py, px;
    for(int i = 0; i < GHOSTS_AMOUNT; i++) {
        while(true) {
            py = rand()%board->ymax;
            px = rand()%board->xmax;
            if(board->field[py][px] == empty || board->field[py][px] == point) {
                board->ghosts_pos[i][0] = py;
                board->ghosts_pos[i][1] = px;
                break;
            }
        }
        board->ghosts_window[i] = newwin(1, 1, py+MENU_HEIGHT, px);
        if(board->ghosts_window[i] == NULL)
            exit_error();
        wbkgd(board->ghosts_window[i], 'X' | COLOR_PAIR(color_ghost) | A_BOLD);
        wrefresh(board->ghosts_window[i]);
    }
}

void play() {
    int ymax, xmax, ch;
    short change_direction = UP, move_direction = UP;
    bool failed = false;

    getmaxyx(stdscr, ymax, xmax);
    board_screen board;
    board.ymax = ymax - MENU_HEIGHT;
    board.xmax = xmax;

    create_board(&board);
    display_board(&board);

    create_menu(&board);
    create_player(&board);
    create_ghosts(&board);

    while (((ch = getch()) != 27) && failed == false) {    
        if (ch == KEY_LEFT) {
            change_direction = LEFT;
        } else if (ch == KEY_RIGHT) {
            change_direction = RIGHT;
        } else if (ch == KEY_UP) {
            change_direction = UP;
        } else if (ch == KEY_DOWN) {
            change_direction = DOWN;
        }
        move_direction = make_move(&board, change_direction, move_direction, &failed);
        change_direction = move_direction;
    }

    wclear(board.menu);
    box(board.menu, 0, 0);
    mvwprintw(board.menu, 1, 5, "Points: %d", board.points);
    mvwprintw(board.menu, 1, board.xmax-20, "esc - end game");
    wattron(board.menu, COLOR_PAIR(7));
    mvwprintw(board.menu, 1, 20, "END OF GAME");
    wrefresh(board.menu);
    sleep(2);
    
    while (ch != 27) {
        sleep(1);
        ch = getch();
    }
    clear_game(&board);
}

void clear_game(board_screen *board) {
    for(int y = 0; y < board->ymax; y++)
        free(board->field[y]);
    free(board->field);
    delwin(board->player_window);
    delwin(board->menu);
    for(int i=0; i<GHOSTS_AMOUNT; i++)
        delwin(board->ghosts_window[i]);
}


int main() {
    srand(6983123+time(0));

    initscr();
    cbreak();
    nodelay(stdscr, TRUE);
    keypad(stdscr, TRUE);
    noecho();
    curs_set(0);

    start_color();
    init_pair(1, COLOR_MAGENTA, COLOR_BLACK);
    init_pair(2, COLOR_CYAN, COLOR_BLACK);
    init_pair(3, COLOR_GREEN, COLOR_BLACK);
    init_pair(4, COLOR_WHITE, COLOR_BLACK);
    init_pair(5, COLOR_BLUE, COLOR_BLACK);
    init_pair(6, COLOR_YELLOW, COLOR_BLACK);
    init_pair(7, COLOR_RED, COLOR_BLACK);


    color_wall = rand()%4 + 1;
    color_point = 6;
    color_pacman = 7;
    color_ghost = 5;
    wbkgd(stdscr, COLOR_PAIR(color_wall));

    play();

    endwin();
    return 0;
}
