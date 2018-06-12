#ifndef PACMAN
#define PACMAN PACMAN_SET

#define UP 0
#define RIGHT 1
#define DOWN 2
#define LEFT 3
#define WALL_DENSITY 20
#define POINT_DENSITY 3
#define MAX_WALL_SIZE 5
#define MENU_HEIGHT 3
#define GHOSTS_AMOUNT 5

typedef struct board_struct {
    int ymax;
    int xmax;
    int player_pos[2];
    int ghosts_pos[GHOSTS_AMOUNT][2];
    WINDOW *player_window;
    WINDOW *menu;
    WINDOW *ghosts_window[GHOSTS_AMOUNT];
    char **field;
    int points;
} board_screen;


void sleep_nano(long nanoseconds);

void create_board(board_screen *board);
void display_board(board_screen *board);

void create_menu(board_screen *board);
void update_menu(board_screen *board);

void create_player(board_screen *board);
void create_ghosts(board_screen *board);

short make_move(board_screen *board, short change_direction, short move_direction, bool *failed);
char check_move_player(board_screen *board, short direction, int *new_position, chtype *pacman_char);
char check_move_ghost(board_screen *board, short direction, int *new_position, int ghost_no);
bool move_ghosts(board_screen *board);
void get_best_directions(board_screen *board, int ghost_no, short best_directions[4]);

void play();
void clear_game(board_screen *board);

#endif