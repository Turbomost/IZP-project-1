#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ctype.h>

//Configuration struct
typedef struct{
    //Vars 
    //Arguments counts by type, max coordinates, positions and selected rows
    int intcount, strcount, argc, x, y, position, arg_position, selected[100];
    char symbol[100], table[200][200][101], argument[500][5][101], *command[50];
}config;

//Mode enumeration for better comparsions
enum _mode{
  dr, ar, dc, ac,
  Rsel, Csel, Bsel
  
};

//Error enumeration
enum _config_error{
    ERR_SUCCESS,
    ERR_READING_TABLE,
    ERR_INT_ARGUMENT,
    ERR_UNKNOWN_FUNCTION,
    ERR_UNKNOWN_FUNCTION_INT,
    ERR_MISSING_ARGUMENT,
    ERR_MISSING_ARGUMENT_EOF,
    ERR_MEMORY_TABLE,
    ERR_10KiB,
    ERR_100,
    ERR_NO_VALID_FUNCTION
};

///PROTOTYPES
//Input functions
int load_table(config *);
int load_symbols(config *, char *[*]);

//Number functions 
double parameters(char *);
int to_int(double);
int _floor(double);
int _round(double);

//Main functions
int edit(config *, int [*], int, char [*][*][*], int);
int function(config *);
int selection(config *, int [*], char [*][*][*], char *, int);

//Control funcions
bool is_function(char *);
int is_number(char *);
int validation(config *, char*[*]);

//Output functions
int print_table(config *);
int print_error(int, config *, char *[*]);

////FUNCTIONS
///Input functions
//Load table
int load_table(config *cfg){
    
    int newline = 0;
    int line = 1;
    char c;
    cfg -> x = 0;
    cfg -> y = 0;
    
    while ((c = getchar())!= EOF){
        if (!strchr(cfg -> symbol, c)){
            //Basic addition to current string
            if (c!='\n')
                if (sizeof(cfg -> table[cfg -> y]) < 81920){
                    if (c != '\r')
                        strncat(cfg -> table[cfg -> y][cfg -> x], &c, 1);
                }
                else
                    return ERR_10KiB;
            
            //New line
            else{
                //Check if the number of collumns is correct in each row
                if (!newline)
                    newline = line;
                else if (newline != line){
                    //Insufficient or overlapping number of collumns
                    return ERR_READING_TABLE;
                }
                    
                line = 1;
                
                //cfg.x & cfg.y represents the maximum coordinates in the table
                ++cfg -> y;
                cfg -> x = 0;
            }
        }
        
        //New cell
        else{
            if (strlen(cfg -> table[cfg -> y][cfg -> x]) <= 100){
                line++;
                ++cfg -> x;
            }
            else 
                return ERR_MEMORY_TABLE;
        }
    }
    
    cfg -> x = --newline;
    --cfg -> y;
    
    //Succesfull action 
    return ERR_SUCCESS;
}

//Load symbols
int load_symbols(config *cfg, char *argv[]){
    
    //argv[0] is the name of the program
    cfg -> arg_position = 1;
    
    //Load all unique symbols
    if (strcmp(argv[1], "-d") == 0){
        int i;
        cfg -> arg_position = 2;
        char *symbol = cfg -> symbol;
        
        //In this program I am considering that [DELIM] string can be a string with spaces and WITHOUT quotations marks
        //That means that if command line looks like this: $ ./sheet -d : ? ::= icol 1 <tab1.txt >tab1a.txt
        //Program is considering following symbols as the splitting symbols: ":?="
        
        //Load first symbols
        do{
            i = 0;
            while (argv[cfg -> arg_position][i]){
                if (!strchr(symbol, argv[cfg -> arg_position][i])){
                    strncat(symbol, &argv[cfg -> arg_position][i], 1);
                }
                i++;
            }
            ++cfg -> arg_position;
            
            //No valid function
            if (cfg -> arg_position == cfg -> argc){
                return ERR_NO_VALID_FUNCTION;
            }
            
        }while (!is_function(argv[cfg -> arg_position]));
        //In case there is a -d flag without given parameters, program will use first argument(s) as the symbols
        //And keep loading until word is a function (or in case there is blank space in the symbols string)
        // $ ./sheet -d icol 1 icol 1   -> this will set first string as a splitting symbols -> in this case "icol1"
        // $ ./sheet icol 1 icol 1      -> this will set blank space as a splitting symbol
    }
    
    //Without any given symbol symbol, set it to blank space
    else
        cfg -> symbol[0] = ' ';
    
    return 0;
}
    
///Number functions 
//Return arg_position of requiered parameters
double parameters(char *command){
    
    //Úprava
    if (!strcmp(command, "irow"))       {return 1;}     //irow R            -> vloží řádek tabulky před řádek R > 0 (insert-row).
    if (!strcmp(command, "arow"))       {return 0;}     //arow              -> přidá nový řádek tabulky na konec tabulky (append-row).
    if (!strcmp(command, "drow"))       {return 1;}     //drow R            -> odstraní řádek číslo R > 0 (delete-row).
    if (!strcmp(command, "drows"))      {return 2;}     //drows N M         -> odstraní řádky N až M (N <= M). V případě N=M se příkaz chová stejně jako drow N.
    if (!strcmp(command, "icol"))       {return 1;}     //icol C            -> vloží prázdný sloupec před sloupec daný číslem C.
    if (!strcmp(command, "acol"))       {return 0;}     //acol              -> přidá prázdný sloupec za poslední sloupec.
    if (!strcmp(command, "dcol"))       {return 1;}     //dcol C            -> odstraní sloupec číslo C.
    if (!strcmp(command, "dcols"))      {return 2;}     //dcols N M         -> odstraní sloupce N až M (N <= M). V případě N=M se příkaz chová stejně jako dcol N.
    
    //Zpracování
    if (!strcmp(command, "cset"))       {return 1.1;}   //cset C STR        -> do buňky ve sloupci C bude nastaven řetězec STR.
    if (!strcmp(command, "tolower"))    {return 1;}     //tolower C         -> řetězec ve sloupci C bude převeden na malá písmena.
    if (!strcmp(command, "toupper"))    {return 1;}     //toupper C         -> řetězec ve sloupce C bude převeden na velká písmena.
    if (!strcmp(command, "round"))      {return 1;}     //round C           -> ve sloupci C zaokrouhlí číslo na celé číslo.
    if (!strcmp(command, "int"))        {return 1;}     //int C             -> odstraní desetinnou část čísla ve sloupci C.
    if (!strcmp(command, "copy"))       {return 2;}     //copy N M          -> přepíše obsah buněk ve sloupci M hodnotami ze sloupce N.
    if (!strcmp(command, "swap"))       {return 2;}     //swap N M          -> zamění hodnoty buněk ve sloupcích N a M.
    if (!strcmp(command, "move"))       {return 2;}     //move N M          -> přesune sloupec N před sloupec M.
    
    //Selekce
    if (!strcmp(command, "rows"))       {return 2;}     // rows N M         -> procesor bude zpracovávat pouze řádky N až M včetně (N <= M). N=1 znamená zpracování od prvního řádku. Pokud je místo čísla M zadán znak - (pomlčka), ta reprezentuje poslední řádek vstupního souboru. Pokud je pomlčka také místo sloupce N, myslí se tím výběr pouze posledního řádku. Pokud není tento příkaz zadán, uvažuje se implicitně o všech řádcích.
    if (!strcmp(command, "beginswith")) {return 1.1;}   // beginswith C STR -> procesor bude zpracovávat pouze ty řádky, jejichž obsah buňky ve sloupci C začíná řetězcem STR.
    if (!strcmp(command, "contains"))   {return 1.1;}   // contains C STR   -> procesor bude zpracovávat pouze ty řádky, jejichž buňky ve sloupci C obsahují řetězec STR.
    if (is_number(command)) {return -2;}
    return -1;
}

//Double to int = add very small constant to make it natural arg_position
int to_int(double x){
    return x + 1e-9;
}

//Floor function
int _floor(double x){
    if (x >= 0)
        return (int) x;
    return (int) x-1;
}

//Round function
int _round(double x){
    if (x < 0.0)
        return (int)(x - 0.5);
    else
        return (int)(x + 0.5);
}

///Main functions
//Move cells when adding or deleting row(s) or column(s)
int edit(config *cfg, int arg[5], int arg2, char table[200][200][101], int mode){
    
    //'difference' is used for adding OR deleting the exact number of rows and also for the correct refferencing
    int difference;
    int i[2];
    int m_x = cfg -> x;
    int m_y = cfg -> y;
    int intcount = cfg -> intcount;
    
    //Check wheter we are working with columns or rows
    //R and C represents opposite values 1 and 0 and they are used for flexible work with cells
    int C = !(mode == ar ||  mode == dr);
    int R = !C;
    
    //If the mode is using columns switch rows and columns for easy work
    //(m_x and m_y represents max coordinates in the table)
    if (C){
        m_x = m_x - m_y;   
        m_y = m_x + m_y;
        m_x = m_y - m_x;
    }
    
    //Exit funtion in case the first argument is bigger than max_y
    //Or the first argument is bigger than the second and the second one exists(arg[1] != -1)
    if (arg[0] > m_y || (arg[0] > arg[1] && arg[1]+1)){
        return 1;
    }
        
    //Check if parameter is in range of table
    for (i[C] = 0; i[C] <= intcount; i[C]++)
        if(arg[i[C]] > m_y)
            arg[i[C]] = m_y;
    
    //Add the second parameter for functions like arow, acol..
    if (arg2 != -1)
        arg[intcount] = arg2;
    
    
    //Add row(s) or column(s)
    if (mode == ar || mode == ac){
        difference = -1;
        for(i[0] = m_y + 1; i[0] >= arg[0]; i[0]--)
            for(i[1] = 0; i[1] <= m_x; i[1]++)
                if (i[0]!= arg[0])
                    
                    //When working with rows - i[0] is x-coordinate and i[1] is y-coordinate and vice-versa
                    strcpy(table[i[C]][i[R]], table[i[C] + difference * R][i[R] + difference * C]);
                else
                   
                    //When adding the empty row/col
                    strcpy(table[i[C]][i[R]],"");
    }
    
    //Delete row(s) or column(s)
    else if (mode == dr || mode == dc){
        
        //Difference is the number of deleting rows/colls
        difference = arg[1] - arg[0] + 1;
        
        //We are starting at the arg[0] position
        for(i[0] = arg[0]; i[0] <= m_y - difference; i[0]++)
            for(i[1] = 0; i[1] <= m_x; i[1]++)
                
                //Copy from the index (starting from agr[1]+1) to current index (this process overwrite the deleting cells)
                strcpy(table[i[C]][i[R]], table[i[C] + difference * R][i[R] + difference * C]);
    }  
    
    //Now reduce (or increase if the difference is -1) the number of rows/colls
    if (R)
        cfg -> y -= difference;
    else    
        cfg -> x -= difference;
    
    return 1;
}

//Select rows wich meet the requirements
int selection(config *cfg, int arg[5], char table[200][200][101], char *string, int mode){
    
    int min, max, i;
    
    //Row selection represented by numbers (or dashes)
    if (mode == Rsel){
        min = arg[0];
        max = arg[1];
        
        //If the first argument is dash - set minimum to 0
        if (arg[0] == -1)
            min = 0;
        
        //If the second argument is dash - set maximum to maximum y-coordinate
        if ((arg[1] == -1 || arg[1] > cfg -> y))
            max = cfg -> y;
        
        if (arg[0] == -1 && arg[1] == -1)
            min = max;
    }
    
    //'Highlight' selected rows for program (set their indexes to 1 in the selected[*] array)
    for (i = 0; i <= 99; i++)
    
        //Select rows between given min anx max
        if (mode == Rsel && i >= min && i <= max)
            cfg -> selected[i] = 1;
        
        //Select rows whose string in columns arg[0] beggins with given 'string'
        else if (mode == Bsel && !strncmp(table[i][arg[0]], string, strlen(string)))
            cfg -> selected[i] = 1;
        
        //Select rows whose string in columns arg[0] contains given 'string'
        else if (mode == Csel && strstr(table[i][arg[0]], string) != NULL)
            cfg -> selected[i] = 1;
        
        //If none of the selection requirements are fullfiled -> the row is NOT selected
        else
            cfg -> selected[i] = 0;
    
    return 1;
}

//Main functionality
int function(config *cfg){
    
    int i;
    int arg2 = -1;
    int arg[5] = {0};
    int max_y = cfg -> y + 1;
    int max_x = cfg -> x + 1;
    char *string;
    int mode = -1;
    char* command = cfg -> command[cfg -> position];
    
    //Set parameters to integers for easier work
    for (i = 0; i <= cfg -> intcount; i++)
        arg[i] = atoi(cfg -> argument[cfg -> position][i]) -1;
    
    //Find string parameter
    if (cfg -> strcount)
        string = cfg -> argument[cfg -> position][i-1];
    
    ///Selection
        //Setting modes for the selecition function
        if (!strcmp(command, "rows"))
            mode = Rsel;
        
        else if (!strcmp(command, "beginswith"))
            mode = Bsel;
        
        else if (!strcmp(command, "contains"))
            mode = Csel;
    
    //Run the selection funtion with key parameters - 'mode' and 'string'       
    if (mode == Rsel || mode == Bsel || mode == Csel)
        return selection(cfg, arg, cfg -> table, string, mode);
            
    ///Size edits functions        
        //Add empty row before row number arg[0]
        else if (!strcmp(command, "irow"))
            mode = ar;
        
        //Add empty row at the end of the table
        else if (!strcmp(command, "arow")){
            mode = ar;
            arg2 = max_y;
        }
        
        //Delete row number arg[0]
        else if (!strcmp(command, "drow")){ 
            mode = dr;
            arg2 = arg[0];
        }
        
        //Delete selected rows from arg[0] to arg[1]
        else if (!strcmp(command, "drows"))
            mode = dr;
        
        //Add empty column before column number arg[0]
        else if (!strcmp(command, "icol"))
            mode = ac;
        
        //Add empty column at the end of the table 
        else if (!strcmp(command, "acol")){ 
            mode = ac;
            arg2 = max_x;
        }
        
        //Delete column number arg[0]
        else if (!strcmp(command, "dcol")){ 
            mode = dc;
            arg2 = arg[0];
        }
        
        //Delete selected columns from arg[0] to arg[1]
        else if (!strcmp(command, "dcols"))
            mode = dc;
      
    //Run the move funtion with key parameters - 'mode' and 'arg2'
    if (mode == ac || mode == ar || mode == dc || mode == dr)
        return edit(cfg, arg, arg2, cfg -> table, mode);
    
    
    ///Processing
    else if (arg[0] < max_x && arg[1] < max_x){
        for (i = 0; i < max_y; i++)
            if (cfg -> selected[i]){
                
                //Write 'string' in the selected rows with arg[0] as the Y-coordinate
                if (!strcmp(command, "cset"))
                    strcpy(cfg -> table[i][arg[0]], string);
                
                //Uppercase everything in the selected rows with arg[0] as the Y-coordinate
                else if (!strcmp(command, "toupper"))
                    for(int pos = 0; cfg -> table[i][arg[0]][pos]; pos++)
                        cfg -> table[i][arg[0]][pos] = toupper(cfg -> table[i][arg[0]][pos]);
                
                //Lowercase everything in the selected rows with arg[0] as the Y-coordinate
                else if (!strcmp(command, "tolower"))
                    for(int pos = 0; cfg -> table[i][arg[0]][pos]; pos++)
                        cfg -> table[i][arg[0]][pos] = tolower(cfg -> table[i][arg[0]][pos]);
                
                //Copy the arg[0] column to arg[1] columns for every selected row        
                else if (!strcmp(command, "copy"))
                    strcpy(cfg -> table[i][arg[1]], cfg -> table[i][arg[0]]);
                
                //Swap the arg[0] column with arg[1] columns for every selected row    
                else if (!strcmp(command, "swap")){
                    
                    char temp[200];
                    strcpy(temp, cfg -> table[i][arg[0]]);
                    strcpy(cfg -> table[i][arg[0]], cfg -> table[i][arg[1]]);
                    strcpy(cfg -> table[i][arg[1]], temp);
                }
                
                //Move the arg[0] column before arg[1] columns for every selected row 
                else if (!strcmp(command, "move")){
                    char temp[200];
                    strcpy(temp, cfg -> table[i][arg[0]]);
                    
                    if (arg[0] > arg[1])
                        for(int pos = arg[0]; pos > arg[1]; pos--)
                            strcpy(cfg -> table[i][pos], cfg -> table[i][pos-1]);
                    
                    else if (arg[0] < arg[1])
                        for(int pos = arg[0]; pos < arg[1]; pos++)
                            strcpy(cfg -> table[i][pos], cfg -> table[i][pos+1]);
                    
                    strcpy(cfg -> table[i][arg[1]], temp);
                }
                
                //INT and ROUND functions
                else{
                    char *p = cfg -> table[i][arg[0]];
                    while (*p) 
                        if (isdigit(*p) || ((*p=='-'||*p=='+') && isdigit(*(p+1)))){
                            if (!strcmp(command, "int"))
                                sprintf(cfg -> table[i][arg[0]], "%ld", strtol(p, &p, 10));
                            else
                                sprintf(cfg -> table[i][arg[0]], "%i", _round(strtof(p, &p)));
                            break;
                        } 
                        else
                            p++;
                }
            }
    }
    
    return 1;
}

///Control funcions
//Check if the expression is valid function
bool is_function(char *command){
    if (parameters(command) >= 0)
        return true;
    return false;
}

//Check if string contains digits only
int is_number(char *s){
    
    //Check digit by digit inserted string
    for (int i = 0; s[i] != '\0'; i++)
        if (!isdigit(s[i]))
            return false;
    return atoi(s);
}

//Check if function has an required amount of valid arguments
int validation(config *cfg, char *argv[]){
    
    int i;
    int position = ++cfg -> position;
    int intcount = cfg -> intcount;
    int strcount = cfg -> strcount;
    
    //commands are stored in char *command[] array
    cfg -> command[position] = argv[cfg -> arg_position];
    ++cfg -> arg_position;
    
    //Load int arguments and check their number
    for (i = 0; i < intcount; i++){
        
        //Check if the whole string is pure number
        if (is_number(argv[cfg -> arg_position])>0)
            
            //Arguments are stored in char argument[][][] array
            strcpy(cfg -> argument[position][i], argv[cfg -> arg_position]);
        
        //Argument is not a arg_position NOR the fuction is accepting string
        else if (strcmp(argv[cfg -> arg_position], "rows") && strcmp(argv[cfg -> arg_position],"-")){
            if (is_function(argv[cfg -> arg_position]))
                return ERR_MISSING_ARGUMENT;
            return ERR_INT_ARGUMENT;
        }
            
        ++cfg -> arg_position;
    }
    
    //Load string arguments
    for (;i < strcount + intcount; i++){
        strcpy(cfg -> argument[position][i], argv[cfg -> arg_position]);
        ++cfg -> arg_position;
    }
    
    //You can also input arguments as a srtings without using quote marks for example:
    //$ ./sheet -d : cset 1 rows string 2 <tab1.txt >tab1a.txt
    //This program will call cset function with argument "rows string 2" - because neither "string" nor "2" is a fuction
    
    //Check strings with blank spaces and add them to the string argument
    if (strcount > 0){
        while(cfg -> arg_position < cfg -> argc && !is_function(argv[cfg -> arg_position])){
            strcat(cfg -> argument[position][i-1], " ");
            strcat(cfg -> argument[position][i-1], argv[cfg -> arg_position]);
            ++cfg -> arg_position;
        }
    }
    
    //Check lenght of each argument
    for (i = 0; i < strcount + intcount; i++)
        if (strlen(cfg -> argument[position][i]) > 100 ){
            cfg -> arg_position = i;
            return ERR_100;
        }
    
    --cfg -> arg_position;
    return 0;
}

///Output functions
//Print table
int print_table(config *cfg){
    
    int max_y = cfg -> y;
    int max_x = cfg -> x;
    
    //Print everything with symbol[0] as the split symbol
    for(int y = 0; y <= max_y; y++){
        for(int x = 0; x <= max_x; x++){
            printf("%s",cfg -> table[y][x]);
            if (x != max_x)
                printf("%c",cfg -> symbol[0]);
        }
        if (y!= max_y)
            printf("\n");
    }
    
    //Return 0
    return ERR_SUCCESS;
}

//Print error code
int print_error(int er, config *cfg, char *argv[]){
    
    int intcount = cfg -> intcount;
    int strcount = cfg -> strcount;
    char *string_arg = cfg -> argument[cfg -> position][cfg -> arg_position];
    char *argument = argv[--cfg -> arg_position];
    char *command = cfg -> command[cfg -> position];
    int x = cfg -> x + 1;
    int y = cfg -> y + 1;
    
    //Successfull compilation -> print sollution
    if (er == ERR_SUCCESS)
        return print_table(cfg);
    
    if (er == ERR_NO_VALID_FUNCTION){
        fprintf(stderr, "No valid function\n");
        print_table(cfg);
    }
        
    //Row in table is bigger than 10KiB
    else if (er == ERR_10KiB)
        fprintf(stderr, "Row %i is too big. Maximum size is 10 KiB\n", y);
    
    //Srtrlen of argument in function is > 100
    else if (er == ERR_100)
        fprintf(stderr, "Argument in function %s is too long. Maximum size is 100 characters\nLength: %lu\nArgument: %s\n", command, strlen(string_arg), string_arg);
    
    //Strlen in one cell > 100
    else if (er == ERR_MEMORY_TABLE)
        fprintf(stderr, "String in row %i, column %i is too long. \nMaximum lenght: 100\nLength: %lu\n", y, x, strlen(cfg -> table[cfg -> y][cfg -> x]));
    
    //Invalid number of columns while reading table
    else if (er == ERR_READING_TABLE){
        fprintf(stderr, "Wrong amount of columns in row %i\nPlease check input file or split symbols\nSymbols:\t", y);
        for(int i = 0; cfg -> symbol[i] != '\0'; i++)
            fprintf(stderr, "%c",cfg -> symbol[i]);
        fprintf(stderr, "\n");
    }
    //Invalid integer
    else if (er == ERR_INT_ARGUMENT)
        fprintf(stderr, "Expected integer in function '%s'\n'%s' is not valid integer\nRequired integers: %d\n", command, argument, intcount);
    
    //Uknow function
    else if  (er == ERR_UNKNOWN_FUNCTION || er == ERR_UNKNOWN_FUNCTION_INT){
        fprintf(stderr, "Unknow function '%s'\n", argument);
        if  (er == ERR_UNKNOWN_FUNCTION_INT)
            fprintf(stderr, "Maybe you entered too many arguments in function '%s'?\nRequired integers: %d\n", command, intcount);
    }
    
    //Not engough argument in the function
    else if (er == ERR_MISSING_ARGUMENT || er == ERR_MISSING_ARGUMENT_EOF){
        if (er == ERR_MISSING_ARGUMENT_EOF)
            command = argument;
        fprintf(stderr, "Not enough arguments in function '%s'\nRequired integers: %d\n", command, intcount);
    }
        
    //Print requiered number of strings if requiered
    if (strcount)
        fprintf(stderr, "Required string: %d\n", strcount);
    
    //Returning code
    return er;
}

//MAIN
int main(int argc, char *argv[]){
    
    //Set vars
    int error = 0;
    double count = -1;
    
    //Struct vars
    config cfg;
    cfg.argc = argc;
    cfg.position = 0;
    
    //Load symbols and check if the -d flag is there - otehrwise set ' ' char as the splitting symbol
    if (!(error = load_symbols(&cfg, argv)))
        //Load table and check the correct amount of collumns in each row
        error = load_table(&cfg);
    
    //Set all rows to selected for now
    for (int i = 0; i <= cfg.y; i++)
        cfg.selected[i] = 1;
        
    //Loading commands in memory AND check for errors
    while (cfg.arg_position < argc && !error){
        if ((count = parameters(argv[cfg.arg_position])) >= 0){
            
            //Set number of requiered parameters
            cfg.intcount = to_int(_floor(count));
            cfg.strcount = to_int(10*(count-_floor(count)));
            
            //Check if the amount of requiered parameters is not lower then given parameters
            if (cfg.intcount + cfg.strcount + cfg.arg_position < argc){
                if (!(error = validation(&cfg, argv)))
                    function(&cfg);
            }
            
            //Not enought parameters due to end of the file
            else
                error = ERR_MISSING_ARGUMENT_EOF;
        }
        
        //Unknow function
        else
            //Unknow 'function' is number
            if (count + 1)
                error = ERR_UNKNOWN_FUNCTION_INT;
            else
                error = ERR_UNKNOWN_FUNCTION;
        
        //Inrcrement position in input string
        cfg.arg_position++;
        
    }
    
    //Print the error message or sollution -> in case the error value is 0
    return print_error(error, &cfg, argv);
}
