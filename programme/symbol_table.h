#ifndef SYMBOL_H_
#define SYMBOL_H_

// Copied from task sheet
typedef enum { _CONST=0, _SCALAR, _ARRAY } entry_type;
typedef enum { _BOOL=0, _INT, _REAL, _STRING } data_type;

typedef struct _entry {
  entry_type etype ;
  data_type dtype ;
  union {
    int int_val ;
    float real_val ;
    char *identifier ; /* identifier, index */
  } symbol;
  struct _entry *next ; /* collision list */
} entry ;

entry *symbol_get_or_add_int(const data_type data_type, const int val);
entry *symbol_get_or_add_real(const double val);
entry *symbol_get_or_add_string(char *string);

entry *symbol_add_ident(const entry_type entry_type, const data_type data_type, char * const identifier);
entry *symbol_get_ident(const char * const identifier);

void symbol_print_table();
void symbol_free();

#endif
