#include "symbol_table.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

entry *symbol_head = NULL;

entry *symbol_create_entry(const entry_type entry_type, const data_type data_type);
void symbol_free_entries(entry *entry);

entry *symbol_get_or_add_int(const data_type data_type, const int val) {
	entry *entry = symbol_head;
	while (entry != NULL) {
		if (entry->etype == _CONST && entry->dtype == _INT && entry->symbol.int_val == val) {
			return entry;
		} else {
			entry = entry->next;
		}
	}

	// not found
	entry = symbol_create_entry(_CONST, data_type);
	entry->symbol.int_val = val;
	return entry;
}

entry *symbol_get_or_add_real(const double val) {
	entry *entry = symbol_head;
	while (entry != NULL) {
		if (entry->etype == _CONST && entry->dtype == _REAL && entry->symbol.real_val == val) {
			return entry;
		} else {
			entry = entry->next;
		}
	}

	// not found
	entry = symbol_create_entry(_CONST, _REAL);
	entry->symbol.real_val = val;
	return entry;
}

entry *symbol_get_or_add_string(char *string) {
	entry *entry = symbol_head;
	while (entry != NULL) {
		if (entry->etype == _CONST && entry->dtype == _STRING && strcmp(entry->symbol.identifier, string) == 0) {
			return entry;
		} else {
			entry = entry->next;
		}
	}

	// not found
	entry = symbol_create_entry(_CONST, _STRING);
	entry->symbol.identifier = string;
	return entry;
}

entry *symbol_create_entry(const entry_type entry_type, const data_type data_type) {
	entry *const entry = calloc(1, sizeof(entry));
	entry->etype = entry_type;
	entry->dtype = data_type;

	entry->next = symbol_head;
	symbol_head = entry;
	return entry;
}

entry *symbol_add_ident(const entry_type entry_type, const data_type data_type, char *const identifier) {
	entry *entry = symbol_get_ident(identifier);
	if (entry != NULL) {
		fprintf(stderr, "identifier '%s' already exists\n", identifier);
		return entry;
	}

	entry = symbol_create_entry(entry_type, data_type);
	entry->symbol.identifier = identifier;
	return entry;
}

entry *symbol_get_ident(const char *const identifier) {
	entry *entry = symbol_head;
	while (entry != NULL) {
		if (entry->etype != _CONST && strcmp(entry->symbol.identifier, identifier) == 0) {
			return entry;
		} else {
			entry = entry->next;
		}
	}

	return NULL;
}

void symbol_free() {
	symbol_free_entries(symbol_head);
	symbol_head = NULL;
}

void symbol_free_entries(entry *entry) {
	if (entry != NULL) {
		symbol_free_entries(entry->next);
		if (entry->etype == _SCALAR || entry->etype == _ARRAY || entry->dtype == _STRING) {
			free(entry->symbol.identifier);
		}
		free(entry);
	}
}

void symbol_print_table() {
	entry *entry = symbol_head;
	while (entry != NULL) {
		switch (entry->etype) {
		case _CONST:
			printf("const\t");
			break;
		case _SCALAR:
			printf("scalar\t");
			break;
		case _ARRAY:
			printf("array\t");
			break;
		}

		switch (entry->dtype) {
		case _BOOL:
			printf("boolean\t");
			break;
		case _STRING:
			printf("string\t");
			break;
		case _INT:
			printf("integer\t");
			break;
		case _REAL:
			printf("real\t");
			break;
		}

		if (entry->etype == _CONST) {
			switch (entry->dtype) {
			case _BOOL:
				if (entry->symbol.int_val) {
					printf("true");
				} else {
					printf("false");
				}
				break;
			case _STRING:
				printf("%s", entry->symbol.identifier);
				break;
			case _INT:
				printf("%d", entry->symbol.int_val);
				break;
			case _REAL:
				printf("%f", entry->symbol.real_val);
				break;
			}
		} else {
			printf("%s", entry->symbol.identifier);
		}
		
		printf("\n");
		entry = entry->next;
	}
}
