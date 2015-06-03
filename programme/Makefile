YACC=bison
YFLAGS = -d

all: parser

parser: node.o parser.o scanner.o symbol_table.o

node.o: node.c parser.y

symbol_table.o: symbol_table.c

scanner.o: scanner.l parser.y

parser.o: parser.tab.c symbol_table.o
	$(CC) -c -o parser.o parser.tab.c

parser.tab.c: parser.y symbol_table.o
	$(YACC) $(YFLAGS) parser.y


clean:
	$(RM) parser *.o *.tab.*
