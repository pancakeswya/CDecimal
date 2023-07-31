OS            := $(shell uname)

OPEN          := $(if ($(OS), Linux),xdg-open,open)

NAME          := s21_decimal.a

CC            := gcc
CFLAGS        := -Wall -Wextra -Werror -std=c11

VALGRIND      := valgrind --tool=memcheck --trace-children=yes --track-origins=yes --leak-check=full

SRC_DIR       := src
SRCS          := src/s21_decimal.c
OBJS          := $(SRCS:.c=.o)

GCOV          := --coverage
LCOV          := lcov --no-external -c

TEST_NAME     := s21-decimal-test
TEST_LDLIB    := -lcheck
TEST_SRCS     := src/s21-decimal-test.c

LCOV_NAME     := s21-decimal.info

REPORT_DIR    := report
GCOV_FILES    := *.gcno *.gcda

LDFLAGS    	  := $(addprefix -L,$(SRC_DIR))
LDFLAGS       += $(if ($(OS), Linux),-lm -lsubunit)

AR            := ar
ARFLAGS       := -r -c -s

RM            := rm -rf
MAKEFLAGS     += --no-print-directory

all: $(NAME)

$(NAME): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

check-valgrind: test
	CK_FORK=NO $(VALGRIND) ./$(TEST_NAME)

test: $(NAME)
	$(CC) $(CFLAGS) $(TEST_SRCS) $(TEST_LDLIB) $(LDFLAGS) $< -o $(TEST_NAME)
	./$(TEST_NAME)

gcov_report: $(NAME)
	$(CC) $(CFLAGS) $(GCOV) $(TEST_SRCS) $(SRCS) $(TEST_LDLIB) $(LDFLAGS) $< -o $(TEST_NAME)
	./$(TEST_NAME)
	$(LCOV) -t $(TEST_NAME) -d . -o $(LCOV_NAME)
	genhtml $(LCOV_NAME) -o $(REPORT_DIR)
	$(OPEN) $(REPORT_DIR)/index.html

clean:
	$(RM) $(NAME)
	$(RM) $(OBJS)
	$(RM) $(TEST_NAME)

fclean: clean
	$(RM) $(LCOV_NAME)
	$(RM) $(REPORT_DIR)
	$(RM) *.gcno *.gcda

rebuild:
	$(MAKE) clean
	$(MAKE) all