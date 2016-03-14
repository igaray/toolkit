.PHONY: all objects tests tests_build tests_run tests_announce clean

CC=clang
CCFLAGS=-std=c11 -pedantic -Wall -Wextra -Wpedantic -Werror -Wformat
SRC_DIR=./src
INC_DIR=./src
TEST_DIR=./src/tests
BUILD_DIR=./build
TEST_BUILD_DIR=./build/tests

all: objects

objects: ${BUILD_DIR} \
	${BUILD_DIR}/element.o \
	${BUILD_DIR}/linked_list.o \
	${BUILD_DIR}/linked_list_simple.o

tests: objects tests_build tests_run

tests_build: ${TEST_BUILD_DIR} \
	test_build_element \
	test_build_linked_list \
	test_build_linked_list_simple

tests_run: tests_announce \
	test_run_element \
	test_run_linked_list \
	test_run_linked_list_simple

tests_announce:
	@echo
	@echo Running tests...

clean:
	@rm -rf ${BUILD_DIR}

${BUILD_DIR}:
	@mkdir -p ${BUILD_DIR}

${TEST_BUILD_DIR}:
	@mkdir -p ${TEST_BUILD_DIR}

###############################################################################

# Element

${BUILD_DIR}/element.o:
	${CC} ${CCFLAGS} -c ${SRC_DIR}/element.c -o ${BUILD_DIR}/element.o

test_build_element:
	${CC} ${CCFLAGS} -I ${INC_DIR} -c ${TEST_DIR}/test_element.c -o ${TEST_BUILD_DIR}/test_element.o
	${CC} ${CCFLAGS} ${BUILD_DIR}/element.o ${TEST_BUILD_DIR}/test_element.o -o ${TEST_BUILD_DIR}/test_element

test_run_element:
	@${TEST_BUILD_DIR}/test_element

# Linked List
${BUILD_DIR}/linked_list.o:
	${CC} ${CCFLAGS} -c ${SRC_DIR}/linked_list.c -o ${BUILD_DIR}/linked_list.o

test_build_linked_list:
	${CC} ${CCFLAGS} -I ${INC_DIR} -c ${TEST_DIR}/test_linked_list.c -o ${TEST_BUILD_DIR}/test_linked_list.o
	${CC} ${CCFLAGS} ${BUILD_DIR}/linked_list.o ${TEST_BUILD_DIR}/test_linked_list.o -o ${TEST_BUILD_DIR}/test_linked_list

test_run_linked_list:
	@${TEST_BUILD_DIR}/test_linked_list

# Linked List Simple
${BUILD_DIR}/linked_list_simple.o:
	${CC} ${CCFLAGS} -c ${SRC_DIR}/linked_list_simple.c -o ${BUILD_DIR}/linked_list_simple.o

test_build_linked_list_simple:
	${CC} ${CCFLAGS} -I ${INC_DIR} -c ${TEST_DIR}/test_linked_list_simple.c -o ${TEST_BUILD_DIR}/test_linked_list_simple.o
	${CC} ${CCFLAGS} ${BUILD_DIR}/linked_list_simple.o ${TEST_BUILD_DIR}/test_linked_list_simple.o -o ${TEST_BUILD_DIR}/test_linked_list_simple

test_run_linked_list_simple:
	@${TEST_BUILD_DIR}/test_linked_list_simple

# TODO
# Dynamic Array
# Stack (Array)
# Queue (Array)
#
# List (Simply Linked)
# List (Doubly Linked)
# Stack (List)
# Queue (List)
#
# Graph (Undirected)
# Graph (Directed)
#
# Disjoint Sets
