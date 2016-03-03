.PHONY: all objects tests tests_build tests_run tests_announce clean

CC=clang
CCFLAGS=-std=c11 -Wall -Wextra -Wpedantic
SRC_DIR=./src
INC_DIR=./src
TEST_DIR=./src/tests
BUILD_DIR=./build
TEST_BUILD_DIR=./build/tests

all: objects

objects: ${BUILD_DIR} \
	${BUILD_DIR}/element.o

tests: objects tests_build tests_run

tests_build: ${TEST_BUILD_DIR} \
	element_test_build

tests_run: tests_announce \
	element_test_run

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

element_test_build:
	${CC} ${CCFLAGS} -I ${INC_DIR} -c ${TEST_DIR}/test_element.c -o ${TEST_BUILD_DIR}/test_element.o
	${CC} ${CCFLAGS} ${BUILD_DIR}/element.o ${TEST_BUILD_DIR}/test_element.o -o ${TEST_BUILD_DIR}/test_element

element_test_run:
	@${TEST_BUILD_DIR}/test_element

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
