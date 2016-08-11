#!/usr/local/bin/python3

import sys

def shift(line):
  for i in range(len(line) - 1):
    line = line[-1:] + line[:-1]
    print(line)

def main():
  input = sys.stdin.readline()
  shift(input[:-1])

if __name__ == "__main__":
  main()
