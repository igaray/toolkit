import os
import sys

def main():
  if len(sys.argv) == 3:
    PREFIX = sys.argv[2]
  else:
    PREFIX = "./treeshadow"
  os.makedirs(PREFIX, exist_ok=True)

  for path, subdirs, files in os.walk(sys.argv[1]):
    for directoryname in subdirs:
      print("D", PREFIX + path + "/" + directoryname)
      os.makedirs(PREFIX + path + "/" + directoryname, exist_ok=True)
    for filename in files:
      print("    F", PREFIX + path + "/" + filename)
      f = open(PREFIX + path + "/" + filename, "w")
      f.close()

if __name__ == "__main__":
  main()