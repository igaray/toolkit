import os

def main():
  # os.mkdir("./shadow/")
  # Obtain all program source files.
  for path, subdirs, files in os.walk("."):
    if os.stat(path).is_dir(path):
      # os.mkdir("./shadow" + path)
      print(path)
      pass
    else:
      pass
      # f = open(path + , "w")

if __name__ == "__main__":
  main()