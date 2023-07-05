import os

def create_text_files():
    """
    """
    for root, _dirs, files in os.walk("."):
        for filename in files:
            base, ext = os.path.splitext(filename)
            if ".mp4" == ext:
                print("Creating file '{}/{}.txt'".format(root, base))
                os.system("touch '{}/{}.txt'".format(root, base))


def main():
    """
    """
    print("Running")
    create_text_files()
    print("Completed")
 

if __name__ == "__main__":
    main()
