import subprocess as sp

def main():
    while True:
        try:
            i: str = input(">> ")
        except KeyboardInterrupt:
            break
        sp.run(["./lambda", i])
    
if __name__ == "__main__":
    _ = main()
