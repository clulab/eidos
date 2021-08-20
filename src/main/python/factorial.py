import sys

def factorial(n):
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

def main():
    if len(sys.argv) != 2:
        print('ERROR: One argument is required.')
        print('Usage:')
        print('\tpython factorial.py <n>')
        sys.exit(1)
    n = int(sys.argv[1])
    print(factorial(n))

if __name__ == "__main__":
    main()
