def rand_str():
    import sys
    import random
    if(len(sys.argv)>1):
        n, m=int(sys.argv[1]), int(sys.argv[2])
    else:
        n, m=map(int, sys.stdin.readline().split())
    str1='123457890qwertyuiop[]asdfghjkl;zxcvbnm,/.'
    for _ in range(n):
        for _ in range(m):
            print(str1[random.randint(0, len(str1)-1)], end='')
        print()
