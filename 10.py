def get_primes(m, n):
    bool_arr = [True for i in range(n-1)]
    for i in range(2, n+1):
        for j in [i*i + k for k in range(0, n-i*i+1, i)]:
            bool_arr[j-2] = False
    primes = []
    for i in range(m-2, n-1):
        if bool_arr[i]: primes.append(i+2)
    return primes

m, n = map(int, input().split())
primes = get_primes(m, n)
if len(primes) == 0:
    print('Absent')
else:
    for i in primes: print(i)
