n = 14
count = [0] * 26
file = open("input.txt", "r").read()

def incr(c):
    c = ord(c) - ord('a')
    count[c] += 1
    return 1 if count[c] == 1 else 0

def decr(c):
    c = ord(c) - ord('a')
    count[c] -= 1
    return 1 if count[c] == 0 else 0

total = 0

for i in range(n):
    total += incr(file[i])

for i in range(n, len(file)):
    total -= decr(file[i - n])
    total += incr(file[i])
    if total == n:
        print(i + 1)
        exit()
