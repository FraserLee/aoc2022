# it's late, I'm busy - Hence using python for part B instead of haskell.
file = open("input.txt", "r").read()

map = [[int(c) for c in line] for line in file.split("\n")]

width = len(map[0])
height = len(map) - 1

res = 0

for x in range(width):
    for y in range(height):

        h = map[y][x]

        right = 0
        for i in range(x + 1, width):
            right += 1
            if map[y][i] >= h: break

        down = 0
        for i in range(y + 1, height):
            down += 1
            if map[i][x] >= h: break

        left = 0
        for i in range(x - 1, -1, -1):
            left += 1
            if map[y][i] >= h: break

        up = 0
        for i in range(y - 1, -1, -1):
            up += 1
            if map[i][x] >= h: break

        res = max(res, right * down * left * up)

print(res)
