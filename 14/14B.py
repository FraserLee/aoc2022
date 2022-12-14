# I've got 2 finals tomorrow so I'm just throwing something quick together in
# python. Back to haskell later.

world = set()

def lookup(x, y):
    return (x, y) in world or y >= max_y + 2

def draw():
    min_x, max_x = min([c[0] for c in world]), max([c[0] for c in world])
    min_y, max_y = min([c[1] for c in world]), max([c[1] for c in world])

    for y in range(min_y - 1, max_y + 3):
        for x in range(min_x - 1, max_x + 2):
            print("#" if lookup(x, y) else ".", end="")
        print()



# for line in open("sample.txt", "r").read().splitlines():
for line in open("input.txt", "r").read().splitlines():
    coords = [c.split(",") for c in line.split(" -> ")]
    coords = [[int(c[0]), int(c[1])] for c in coords]
    print(coords)

    for i in range(len(coords) - 1):
        c1 = coords[i]
        c2 = coords[i + 1]

        min_x, max_x = min(c1[0], c2[0]), max(c1[0], c2[0])
        min_y, max_y = min(c1[1], c2[1]), max(c1[1], c2[1])

        if min_x == max_x:
            for y in range(min_y, max_y + 1):
                world.add((min_x, y))
        else:
            for x in range(min_x, max_x + 1):
                world.add((x, min_y))

draw()

# honestly this code is gross. I'm just going for minimum time before I can get
# back to studying

max_y = max([c[1] for c in world])

def sandpos(x, y):
    targets = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
    for t in targets:
        if not lookup(t[0], t[1]):
            return t
    return None


n = 0 
while not lookup(500, 0):
    sand_x = 500
    sand_y = 0


    while sand_y < max_y + 5:
        new_pos = sandpos(sand_x, sand_y)
        # print(sand_x, sand_y, new_pos)
        if new_pos is None:
            world.add((sand_x, sand_y))
            break
        else:
            sand_x, sand_y = new_pos

    n += 1

print(n)

            
