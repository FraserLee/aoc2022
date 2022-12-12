import heapq
lines = open("input.txt", "r").read().splitlines()
# lines = open("sample.txt", "r").read().splitlines()

grid = [[ 0 if c == 'S' else \
        (25 if c == 'E' else \
        ord(c) - ord('a')) for c in line] for line in lines]

width = len(grid[0])
height = len(grid)

starts = []
start = (0, 0)
end = (0, 0)

for y in range(len(lines)):
    for x in range(len(lines[0])):
        match lines[y][x]:
            case 'S': start = (x, y)
            case 'E': end = (x, y)
            case 'a': starts.append((x, y))

def dist(sx, sy):
    # breadth first search from start to end.
    # each element in the queue is a tuple of (distance, x, y)
    # (python's heapq just compares the first element when confronted with a tuple)

    queue = [(0, sx, sy)]
    visited = set()

    while queue:
        dist, x, y = heapq.heappop(queue)

        if x == end[0] and y == end[1]: return dist

        if (x, y) in visited: continue
        visited.add((x, y))

        for dx, dy in ((0, 1), (0, -1), (1, 0), (-1, 0)):
            nx, ny = x + dx, y + dy

            if 0 <= nx < width and \
               0 <= ny < height and \
               (grid[ny][nx] - grid[y][x]) <= 1:
                heapq.heappush(queue, (dist + 1, nx, ny))

    return float('inf')

print('PART A', dist(start[0], start[1]))
print('PART B', min([dist(sx, sy) for sx, sy in starts]))
