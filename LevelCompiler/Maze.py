import sys
from collections import deque

# restricted directions
# 1 up
# 2 right
# 4 down
# 8 left

UP = 1
RIGHT = 2
DOWN = 4
LEFT = 8

mazeLines = open(sys.argv[1]).readlines()
theseus = (int(mazeLines[0].split()[0]) - 1, int(mazeLines[0].split()[1]) - 1)
minotaur = (int(mazeLines[1].split()[0]) - 1, int(mazeLines[1].split()[1]) - 1)
escape = (int(mazeLines[2].split()[0]) - 1, int(mazeLines[2].split()[1]) - 1)
maze = [[int(i, 16) for i in line[:-1]] for line in mazeLines[3:]]

def is_position_valid(of, to):
  x1, y1 = of
  x2, y2 = to
  if 0 <= x2 < len(maze[0]):
    if 0 <= y2 < len(maze):
      #print("  checking restrictions at", of, maze[y1][x1])
      if ((y2 > y1 and maze[y1][x1] & DOWN == 0)
         or (y2 < y1 and maze[y1][x1] & UP == 0)
         or (x2 < x1 and maze[y1][x1] & LEFT == 0)
         or (x2 > x1 and maze[y1][x1] & RIGHT == 0)
         or (of == to)):
        #print("    ", of, to, "accepted")
        return True
      else:
        #print("    ", of, to, "movement not allowed")
        return False
    else:
      #print("  ", of, to, "y2 outside of maze")
      return False
  else:
    #print("  ", of, to, "x2 outside of maze")
    return False

def next_theseus_positions(position):
  x, y = position
  states = []

  for dx, dy in [(1, 0), (0, 1), (-1, 0), (0, -1), (0, 0)]:
    nx = x + dx
    ny = y + dy

    if is_position_valid(position, (nx, ny)):
      states.append((nx, ny))

  return states

def move_towards(m, t):
  return +1 if m < t else 0 if m == t else -1

def next_minotaur_positions(theseus, minotaur):
  tx, ty = theseus
  mx, my = minotaur

  for _ in range(2):
    # horizontal
    dx = move_towards(mx, tx)
    if dx != 0 and is_position_valid((mx, my), (mx + dx, my)):
      mx += dx
      continue

    # vertical
    dy = move_towards(my, ty)
    if dy != 0 and is_position_valid((mx, my), (mx, my + dy)):
      my += dy
      continue

  return (mx, my)

def next_states(state):
  t, m = state

  states = []
  for new_t in next_theseus_positions(t):
    #print("theseus moves to", new_t)
    new_m = next_minotaur_positions(new_t, m)
    if new_t == new_m:
      #print("minotaur catches up")
      continue

    #print("success, now", new_t, new_m)
    states.append((new_t, new_m))
  return states

def bfs(starting_state, stop_condition):
  queue = deque([starting_state])
  discovered = {starting_state: None}

  while len(queue) != 0:
    current = queue.popleft()
    #print("current:", current)
    if stop_condition(current):
      #print("Solution found!")
      path = [current]
      while discovered[current] is not None:
        current = discovered[current]
        path.append(current)

      prev = None
      i = 0
      for coordinate in reversed(path):
        if prev is not None:
          px, py = prev
          (pxn, pyn), _ = coordinate
          if px < pxn:
            print(i, coordinate, "right")
          elif px > pxn:
            print(i, coordinate, "left")
          elif py > pyn:
            print(i, coordinate, "up")
          elif py < pyn:
            print(i, coordinate, "down")
          else:
            print(i, coordinate, "stay")
        else:
          print(i, coordinate)
        prev, _ = coordinate
        i = i + 1

      return

    for next_state in next_states(current):
      if next_state not in discovered:
        queue.append(next_state)
        discovered[next_state] = current

  print("No solution found!")

for y, row in enumerate(maze):
  for x, i in enumerate(row):
    print(i, end='')
  print()

print("theseus", theseus)
print("escape", escape)
print("minotaur", minotaur)
bfs(
  (theseus, minotaur),
  lambda state: state[0] == escape)
