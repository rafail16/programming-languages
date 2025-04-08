import sys
from collections import deque

def moving(other1, other2):
    if other1[0] < other2[0]:
        return 'U'
    elif other1[0] > other2[0]:
        return 'D'
    elif other1[1] > other2[1]:
        return 'R'
    else: 
        return 'L'
    

def replace_at_index(tup, ix, val):
    return tup[:ix] + (val,) + tup[ix+1:]

def solve(rows, cols, inputs):
    def getNeighbor(x, y):
        neighbors = deque() 
        if x != rows-1 and inputs[x+1][y] != 'X':
            neighbors.append((x+1, y))  # down
        if y != 0 and inputs[x][y-1] != 'X':
            neighbors.append((x, y-1))  # left
        if y != cols-1 and inputs[x][y+1] != 'X':
            neighbors.append((x, y+1))  # right
        if x != 0 and inputs[x-1][y] != 'X':
            neighbors.append((x-1, y))  # up
        return neighbors
    #global matrix
    matrix = [[None for x in range(cols)] for y in range(rows)]
    coronaQ = deque()
    sotirisQ = deque()
    airportQ = deque()
    usedAirports = False
    
    for y in range(rows):
        for x in range(cols):
            c = inputs[y][x]
            if c == 'S':
                matrix[y][x] = (' ', 0, float("inf"), False)
                sotirisQ.append((y, x))
            elif c == 'A':
                matrix[y][x] = (' ', float("inf"), float("inf"), False)
                airportQ.append((y, x))
            elif c == 'W':
                matrix[y][x] = (' ', float("inf"), 0, False)
                coronaQ.append((y, x))
            elif c == 'T':
                matrix[y][x] = (' ', float("inf"), float("inf"), False)
                safeSquare = (y, x, matrix[y][x])
            else:
                matrix[y][x] = (' ', float("inf"), float("inf"), False)
    flag = 0
    #corona fill
    while coronaQ:
        y, x = coronaQ.popleft()
        temp = matrix[y][x]
        time = temp[2]
        if not temp[3]:
            matrix[y][x] = replace_at_index(matrix[y][x], 3, True)
            #if first time to visit airport visit all airports
            if inputs[y][x] == 'A' and not usedAirports:
                usedAirports = True
                firstA = (y, x, time + 5)
            if usedAirports and flag == 0:
                if firstA[2] < time or not coronaQ:
                    flag = 1
                    for w, z in airportQ:
                        if matrix[w][z][2] == float("inf"):
                            matrix[w][z] = replace_at_index(matrix[w][z], 2, firstA[2])
                            coronaQ.append((w, z))
                        elif matrix[w][z][2] > firstA[2]:
                            matrix[w][z] = replace_at_index(matrix[w][z], 3, False)
                            matrix[w][z] = replace_at_index(matrix[w][z], 2, firstA[2])
                            coronaQ.append((w, z))
            #flood fill neighbors
            for w, z in getNeighbor(y, x):
                if matrix[w][z][2] == float("inf"):
                    matrix[w][z] = replace_at_index(matrix[w][z], 2, time + 2)
                    coronaQ.append((w, z))
                elif matrix[w][z][2] > time + 2:
                    matrix[w][z] = replace_at_index(matrix[w][z], 3, False)
                    matrix[w][z] = replace_at_index(matrix[w][z], 2, time + 2)
                    coronaQ.append((w, z))
    
    safeTime = 0
    #sotiris movement
    while sotirisQ:
        y, x = sotirisQ.popleft()
        temp = matrix[y][x]
        time = temp[1]
        if inputs[y][x] == 'T':
            safeTime = time
            break
        for w, z in getNeighbor(y, x):
            if matrix[w][z][1] == float("inf") and matrix[w][z][2] > time + 1:
                matrix[w][z] = replace_at_index(matrix[w][z], 1, time + 1)
                matrix[w][z] = replace_at_index(matrix[w][z], 0, (y, x))
                sotirisQ.append((w, z))
    '''sot = [[None for x in range(cols)] for y in range(rows)]
    for i in range(rows):
        for j in range(cols):
            sot[i][j] = matrix[i][j][1]
    print(sot)
    cor = [[None for x in range(cols)] for y in range(rows)]
    for i in range(rows):
        for j in range(cols):
            cor[i][j] = matrix[i][j][2]
    print(cor)'''
    #if sotiris wasn't able to get to home
    if safeTime == 0:
        string = "IMPOSSIBLE\n"
    #sotiris got to home
    else:
        moves = deque()
        y, x, temp = safeSquare
        #go to the end
        while not inputs[y][x] == 'S':
            temp = matrix[y][x]
            moves.appendleft(moving((y, x), temp[0]))
            (y, x) = temp[0]
        string = str(safeTime) + '\n' + ''.join(moves) + '\n'
    return string
    
if __name__ == '__main__':
    filename = sys.argv[1]
    with open(filename) as f:
        inputs = [x.strip() for x in f.readlines()]
        # inputs[row][column]
        inputs = list(map(list,inputs))
        # rows = number of rows
        rows = len(inputs)
        # cols = number of columns
        cols = len(inputs[0])
        solution = solve(rows, cols, inputs)
        print(solution) 