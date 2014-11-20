# calculates shortest paths within graph passed in as adjacency list

from collections import defaultdict

def import_file():
    res = {}
    with open('/home/tom/Documents/lisp/dijkstraData.txt','r') as f:
        for line in f.readlines():
            split_line = [x for x in line.strip(' \t\n\r').split()]
            res[int(split_line[0])] = [x.split(',') for x in split_line[1:]]
    return res

def dijkstra(graph, start=1):
    processed = set()
    unvisited = {x for x in range(1, 201)} # 201
    distance = {}
    for zxy in range(1, 201):
        distance[zxy] = 1000000
    distance[start] = 0
    previous = {}

    current = start

    while len(unvisited) > 0:
        neighbours = graph[current]
        for n in neighbours:
            alt = distance[current] + int(n[1])
            if alt < distance[int(n[0])]:
                distance[int(n[0])] = alt
                previous[int(n[0])] = current

        unvisited.remove(current)

        foodict = {k: v for k, v in distance.items() if k in unvisited}
        if foodict:
            current = min(foodict, key=foodict.get)

    return distance, previous

output = dijkstra(import_file())[0]
for x12 in [7, 37, 59, 82, 99, 115, 133, 165, 188, 197]:
    print output[x12]
