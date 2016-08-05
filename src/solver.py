from fractions import Fraction
# Solving a convex problem

class Point():
    def __init__(self,x,y):
        self.x = x
        self.y = y

    def __str__(self):
        return '({},{})'.format(self.x,self.y)

    def get_x(self):
        return self.x

    def get_y(self):
        return self.y

class Vert():
    def __init__(self,p1,p2):
        self.p1 = p1
        self.p2 = p2

    def __str__(self):
        return ('(' + str(self.p1) + str(self.p2) + ')')

    def origin(self):
        return self.p1

    def dest(self):
        return self.p2

# class Poly():

#     def __init__(self,vertices):
#         self.vert = []
#         for ((x1,y1),(x2,y2)) in vertices:
#             self.vert.append(Vert(Point(x1,y1)),Point(x2,y2))

class Facet():
    # A polygone, right now just a list of verticies, which are pairs of points which are pairs of fractions
    def __init__(self,verts):
        self.verticies = verts

    def __str__(self):
        res = '['
        for v in self.verticies:
            res = res + str(v) + ' '
        res = res + ']'
        return res

class Problem():

    def __init__(self,verticies):
        self.skeleton = []
        self.silhouette = Facet(verticies)

    def __str__(self):
        return str(self.silhouette) 

def parse_frac(s):
    p = s.split('/')
    if len(p) == 1:
        return Fraction(int(p[0]))
    else:
        return Fraction(int(p[0]),int(p[1]))

def parse_point(s):
    f1,f2 = s.split(',')
    f1 = parse_frac(f1)
    f2 = parse_frac(f2)
    return Point(f1,f2)

# Should return an object Problem
def parse(fname):

    lines = [line.rstrip('\n') for line in open(fname)]

    facets = []

    nb_poly = int(lines[0])
    current = 1

    for _ in range(nb_poly):
        nb_vert = int(lines[current])
        current += 1
        verticies = []

        for _ in range(nb_vert - 1):
            v = Vert(parse_point(lines[current]),parse_point(lines[current]))
            print('v : {}'.format(v))
            verticies.append(v)
            current += 1
        facets.append(Facet(verticies))

    return Problem(verticies)
