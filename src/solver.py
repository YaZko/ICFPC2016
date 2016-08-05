from fractions import Fraction

# Parsing

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

def join_aux(acc,l1,l2):
    print(acc)
    if len(l1) == 1:
        return acc
    # return acc.extend(l2)
    elif l2 == []:
        return acc.extend(l1)
    else:
        e1 = Edge(l1[0],l1[1])
        e2 = Edge(l2[0],l2[1])
        if e1.intersect(e2):
            print('intersection {} with {}'.format(e1,e2))
            p = e1.compute_intersection(e2)
            acc.append(p)
            acc.append(l2[1])
            join_aux(acc,l2[1:],[p] + l1[1:])
        else:
            # Warning: weird case not handled when the first intersection is not the right one, you actually want the leftmost one on the projection on the folding line
            # SUPER WARNING: we want to search further to check intersections
            acc.append(l1[1])
            join_aux(acc,l1[1:],l2)

# Should return an object Problem
def parse(fname):

    lines = [line.rstrip('\n') for line in open(fname)]

    polys = []

    nb_poly = int(lines[0])
    current = 1

    for _ in range(nb_poly):
        # We parse each polygon in the silhouette
        nb_vert = int(lines[current])
        current += 1
        verticies = []

        for _ in range(nb_vert):
            # We parse each vertex of the current polygon
            v = parse_point(lines[current])
            verticies.append(v)
            current += 1
        polys.append(Poly(verticies))

    nb_seg = int(lines[current])
    current += 1
    skel = []

    for _ in range(nb_seg):
        (p1,p2) = lines[current].split()
        v = Edge(parse_point(p1),parse_point(p2))
        skel.append(v)
        current += 1

    return Problem(polys,skel)


# Helping functions

# Compute the determinant of two vectors given by their four coordinates
def det(x1,y1,x2,y2):
    return x1 * y2 - x2 * y1

# Compute if a polygone given by a list of vertexes is positive
def is_positive(verts):
    sum = 0
    for i in range(len(verts) - 1):
        sum += (verts[i+1].x - verts[i].x) * (verts[i+1].y - verts[i].y)
        sum += (verts[0].x - verts[len(verts) - 1].x) * (verts[0].y - verts[len(verts)-1].y)
    return sum >= 0

class Point():
    def __init__(self,x,y):
        self.x = x
        self.y = y

    def __repr__(self):
        return '({},{})'.format(self.x,self.y)

    def __str__(self):
        return '({},{})'.format(self.x,self.y)

    # Test whether a point is to moved if a folding occur toward the edge in direction dir
    # dir = 1 if folding to the right, -1 if folding to the left
    def is_moved(self,edge,dir):
        (x1,y1) = (self.x-edge.p1.x,self.y-edge.p1.y)
        (x2,y2) = (self.x-edge.p2.x,self.y-edge.p2.y)
        det = x1 * y2 - x2 * y1
        # If det is negative, the point is on the right of the edge (the sinus is negative)
        return det * dir > 0

    # Seen as a vector, computes its scalar product
    def scalar(self,p):
        return self.x * p.x + self.y * p.y

    def vect_ortho(self):
        return Point(self.y,-self.x)

    # Compute the orthogonal symmetric of a point by an edge
    def move_to(self,edge):
        d = edge.norm_sq()
        v = edge.to_vect()
        n = v.vect_ortho()
        resx = self.x - 2*self.scalar(n)/d*n.x + 2 * edge.p1.scalar(n)/d*n.x
        resy = self.y - 2*self.scalar(n)/d*n.y + 2 * edge.p1.scalar(n)/d*n.y
        return Point(resx,resy)

    # Computes if three points are counter clock wise
    def CCW(self,a,b):
        x1 = a.x - self.x
        x2 = b.x - a.x
        y1 = a.y - self.y
        y2 = b.y - a.y
        return det(x1,y1,x2,y2) > 0

class Edge():
    def __init__(self,p1,p2):
        self.p1 = p1
        self.p2 = p2

    def __str__(self):
        return ('(' + str(self.p1) + ' -> ' + str(self.p2) + ')')

    def __repr__(self):
        return ('(' + str(self.p1) + ' -> ' + str(self.p2) + ')')

    # Tests whether an edge intersect a line of equation y = line
    def intersect_line(self,line):
        y1,y2 = self.p1.y,self.p2.y
        if y1 > y2:
            return line <= y1 and line >= y2
        else:
            return line <= y2 and line >= y1

    # Tests whether two edges intersect each other
    def intersect(self,edge):
        a,b,c,d = self.p1,self.p2,edge.p1,edge.p2
        if a.CCW(c,d) == b.CCW(c,d):
            return False
        elif a.CCW(b,c) == a.CCW(b,d):
            return False
        else:
            return True

    def compute_intersection(self,edge):
        a,b,c,d = self.p1,self.p2,edge.p1,edge.p2
        deter = Fraction(1,det(b.y-a.y,d.y-c.y,a.x-b.x,c.x-d.x))
        px = ((c.x-d.x)*(a.x*(b.y - a.y)+a.y*(a.x-b.x)) +
              (c.y-d.y)*(c.x*(d.y-c.y)+c.y*(c.x-d.x))) * deter
        py = ((b.x-a.x)*(a.x*(b.y - a.y)+a.y*(a.x-b.x)) +
              (b.y-a.y)*(c.x*(d.y-c.y)+c.y*(c.x-d.x))) * deter
        return Point(px,py)

    # Computes the vector defined by the edge
    def to_vect(self):
        return Point(self.p2.x-self.p1.x,self.p2.y-self.p1.y)

    # Compute the square of the norm of the segment
    def norm_sq(self):
        return (self.p2.x - self.p1.x) * (self.p2.x - self.p1.x) + (self.p2.y - self.p1.y) * (self.p2.y - self.p1.y)

class Poly():
    # A polygon, is a list of verticies, which are pairs of points which are pairs of fractions.
    # We also compute an equivalent representation in terms of its list of edges for convenience
    def __init__(self,verts):
        self.verticies = verts

        self.edges = []
        for i in range(len(verts)-1):
            self.edges.append(Edge(verts[i],verts[i+1]))
        self.edges.append(Edge(verts[len(verts)-1],verts[0]))

        self.positive = is_positive(verts)

    def print_edges(self):
        res = '['
        for e in self.edges:
            res = res + str(e) + '\n'
        res = res + ']'
        print(res)

    def __str__(self):
        res = '['
        for v in self.verticies:
            res = res + str(v) + ' '
        res = res + ']'
        return res

    def __repr__(self):
        res = '['
        for v in self.verticies:
            res = res + str(v) + ' '
        res = res + ']'
        return res

    def is_positive(self):
        return is_positive(self.verticies)

    # Computes whether a point is inside the polygon by throwing an horizontal ray of ordinate y
    def is_inside(self,y):
        nb = 0
        for e in self.edges:
            if e.intersect_line(y):
                nb += 1
        return nb % 2 == 1

    # Computes the list of vertexes which are to be moved by a folding along an edge.
    # Removes them and returns their 
    def are_moved(self,edge,dir):
        res = {}
        for vert in self.verticies:
            if vert.is_moved(edge,dir):
                self.verticies.remove(vert)
                res[vert] = vert.move_to(edge)
        return res

    # Computes the union of two polygones which share a common edge
    def join(self,poly):
        l1 = self.verticies
        l2 = poly.verticies




class Problem():

    def __init__(self,sil,skel):
        self.skeleton = skel
        self.silhouette = sil

    def __str__(self):
        res = 'Skel:\n['
        for v in self.skeleton:
            res = res + str(v) + ' '
        res = res + ']\nSilhouette :\n'
        for p in self.silhouette:
            res = res + str(p)
        res = res + ']'
        return res

    def __repr__(self):
        res = 'Skel:\n['
        for v in self.skeleton:
            res = res + str(v) + ' '
        res = res + ']\nSilhouette :\n'
        for p in self.silhouette:
            res = res + str(p)
        res = res + ']'
        return res


def solve(data):
    return "fail"

class Sol():

    def __init__(points,facets,dest):
        self.points = points
        self.facets = facets
        self.dest = dest

# We assume the problem to be reduce to a single, convex polygon

# Given a current state of the sheet, an edge and the direction of the folding, fold the sheet upon this edge.
def fold(edge,current,dir):
    new_points = current.are_moved(edge,dir)

# Testing
pb = parse('../pb/ex')
poly = pb.silhouette[0]
e = Edge(Point(Fraction(1),Fraction(1)),Point(Fraction(2),Fraction(2)))
p = Point(Fraction(0),Fraction(0))

p00 = Point(Fraction(0),Fraction(0))
p01 = Point(Fraction(0),Fraction(1))
p02 = Point(Fraction(0),Fraction(2))
pm11 = Point(Fraction(-1),Fraction(1))
p11 = Point(Fraction(1),Fraction(1))
p20 = Point(Fraction(2),Fraction(0))
p22 = Point(Fraction(2),Fraction(2))
p31 = Point(Fraction(3),Fraction(1))

ed1 = Edge(pm11,p31)
ed2 = Edge(p02,p00)


e1 = Edge(p1,p2)
e2 = Edge(p3,p4)
e3 = Edge(p1,p4)
e4 = Edge(p2,p3)

l1 = [p02,pm11,p31,p22]
l2 = [p02,p00,p20,p22]
