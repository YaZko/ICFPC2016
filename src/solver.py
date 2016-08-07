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

    # WARNING: assume only one polygone
    return Problem(polys[0].verticies,skel)


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

def rotate_to_fit(l,p):
    idx = 0
    for (i,pl) in enumerate(l):
        if pl == p:
            idx = i
            break
        else:
            pass
    return l[idx:] + l[:idx]

def join_aux(acc,l1,l2):
    # print(acc)
    if len(l1) == 1:
        # print('outing from join_aux with {}'.format(acc))
        return acc
    # return acc.extend(l2)
    elif l2 == []:
        acc.extend(l1)
        # print('outing from join_aux with {}'.format(acc))
        return acc
    else:
        e1 = Edge(l1[0],l1[1])
        e2 = Edge(l2[0],l2[1])
        if e1.intersect(e2):
            p = e1.compute_intersection(e2)
            # print('intersection {} with {} intersects at {}'.format(e1,e2,p))
            acc.append(p)
            acc.append(l2[1])
            return join_aux(acc,l2[1:],[p] + l1[1:])
        elif e1.col(e2):
            p = e1.compute_col_intersection(e2)
            acc.append(p)
            acc.append(l2[1])
            return join_aux(acc,l2[1:],[p] + l1[1:])
        else:
            # Warning: weird case not handled when the first intersection is not the right one, you actually want the leftmost one on the projection on the folding line
            # SUPER WARNING: we want to search further to check intersections
            acc.append(l1[1])
            return join_aux(acc,l1[1:],l2)
    # Computes the union of two polygones which share a common edge

def get_downmost(l1,l2):
    score = l1[0].y
    best = (0,True)

    for (i,p) in enumerate(l1):
        if p.y < score:
            score = p.y
            best = (i,True)
        elif p.y == score:
            if p.x < l1[best[0]].x:
                best = (i,True)
        else:
            pass

    for (i,p) in enumerate(l2):
        if p.y < score:
            score = p.y
            best = (i,False)
        elif p.y == score:
            if best[1] == True:
                if p.x < l1[best[0]].x:
                    best = (i,False)
            else:
                if p.x < l2[best[0]].x:
                    best = (i,False)
        else:
            pass

    if best[1]:
        return l1[best[0]]
    else:
        return l2[best[0]]

def get_edges(l):
    edges = [Edge(l[i],l[i+1]) for i in range(len(l) - 1)]
    edges.append(Edge(l[len(l)-1],l[0]))
    return edges

def find_best(p,points):
    if len(points) == 1:
        return points[0]
    else:
        best = points[0]
        for point in points[1:]:
            if p.CCW(point,best):
                best = point
        return best

# def my_comp(p0,p1,p,p_):
#     d1 = Edge(p0,p1).to_vect().scalar(Edge(p0,p).to_vect())
#     d2 = Edge(p0,p1).to_vect().scalar(Edge(p0,p_).to_vect())
#     return d1 <= d2

def my_comp(p0,p):
    d = Edge(p0,p).to_vect().scalar(Edge(p0,p).to_vect())
    # print('comp {} with {} gives d = {}'.format(p0,p,d))
    return d

def ordonnate(l):
    to_sort = l[2:]
    to_sort = sorted(to_sort,key = lambda p: my_comp(l[0],p))
    return [l[0]] + to_sort + [l[1]]

def join(l1,l2):

    # print('joining {} with {}'.format(l1,l2))
    start = get_downmost(l1,l2)
    # print('lowest : {}'.format(start))
    ed1 = get_edges(l1)
    ed2 = get_edges(l2)

    d = {e:[e.p1,e.p2] for e in ed1 + ed2}
    for e1 in ed1:
        for e2 in ed2:
            if e1.intersect(e2):
                p = e1.compute_intersection(e2)
                d[e1].append(p)
                d[e2].append(p)

    # print('d :')
    # for (a,b) in d.items():
        # print('{} : {}'.format(a,b))
    # print('End d')

    ed = []
    for e in ed1 + ed2:
        # print('e = {}'.format(e))
        l = d[e]
        # print('l = {}'.format(l))
        if len(l) == 2:
            ed.append(e)
        else:
            l = ordonnate(l)
            # print('l ordonne = {}'.format(l))
            for i in range(len(l) - 1):
                ed.append(Edge(l[i],l[i+1]))

    # print('ed = {}'.format(ed))

    succ = {}
    for e in ed:
        if e.p1 in succ:
            succ[e.p1].append(e.p2)
        else:
            succ[e.p1] = [e.p2]
        # succ[e.p2].append(e.p1)

    # print('Initial succ :')
    # for (a,b) in succ.items():
    #     print('{} : {}'.format(a,b))
    # print('End succ')

    # for e1 in ed1:
    #     for e2 in ed2:
    #         if e1.intersect(e2):
    #             p = e1.compute_intersection(e2)
    #             print('e1 = {} intersects with e2 = {} at point {}'.format(e1,e2,p))
    #             # print('succ[{}] = {}'.format(e1.p1,succ[e1.p1]))
    #             try:
    #                 succ[e1.p1].remove(e1.p2)
    #             except:
    #                 pass
    #             # print('succ[{}] = {}'.format(e1.p1,succ[e1.p1]))
    #             # print('succ[{}] = {}'.format(e2.p1,succ[e2.p1]))
    #             try:
    #                 succ[e2.p1].remove(e2.p2) 
    #             except:
    #                 pass
    #             # print('succ[{}] = {}'.format(e2.p1,succ[e2.p1]))
    #             succ[e1.p1].append(p)
    #             succ[e2.p1].append(p)
    #             # print('succ[{}] = {}'.format(e1.p1,succ[e1.p1]))
    #             # print('succ[{}] = {}'.format(e2.p1,succ[e2.p1]))
    #             succ[p] = [e1.p2,e2.p2]
    #             # print('succ[{}] = {}'.format(p,succ[p]))


    # print('Resulting succ :')
    # for (a,b) in succ.items():
        # print('{} : {}'.format(a,b))
    # print('End succ')

    current = start
    union = [current]
    # print('looping with current = {}, succ = {}'.format(current,succ))
    i = 1
    while True and i < 10:
        i += 1
        # print('finding best of {} among {}'.format(current,succ[current]))
        next = find_best(current,succ[current])
        # print('next = {}'.format(next))
        if next == start:
            break
        union.append(next)
        current = next

    # print('resulting union : {}'.format(union))

    return union

class Point():
    def __init__(self,x,y):
        self.x = x
        self.y = y

    def __repr__(self):
        return '({},{})'.format(self.x,self.y)

    def __str__(self):
        return '({},{})'.format(self.x,self.y)

    def __eq__(self,p):
        return self.x == p.x and self.y == p.y

    def is_equal(self,p):
        return self.x == p.x and self.y == p.y

    def __hash__(self):
        return self.x.__hash__() + self.y.__hash__()
    
    # Test whether a point is to moved if a folding occur toward the edge in direction dir
    # dir = 1 if folding to the right, -1 if folding to the left
    def is_moved(self,edge,dir):
        (x1,y1) = (self.x-edge.p1.x,self.y-edge.p1.y)
        (x2,y2) = (self.x-edge.p2.x,self.y-edge.p2.y)
        det = x1 * y2 - x2 * y1
        # If det is negative, the point is on the right of the edge (the sinus is negative)
        return det * dir > 0

    # Return the translation of the point by the vector (encoded as a Point) p
    def translate(self,p):
        rx = self.x + p.x
        ry = self.y + p.y
        return Point(rx,ry)

    # Tests if a point fit in the initial square
    def fit_in_square(self):
        return self.x >=0 and self.x <= 1 and self.y >= 0 and self.y <= 1

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
        asx = self.x - a.x
        asy = self.y - a.y
        abx = b.x - a.x
        aby = b.y - a.y
        d = det(asx,asy,abx,aby)
        # print('ccw({}, {}, {}) return a determinant = {}'.format(self,a,b,d))
        return d < 0

class Edge():
    def __init__(self,p1,p2):
        self.p1 = p1
        self.p2 = p2

    def __str__(self):
        return ('(' + str(self.p1) + ' -> ' + str(self.p2) + ')')

    def __hash__(self):
        return hash(self.p1) + hash(self.p2)

    def __eq__(self,e):
        return self.p1 == e.p1 and self.p2 == e.p2

    def __repr__(self):
        return ('(' + str(self.p1) + ' -> ' + str(self.p2) + ')')

    # Tests whether an edge intersect a line of equation y = line
    def intersect_line(self,line):
        y1,y2 = self.p1.y,self.p2.y
        if y1 > y2:
            return line <= y1 and line >= y2
        else:
            return line <= y2 and line >= y1

    def are_parallel(self,e):
        v1 = self.to_vect()
        v2 = e.to_vect()
        return det(v1.x,v1.y,v2.x,v2.y) == 0

    # Tests whether an edge intersect a line of direction e
    def touch_or_par(self,e):
        # print('intersecting segment {} along line {}'.format(self,e))
        par = False
        touch = False
        if self.are_parallel(e):
            par = True
            if Edge(e.p1,self.p2).are_parallel(e):
                touch = True
        else:
            p = self.compute_intersection(e)
            # print('point: {}'.format(p))
            v1 = Edge(self.p1,p).to_vect()
            v2 = Edge(self.p2,p).to_vect()
            # print('v1: {} and v2 : {}'.format(v1,v2))
            touch = v1.scalar(v2) <= 0
        return (touch,par)

    # Tests whether an edge intersect a line of direction e
    def intersect_line_gen(self,e):
        # print('intersecting segment {} along line {}'.format(self,e))
        if self.are_parallel(e):
            # print('parallel')
            if Edge(e.p1,self.p2).are_parallel(e):
                return True
            else:
                return False
        else:
            p = self.compute_intersection(e)
            # print('point: {}'.format(p))
            v1 = Edge(self.p1,p).to_vect()
            v2 = Edge(self.p2,p).to_vect()
            # print('v1: {} and v2 : {}'.format(v1,v2))
            return v1.scalar(v2) <= 0

    # Tests whether two edges intersect each other
    def intersect(self,edge):
        a,b,c,d = self.p1,self.p2,edge.p1,edge.p2
        share_extr = self.p1 == edge.p1 or self.p1 == edge.p2 or self.p2 == edge.p1 or self.p2 == edge.p2
        if a.CCW(c,d) == b.CCW(c,d):
            return False
        elif a.CCW(b,c) == a.CCW(b,d):
            return False
        else:
            return not share_extr

    def compute_intersection(self,edge):
        p1,p2,p3,p4 = self.p1,self.p2,edge.p1,edge.p2
        deter = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)
        px = (p1.x * p2.y - p1.y * p2.x) * (p3.x - p4.x) - (p1.x - p2.x) * (p3.x * p4.y - p3.y * p4.x)
        py = (p1.x * p2.y - p1.y * p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x * p4.y - p3.y * p4.x)
        return Point(px / deter, py / deter)

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

        # TODO, is_positive is bugged, use CCW
        self.positive = True
        # self.positive = is_positive(verts)

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
        return res

    def __repr__(self):
        res = 'Skel:\n['
        for v in self.skeleton:
            res = res + str(v) + ' '
        res = res + ']\nSilhouette :\n'
        for p in self.silhouette:
            res = res + str(p)
        return res

class Sol():

    def __init__(self,points,facets,dest):
        self.points = points
        self.facets = facets
        self.dest = dest
        self.index = {}
        for (i,p) in enumerate(points):
            self.index[p] = i

    def __str__(self):
        return str(self.points) + '\n\n' + str(self.facets) + '\n\n' + str(self.dest) + '\n\n' + str(self.index)

    def __repr__(self):
        return str(self.points) + '\n\n' + str(self.facets) + '\n\n' + str(self.dest) + '\n\n' + str(self.index)

    def output(self,fname):
        f = open(fname,'w')
        nb = len(self.points)
        f.write('{}\n'.format(nb))
        for p in self.points:
            f.write('{},{}\n'.format(p.x,p.y))
        nb = len(self.facets)
        f.write('{}\n'.format(nb))
        for facet in self.facets:
            nb = len(facet)
            f.write('{}'.format(nb))
            for p in facet:
                f.write(' {}'.format(self.index[p]))
            f.write('\n')

        for (i,p) in enumerate(self.points):
            p = self.dest[p]
            if i == len(self.points) - 1:
                f.write('{},{}'.format(p.x,p.y))
            else:
                f.write('{},{}\n'.format(p.x,p.y))

        f.close()

# Translate a polygone defined as its list of verticies
def translate_poly(poly,v):
    res = []
    for p in poly:
        res.append(p.translate(v))
    return res

# Tests whether a polygone defined as a list of verticies fits in the initial square
def poly_in_square(poly):
    return all(v.fit_in_square() for v in poly)

# We assume the problem to be reduce to a single, convex polygon

# Computes the list of vertexes which are to be moved by a folding along an edge.
# Removes them and returns their 
def are_moved(verts,edge,dir):
    polys = [[verts[0]]]
    #print('Executing are_moved with verts = {} and edge = {} over dir = {}'.format(verts,edge,dir))

    for i in range(len(verts) - 1):
        # print('polys are : {}'.format(polys))
        # if i == len(verts) - 1:
            # j = 0
        # else:
            # j = i + 1
        j = i + 1
        e = Edge(verts[i],verts[j])
        # print('e = {}'.format(e))

        touch,par = e.touch_or_par(edge)

        if touch and not par:
            # print('Computing intersection of {} with {}'.format(e,edge))
            inter = e.compute_intersection(edge)
            if inter == e.p1:
                # print('nope')
                polys[-1].append(verts[j])
            else:
                # print('intersecting {} with {}, finding point {}'.format(e,edge,inter))
                polys[-1].append(inter)
                polys.append([])
                polys[-1].append(inter)
                if not inter.is_equal(verts[j]):
                    polys[-1].append(verts[j])
        else:
            # print('not intersecting')
            polys[-1].append(verts[j])

    # print('polys are : {}'.format(polys))

    if len(polys)>1:
        e = Edge(verts[-1],verts[0])
        # print('intersecting {} with {}'.format(e,edge))
        touch,par = e.touch_or_par(edge)
        if touch and not par:
            inter = e.compute_intersection(edge)
            if inter == polys[0][0]:
                # print('1')
                polys[-1].append(inter)
            elif inter == polys[-1][-1]:
                # print('2')
                polys[0].append(inter)
            else:
                # print('3')
                polys[-1].append(inter)
                polys[0].append(inter)
        else:
            # print('4')
            polys[0].extend(polys[-1])
            polys.pop()

    # print('After first phase, polys = {}'.format(polys))
    l1 = []
    l2 = []
    for (i,l) in enumerate(polys):

        if any(p.is_moved(edge,dir) for p in l):
            l1.append([])
            for v in l:
                new_pos = v.move_to(edge)
                l1[-1].append(new_pos)
        else:
            l2 = l2 + l

    for l in l1:
        l.reverse()
    l1_ = [elim_col(elim_doubles(l)) for l in l1]
    l2_ = elim_col(elim_doubles(l2))

    # print('Returning from are_moved with l1 = {} and l2 = {}'.format(l1,l2))
    return (l1_,l2_)

def inverse(edge,tsfs):
    p1 = edge.p1
    p2 = edge.p2
    # print('inversing for {}'.format(tsfs))
    for tsf in tsfs[::-1]:
        aux = p1.move_to(tsf)
        p1 = p2.move_to(tsf)
        p2 = aux
    return Edge(p1,p2)

def transf_point(p,tsfs,translation):
    # print('inversing for {}'.format(tsfs))
    for tsf in tsfs:
        p = p.move_to(tsf)
    return p.translate(translation)

def update_facet(verts,edge,dir,tsfs):

    # print('updating facet {} {} {} {}'.format(verts,edge,dir,tsfs))
    # print('inverting {} by {}'.format(edge,tsfs))
    edge_proper = inverse(edge,tsfs)
    # print('edge_proper : {}'.format(edge_proper))
    new_tsfs = tsfs + [edge]

    polys = [[verts[0]]]
    # print('Executing update_facet with verts = {} and edge = {} over dir = {}'.format(verts,edge_proper,dir))

    for i in range(len(verts) - 1):
        # print('polys are : {}'.format(polys))
        # if i == len(verts) - 1:
            # j = 0
        # else:
            # j = i + 1
        j = i + 1
        e = Edge(verts[i],verts[j])
        # print('e = {}'.format(e))

        touch,par = e.touch_or_par(edge_proper)

        if touch and not par:
            # print('Computing intersection of {} with {}'.format(e,edge_proper))
            inter = e.compute_intersection(edge_proper)
            if inter == e.p1:
                # print('nope')
                polys[-1].append(verts[j])
            else:
                # print('intersecting {} with {}, finding point {}'.format(e,edge_proper,inter))
                polys[-1].append(inter)
                polys.append([])
                polys[-1].append(inter)
                if not inter.is_equal(verts[j]):
                    polys[-1].append(verts[j])
        else:
            # print('not intersecting')
            polys[-1].append(verts[j])

    # print('polys are : {}'.format(polys))

    if len(polys)>1:
        e = Edge(verts[-1],verts[0])
        # print('intersecting {} with {}'.format(e,edge_proper))
        touch,par = e.touch_or_par(edge_proper)
        # if e.intersect_line_gen(edge_proper):
        if touch and not par:
            inter = e.compute_intersection(edge_proper)
            if inter == polys[0][0]:
                # print('1')
                polys[-1].append(inter)
            elif inter == polys[-1][-1]:
                # print('2')
                polys[0].append(inter)
            else:
                # print('3')
                polys[-1].append(inter)
                polys[0].append(inter)
        else:
            # print('4')
            polys[0].extend(polys[-1])
            polys.pop()

    # print('After first phase, polys = {}'.format(polys))
    res = []
    for (i,l) in enumerate(polys):
        if any(p.is_moved(edge_proper,dir) for p in l):
            res.append((elim_col(elim_doubles(l)),new_tsfs))
        else:
            res.append((elim_col(elim_doubles(l)),tsfs))

    for (i,l) in enumerate(res):
        if len(l[0]) < 3:
            res.pop(i)
    # for l in l1:
    #     l.reverse()
    # l1_ = [elim_col(elim_doubles(l)) for l in l1]
    # l2_ = elim_col(elim_doubles(l2))

    # print('Returning from update_facet with res = {}'.format(res))
    return res

def elim_doubles(poly):
    pol = []
    for i in range(len(poly)):
        if i == len(poly) - 1:
            j = 0
        else:
            j = i + 1
        if not poly[i] == poly[j]:
            pol.append(poly[i])
    return pol

def elim_col(poly):
    # print ('Cleaning {}'.format(poly))
    if len(poly) < 3:
        return poly
    else:
        pol = []
        for i in range(len(poly)):
            if i == len(poly) - 2:
                j = i + 1
                k = 0
            elif i == len(poly) - 1:
                j = 0
                k = 1
            else:
                j = i + 1
                k = i + 2
            e1 = Edge(poly[i],poly[j])
            e2 = Edge(poly[j],poly[k])
            if not e1.are_parallel(e2):
                pol.append(e1.p2)
            # else:
                # print ('Removing {} because {} and {} are parallel'.format(e1.p2,e1,e2))
        return pol

# Given a current state of the sheet, an edge and the direction of the folding, fold the sheet upon this edge.
def fold(current,edge,dir,facets_init):
    print('\n\nFolding {} along {} in direction {}'.format(current,edge,dir))
    # print('facets_init = {}'.format(facets_init))
    polys,main = are_moved(current,edge,dir)

    new_facets_init = []
    for (facet,tsfs) in facets_init:
        new_facets_init.extend(update_facet(facet,edge,dir,tsfs))

    # print('new_facets_init = {}'.format(new_facets_init))
    # print('unifying polys: {} with main: {}'.format(polys,main))
    for poly in polys:
        if main == []:
            main = poly
        elif not poly == []:
            main = join(main,poly)
        # print(main)
    # print('Resulting poly : {}'.format(main))
    return (elim_col(main),new_facets_init)

# Computes whether a point is inside the polygon by throwing an horizontal ray of ordinate y
def is_inside(main,y):
    nb = 0
    for i in range(len(main)):
        if i == len(main) - 1:
            j = 0
        else:
            j = i + 1
        e = Edge(main[i],main[j])
        if e.intersect_line(y):
            nb += 1
    return nb % 2 == 1

def is_included(main,poly):
    # print('Testing inclusion of {} in {}'.format(poly,main))
    b = True
    for e in poly:
        b = b and is_inside(main,e.y)
    return b

def solve(idx,pb):

    p00 =  Point(Fraction(0),Fraction(0))
    p10 =  Point(Fraction(1),Fraction(0))
    p01 =  Point(Fraction(0),Fraction(1))
    p11 =  Point(Fraction(1),Fraction(1))
    current = [p00,p10,p11,p01]
    verts = pb.silhouette
    # print('Goal: {}'.format(verts))

    # Computes if an initial translation is needed to fit in the square
    if poly_in_square(verts):
        trans_v = Point(0,0)
    else:
        vx = - min([p.x for p in verts])
        vy = - min([p.y for p in verts])
        trans_v = Point(vx,vy)

    # print('\nGoal: {}\n'.format(verts))

    verts = translate_poly(verts,trans_v)

    # print('\nGoal after translation: {}\n'.format(verts))

    # if not poly_in_square(verts):
    #     raise

    facets_init = [(current,[])]

    unfinished = True

    k = 0
    while unfinished:
        snapshot = current
        # print('CYCLED')
        k += 1
        if k == 10:
            raise

        for (i,_) in enumerate(verts):
            # print('current : {}'.format(current))
            # if is_included(verts,current):
            # print('finished? {} and {}'.format(verts,current))
            # print('plop : {}'.format(rotate_to_fit(current,verts[0])))
            if verts == rotate_to_fit(current,verts[0]):
                unfinished = False
                break
            else:
                if i == len(verts) - 1:
                    j = 0
                else:
                    j = i+1
                e = Edge(verts[i],verts[j])
                # print('Folding along e : {}'.format(e))
                current,facets_init = fold(current,e,-1,facets_init)

        if verts == rotate_to_fit(current,verts[0]):
            unfinished = False

        if current == rotate_to_fit(snapshot,current[0]):
            unfinished = False
            # print('Problem {} has cycled'.format(idx))
            # raise('Cycled without change')

    # print('pretty please: {}'.format(facets_init))
    facets_init = [(elim_col(elim_doubles(l[0])),l[1]) for l in facets_init]
    facets_init = [l for l in facets_init if len(l[0]) >2]
    # print('\n\n')
    # for l in facets_init:
        # print(l)
    # print('\n\n')

    trans_v = Point(-trans_v.x,-trans_v.y)

    points = []
    dest = {}
    facets = []
    for l in facets_init:
        facets.append(l[0])
        for p in l[0]:
            if not p in points:
                points.append(p)
                dest[p] = transf_point(p,l[1],trans_v)

    # print('\n\n')
    # print('dests: {}'.format(dest))
    # print('\n\n')


    return Sol(points,facets,dest)

# Testing

p00 =  Point(Fraction(0),Fraction(0))
p00_ = Point(Fraction(0),Fraction(0))
p01 =  Point(Fraction(0),Fraction(1))
p02 =  Point(Fraction(0),Fraction(2))
p03 =  Point(Fraction(0),Fraction(3))
p04 =  Point(Fraction(0),Fraction(4))
p10 =  Point(Fraction(1),Fraction(0))
p11 =  Point(Fraction(1),Fraction(1))
p12 =  Point(Fraction(1),Fraction(2))
p13 =  Point(Fraction(1),Fraction(3))
p14 =  Point(Fraction(1),Fraction(4))
p20 =  Point(Fraction(2),Fraction(0))
p21 =  Point(Fraction(2),Fraction(1))
p22 =  Point(Fraction(2),Fraction(2))
p23 =  Point(Fraction(2),Fraction(3))
p30 =  Point(Fraction(3),Fraction(0))
p31 =  Point(Fraction(3),Fraction(1))
p32 =  Point(Fraction(3),Fraction(2))
p33 =  Point(Fraction(3),Fraction(3))
p34 =  Point(Fraction(3),Fraction(4))
pm11 = Point(Fraction(-1),Fraction(1))
p40 =  Point(Fraction(4),Fraction(0))

p2 =  Point(Fraction(2,3),Fraction(0))
p3 =  Point(Fraction(1,3),Fraction(1))
pi =  Point(Fraction(1,2),Fraction(0))
pj =  Point(Fraction(1,2),Fraction(1))

poly = Poly([p00,p2,p3])
ed = Edge(Point(Fraction(1,3),Fraction(1)),Point(Fraction(5,3),Fraction(1)))

# ed1 = Edge(pm11,p31)
# ed2 = Edge(p00,p02)

# q1 =  Point(Fraction(1),Fraction(1,2))
# q2 =  Point(Fraction(0),Fraction(1,2))
q1 =  Point(Fraction(3,4),Fraction(1,4))
q2 =  Point(Fraction(1,2),Fraction(3,4))

triangle = [p00,p2,p3]
# triangle = [p00,p10,pj]
# square = [p00,p10,q1,q2]
square = [p00,p10,p11,p01]
pb1 = Problem(triangle,[])

c1 =  Point(Fraction(1,4),Fraction(1,2))
c2 =  Point(Fraction(1,2),Fraction(1,3))
c3 =  Point(Fraction(3,4),Fraction(1,2))
c4 =  Point(Fraction(1,2),Fraction(5,6))
truc = [c1,c2,c3,c4]
pb2 = Problem(truc,[])

# print (ed1.compute_intersection(ed2))
# square = [p00,p10,p11,p01]
# edge = Edge(p00,p2)
# edge_ = Edge(pi,pj)
# e1 = Edge(p1,p2)
# e2 = Edge(p3,p4)
# e3 = Edge(p1,p4)
# e4 = Edge(p2,p3)

# Testing the join
# print('Joining square with itself: {}'.format(join(square,square)))

# print('Joining square with triangle: {}'.format(join(square,triangle)))
foo = [Point(Fraction(2,3),Fraction(0)), p10, p11,Point(Fraction(1,3),Fraction(1))]
bar = Edge(Point(Fraction(1,3),Fraction(1)),p00)
baz = Edge(Point(Fraction(2,3),Fraction(0)),Point(Fraction(1,3),Fraction(1)))

# pb8 = parse('../fitting/8.pb')
# sol8 = solve(pb8)
# sol8.output('../sol/8.sol')

# pb9 = parse('../fitting/9.pb')
# sol9 = solve(pb9)
# sol9.output('../sol/9.sol')

def solve_prob(id):
    pb = parse('../pb/' + str(id) + '.pb')
    sol = solve(id, pb)
    sol.output('../sol2/' + str(id) + '.sol')

solve_prob(27)
# for i in range(6000):
#     try:
#         solve_prob(i)
#         print(i)
#     except:
#         pass


# for i in range(2237,2727):

# for i in range(5900):
#     try:
#         solve_prob(i)
#         # print(i)
#     except Exception as e:
#         # print(e)
#         pass  

# pb9 = parse('../fitting/9.pb')
# pb10 = parse('../fitting/10.pb')
