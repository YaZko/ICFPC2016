def solve(data):
    print(data)
    polys,skel = parse(data)
    
    return "fail"

def parse(data):
    data = data.split("\n")
    data.pop()
    data.reverse()
    nb_poly = int(data.pop())
    polys = []
    for i in range(nb_poly):
        nb_point = int(data.pop())
        poly = []
        polys.append(poly)
        for j in range(nb_point):
            point = tuple([fr(x) for x in data.pop().split(",")])
            poly.append(point)
    nb_skel = int(data.pop())
    skel = []
    for i in range(nb_skel):
        seg = tuple([tuple([fr(x) for x in y.split(",")]) for y in data.pop().split(" ")])
        skel.append(seg)

    print(polys)
    print(skel)
    return polys,skel
    
class fr:
    def __init__(self,s):
        s = s.split("/")
        self.n = int(s[0]) 
        if len(s)>1:
            self.d = int(s[1])
        else:
            self.d = 1

    def __str__(self):
        if self.d == 1:
            return str(self.n)
        else:
            return "%d/%d"%(self.n,self.d)

    def __repr__(self):
        return str(self)
