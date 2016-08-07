# import solver,solver2
import requests
import json
import time

snap_hash = False
headers = {'X-API-Key':'188-bba213ccaf3cb06f9f2db4a27a6cf380','Accept-Encoding':'gzip'}
if snap_hash:
    r = requests.get('http://2016sv.icfpcontest.org/api/snapshot/list',headers=headers)
    print (r.text)
    snapshothash = r.json()["snapshots"][-1]["snapshot_hash"]
else:
    snapshothash = "9023974d5b9aed37a1055311759e770a3809a96e"

try:
    snap = json.load(open("snapshot.last"))
except Exception as e:
    r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%snapshothash,headers=headers)
    with open("snapshot.last","w") as fi:
        fi.write(r.text)

# print(snap)

pbs = snap["problems"]

for pb in pbs:
    pb_id = pb["problem_id"]
    # try:
    #     pb_data = open("pb/%s.pb"%(pb_id)).read()
    # except Exception as e:
    #     print("fetch pb", pb_id)
    #     pb_hash = pb["problem_spec_hash"]
    #     r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%pb_hash,headers=headers)
    #     # print(r.text)
    #     pb_data = r.text
    #     with open("pb/%s.pb"%(pb_id),"w") as pb_file:
    #         pb_file.write(pb_data)
    try:
        pb_sol = open("sol/%s.sol"%pb_id).read()
        data = {"problem_id":pb_id,"solution_spec":pb_sol}
        r = requests.post('http://2016sv.icfpcontest.org/api/solution/submit',headers=headers,data=data)
        time.sleep(1)
        print(r.text)
    except Exception as e:
        pass


# pb_id = 99 
# print(pb_id,"",end="")
# pb_data = open("pb/%s.pb"%pb_id).read()

#     solution = solver.solve(pb_data)
# solution = solver2.solve(pb_data)
#     print("\n",solution)
# content = gzip.decompress(r.content)
# print(content)
