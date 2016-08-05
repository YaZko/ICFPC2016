import solver,solver2
import requests
import json

snap_hash = False
get_snap = False
headers = {'X-API-Key':'188-bba213ccaf3cb06f9f2db4a27a6cf380','Accept-Encoding':'gzip'}
if snap_hash:
    r = requests.get('http://2016sv.icfpcontest.org/api/snapshot/list',headers=headers)
    snapshothash = r.json()["snapshots"][-1]["snapshot_hash"]
else:
    snapshothash = "bbe7d278079a690055aab5a142688983fcf0d118"
if get_snap:
    r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%snapshothash,headers=headers)
    with open("snapshot.last","w") as fi:
        fi.write(r.text)

snap = json.load(open("snapshot.last"))

for pb_id in range(101):
    try:
        pb_data = open("pb/%s.pb"%(pb_id+1)).read()
    except Exception as e:
        print("fetch pb",pb_id+1)
        pb_hash = snap["problems"][pb_id]["problem_spec_hash"]
        r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%pb_hash,headers=headers)
# print(r.text)
        pb_data = r.text
        with open("pb/%s.pb"%(pb_id+1),"w") as pb_file:
            pb_file.write(pb_data)

pb_id = 99 
print(pb_id,"",end="")
pb_data = open("pb/%s.pb"%pb_id).read()

#     solution = solver.solve(pb_data)
solution = solver2.solve(pb_data)
#     print("\n",solution)
# content = gzip.decompress(r.content)
# print(content)
