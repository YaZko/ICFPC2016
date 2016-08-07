# TO BE LAUNCH WITH python3 src/submit.py
import requests
import json
import time

snap_hash = True
headers = {'X-API-Key':'188-bba213ccaf3cb06f9f2db4a27a6cf380','Accept-Encoding':'gzip'}
if snap_hash:
    r = requests.get('http://2016sv.icfpcontest.org/api/snapshot/list',headers=headers)
    snapshothash = r.json()["snapshots"][-1]["snapshot_hash"]
else:
    snapshothash = "40f0f10ea3b4a6805aecccc4b4274d004b3dceca"

try:
    snap = json.load(open("snapshot." + snapshothash))
except Exception as e:
    print('getting latest snapshop')
    time.sleep(1)
    r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%snapshothash,headers=headers)
    with open("snapshot." + snapshothash,"w") as fi:
        fi.write(r.text)
    snap = json.load(open("snapshot." + snapshothash))

pbs = snap["problems"]


with open('submissions.json') as fp:
    submissions = json.load(fp)

    
#######################
# DIRECTORY TO SUBMIT #
#######################
# submit all id.sol of this directory where id is the id of the problem
dirpath = 'pb/'


for pb in pbs:
    pb_id = pb["problem_id"]
    try:
        if submissions[str(pb_id)] < 1 :
            pb_sol = open(dirpath + "%s.sol"%pb_id).read()
            data = {"problem_id":pb_id,"solution_spec":pb_sol}
            print('submitting pb', pb_id)
            time.sleep(1)
            r = requests.post('http://2016sv.icfpcontest.org/api/solution/submit',headers=headers,data=data)
            if r.json()['ok']:
                res = r.json()['resemblance']
                print('resemblance :', res)
                submissions[str(pb_id)] = res
            else:
                print('failed', r.text)
    except Exception as e:
        # print(e)
        pass
    
with open('submissions.json', 'w') as fp:
    json.dump(submissions, fp)
