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


for pb in pbs:
    pb_id = pb["problem_id"]
    try:
        pb_data = open("pb/%s.pb"%(pb_id)).read()
    except Exception as e:
        print("fetch pb", pb_id)
        pb_hash = pb["problem_spec_hash"]
        time.sleep(1)
        r = requests.get('http://2016sv.icfpcontest.org/api/blob/%s'%pb_hash,headers=headers)
        if (r.status_code == 200):
            pb_data = r.text
            with open("pb/%s.pb"%(pb_id),"w") as pb_file:
                pb_file.write(pb_data)
        else:
            print(r.text)
