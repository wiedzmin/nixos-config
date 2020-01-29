import json
import os
import subprocess

import dmenu
import redis


r = redis.Redis(host='localhost', port=6379, db=0)
dbms_meta = json.loads(r.get("job/dbms_meta"))

dbms_entry = dmenu.show(dbms_meta.keys(), lines=5)
if dbms_entry:
    dbms_pass_task = subprocess.Popen("@passBinary@ {0}".format(dbms_meta[dbms_entry]["passwordPassPath"]),
                                      shell=True, stdout=subprocess.PIPE)
    dbms_pass = dbms_pass_task.stdout.read().decode().split("\n")[0]
    assert dbms_pass_task.wait() == 0

    if dbms_meta[dbms_entry]["command"] == "mycli":
        os.system('@tmuxBinary@ new-window "@mycliBinary@ --host {0} --user {1} --password {2}"'.format(
            dbms_meta[dbms_entry]["ip"],
            dbms_meta[dbms_entry]["user"],
            dbms_pass
        ))
    elif dbms_meta[dbms_entry]["command"] == "mycli":
        os.system('@tmuxBinary@ new-window "PGPASSWORD={2} @pgcliBinary@ --host {0} --user {1}"'.format(
            dbms_meta[dbms_entry]["ip"],
            dbms_meta[dbms_entry]["user"],
            dbms_pass
        ))
