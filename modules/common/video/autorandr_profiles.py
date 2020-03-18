import os
import dmenu

profiles = []


for root, dirs, files in os.walk("@autorandrProfiles@"):
    for dir in dirs:
        if not dir.endswith(".d"):
            profiles.append(dir)

result = dmenu.show(profiles, prompt='profile', lines=5)
if result:
    print(result)
    os.system("autorandr --load {0}".format(result))
