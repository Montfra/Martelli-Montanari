import os
import subprocess
import re
import sys

f = open('test.txt')

for line in f:

    if line[0] == 'F' and line[1] == ' ':
        end1 = "false"
        line = line[2:]
    else:
        #print line
        command = "printf '" + line + ".'" + " | swipl"
        #print(command)
        result = subprocess.check_output(command, stderr=subprocess.STDOUT, shell=True)
        endString = result.split(".")[10]
        end1 = endString.replace("\n", "")
        #print(end1)

    #print line
    line2 = line.replace("=", "?=")
    line3 = line2.rstrip()
    #print(line2)
    command = "printf 'unify([" + line3 + "], choix_premier).'" + " | swipl test.pl"
    #print(command)
    result = subprocess.check_output(command, stderr=subprocess.STDOUT, shell=True)
    endString = result.split(".")[10]

    endd = re.sub(r'^SYSTEM.*\n?', '', endString, flags=re.MULTILINE)
    end2 = endd.rstrip()
    end2 = end2.replace("\n", "")
    end2 = re.sub(r'^Unification.*true?', 'false', end2, flags=re.MULTILINE)
    #print(end2)

    if end1 == end2:
        print("Test OK ;)")
    else:
        sys.stdout.write("Test pas OK :( [")
        sys.stdout.write("Expected : ")
        sys.stdout.write(end1)
        sys.stdout.write(" | Your : ")
        sys.stdout.write(end2)
        sys.stdout.write("]\n")

f.close()