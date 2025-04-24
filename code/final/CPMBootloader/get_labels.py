#!/usr/bin/env python3

import re

accepted_labels = {"STACK", "SYSTEM_VARIABLES", "CFLBA0", "CFLBA1", "CFLBA2", "CFLBA3", "BLKDAT"}

with open('bootloader.lst', 'r') as file:
    data = file.read()
    labels = data.partition('\x0c')[2]
    lines = labels.split("\n")
    #print(lines)
    for line in lines:
        x = line.split()
        odd = x[::2]
        even = x[1::2]
		#print(odd)
		#print(even)
        for label in even:
            if label not in accepted_labels:
                continue
            print(label + " EQU 0" + odd[even.index(label)] + "H")
			#print(even.index(label))
	#result = [x for x in re.split("\s{3,}",labels) if x]
	#print(result)
