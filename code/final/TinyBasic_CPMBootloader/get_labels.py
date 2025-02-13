#!/usr/bin/env python3

import re

with open('tinybasic-2.0.lst', 'r') as file:
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
			print(label + " EQU 0" + odd[even.index(label)] + "H")
			#print(even.index(label))
	#result = [x for x in re.split("\s{3,}",labels) if x]
	#print(result)
