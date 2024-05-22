#!/usr/bin/env python3

import re

with open('tinybasic-2.0.lst', 'r') as file:
	data = file.read()
	labels = data.partition('\x0c')[2]
	lines = labels.split("\n")
	#print(lines)
	for line in lines:
		x = line.split('\s{3,}')
		print(x)
	#result = [x for x in re.split("\s{3,}",labels) if x]
	#print(result)
