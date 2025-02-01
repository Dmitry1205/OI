import sys
if(len(sys.argv)>1):
	for j in sys.argv[1:]:
		name=j
		with open(name, 'r') as file:
			num=1
			for line in file.readlines():
				if(line.isspace() or line==''):
					print(line, end='')
					continue
				print(num, line, end='')
				num+=1
		print()
		
else:
	num=1
	for line in sys.stdin.readlines():
		if(line.isspace() or line==''):
			print(line, end='')
			continue
		print(num, line, end='')
		num+=1
	print()
