import os
import subprocess
import re
import json
import time

def run_sim(iterations, bodies, cores, strategy='default', chunk_size = None):

	extra_args = ""
	if strategy == "pm" or strategy == "pb":
		extra_args = strategy

	if strategy == "plc":
		extra_args = "plc {}".format(chunk_size)

	if strategy == "pbc":

		extra_args = "pbc {}".format(chunk_size)

	command = "(time barnes-hut -i {} -n {} {} +RTS -N{}) 2> times.txt 1>times.txt".format(iterations, bodies, extra_args, cores)
	print(command)
	result = subprocess.run(command, shell=True)

	with open("times.txt") as f:
		lines = f.readlines()
		real = re.search('real\t([^s]*)', lines[1])
		user = re.search('real\t([^s]*)', lines[2])
		sys = re.search('real\t([^s]*)', lines[3])
		return (parse_time(real.group(1)), parse_time(user) + parse_time(sys))

def parse_time(time_string):
	
	split = time_str.split('m')
	mins = split[0]
	secs = split[1]

	return float(mins) * 60 + secs

def main():

	iterations = 500
	bodies = 1000
	print("Starting tests.")

	os.chdir('../')
	subprocess.run('stack install', shell=True)
	os.chdir('test/')
	cores = [2, 4, 6, 8]
	chunks = [25 * i for i in range(1,21)]
	results = {}
	strategies = ["default", 'pm', 'pb', 'plc', 'pbc']

	for strategy in strategies:

		strategyResults = {}

		if strategy == 'default':
			run_time = run_sim(iterations, bodies, 1) # no parallel
			clock_time = run_time[0]
			strategyResults[strategy] = clock_time
			continue

		for core in cores:

			coreResults = {}
			if strategy == 'pbc' or strategy == 'plc':

				for chunk in chunks:

					run_time = run_sim(iterations, bodies, core, strategy=strategy, chunk_size=chunk)
					clock_time = run_time[0]

					coreResults[chunk] = clock_time
			else:

				run_time = run_sim(iterations, bodies, core, strategy=strategy)
				clock_time = run_time[0]
				coreResults[1] = clock_time

			strategyResults[core] = coreResults

		results[strategy] = strategyResults

	timestr = time.strftime("%Y%m%d-%H-%M-%S")
	with open('test_results_' + timestr + '.json', 'w') as f:

		f.write(json.dumps(results))

if __name__ == '__main__':
    main()