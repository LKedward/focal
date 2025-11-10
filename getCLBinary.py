import sys
import shutil
import os
import random

inputFile  = sys.argv[1]
aux = 'fclKernels.cl'
outputFile = sys.argv[2]

actualFolder = os.getcwd()
folder = '/tmp/' + str(random.randint(1,1e15))
os.system(f'mkdir {folder}')
shutil.copyfile(inputFile,folder + '/' + aux)
os.chdir(folder)
os.system(f'ld -r -b binary {aux} -o {actualFolder}/{outputFile}')
os.chdir(actualFolder)
os.system(f'rm -rf {folder}')
