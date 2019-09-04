import os
import sys
import glob
import re


def main():
    extraction_dir = sys.argv[1]
    output_dir = sys.argv[2]
    filelist = glob.glob(os.path.join(extraction_dir, "*.txt"))
    for filename in filelist:
        basename = filename.split("/")[-1] # .split('.')[0]
        with open(filename) as f:
            contents = f.read()
            fixed = re.sub(r'\s*\n\s*\n\s*', '.\n\n', contents)
            fixed = re.sub(r'\.\.', '.', fixed)
            ofile = os.path.join(output_dir, basename) #  + '.txt')
            with open(ofile, 'w') as outfile:
                outfile.write(fixed)

main()
