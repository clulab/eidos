from glob import glob
import argparse
import os
import sys

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('wdir', help='directory with the grounding eval files')
    args = parser.parse_args()
    return args




if __name__ == '__main__':
    args = parse_args()
    files = glob(os.path.join(args.wdir, '*.ground.csv'))
    print(files[:10])
    with open(os.path.join(args.wdir, 'concatenated.csv'), 'w') as fo:
        header_found = False
        for filename in files:
            with open(filename) as fi:
                for line in fi:
                    if line[:5] == 'DocID':
                        if not header_found:
                            fo.write(line)
                            header_found = True
                    elif line.strip() != '':
                        fo.write(line)
