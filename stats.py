import sys
import csv
import numpy as np
import matplotlib.pyplot as plt


def error(code):
    print(f"Usage: ./{sys.argv[0]} <TDCE.csv> <LVN.csv> <benchmark name> <output>")
    exit(code)


def draw(baseline, tdce, lvn, names, bmark, file):
    N = len(baseline)

    ind = np.arange(N)

    plt.figure(figsize=(30,20))

    width = 0.3       

    plt.bar(ind,           baseline , width, label='Baseline')
    plt.bar(ind + width,   tdce,      width, label='TDCE')
    plt.bar(ind + 2*width, lvn,       width, label='LVN')

    plt.xlabel('benchmarks', fontsize=20)
    plt.ylabel("(baseline total_dyn_inst) / (optimized total_dyn_inst)", fontsize=20)
    plt.title(f"LVN and TDCE Performance ({bmark} benchmarks)", fontsize=30)

    plt.xticks(ind + width / 2, names)
    plt.xticks(rotation=90)

    plt.legend(loc='best')
    plt.savefig(file)

    print(f"Generated {file}")


if __name__ == '__main__':
    if '--help' in sys.argv or '-h' in sys.argv:
        error(0)
    if len(sys.argv) < 5:
        error(1)

    TDCE  = sys.argv[1]
    LVN   = sys.argv[2]
    BMARK = sys.argv[3]
    OUT   = sys.argv[4]

    tdce = []
    baseline = []
    names = []
    with open(TDCE, 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            if row[1] == 'tdce':
                tdce.append(int(row[2]))
                names.append(row[0])
            if row[1] == 'baseline':
                baseline.append(int(row[2]))

    lvn = []
    with open(LVN, 'r') as file:
        reader = csv.reader(file)
        for i, row in enumerate(reader):
            if row[1] == 'lvn':
                lvn.append(int(row[2]))
            if row[1] == 'baseline':
                assert row[0] == names[int(i/2)]
                assert int(row[2]) == baseline[int(i/2)]
    
    for i, b in enumerate(baseline):
        baseline[i] = b/baseline[i]
        tdce[i] = b/tdce[i]
        lvn[i] = b/lvn[i]

    draw(baseline, tdce, lvn, names, BMARK, OUT)

    tdce_max = max(tdce)
    lvn_max  = max(lvn)

    tdce_min = min(tdce)
    lvn_min  = min(lvn)
    
    assert len(tdce) == len(lvn)
    n = len(tdce)
    tdce_avg = sum(tdce)/n
    lvn_avg  = sum(lvn)/n

    print("speedup stats")
    print(f"TDCE avg = {tdce_avg} LVN avg = {lvn_avg}")
    print(f"TDCE min = {tdce_min} LVN min = {lvn_min}")
    print(f"TDCE max = {tdce_max} LVN max = {lvn_max}")
