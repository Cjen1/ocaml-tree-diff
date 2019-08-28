import os
import subprocess

logpath = '/home/<user>/log.txt'
timepath = '/usr/bin/time'
timeout_sec = 2*60*60 # 2 hr command time limit.

def test_cmd(cmd, label):
    print("testing", label)
    cmdlet = '{} --format="%M %E" --output={} -a {}'.format(timepath, logpath, cmd)

    interrupted = False
    try:
        completed = subprocess.run(cmdlet, shell=True, timeout=timeout_sec)

        if completed.returncode != 0:
            interrupted = True
    except subprocess.TimeoutExpired:
        interrupted = True
        
    if not interrupted:
        with open(logpath, "a") as myfile:
            myfile.write("{}\n\n".format(label))

    return interrupted


def test_mine(f1, f2, file_label):
    label = "mine-{}".format(file_label)

    cmd = './reference.exe -- -s {} -t {}'.format(f1,f2)
    return test_cmd(cmd, label)

def test_jsondiff(f1, f2, file_label):
    label = "jsondiff-{}".format(file_label)

    cmd = "python3 ../jsondiff/cli.py {} {}".format(f1,f2)
    res = test_cmd(cmd, label)
    print("")
    return res

def main():
    rel_data_path = "../data/"
    targets = range(1,9)

    if os.path.exists(logpath):
      os.remove(logpath)

    os.system("dune build reference/reference.exe")
    os.system("cp _build/default/reference/reference.exe .")

    for target in targets:
        f1 = "{}{}a.json".format(rel_data_path,target)
        f2 = "{}{}b.json".format(rel_data_path,target)

        file_label = "{}-{}-{}".format(target, os.path.getsize(f1), os.path.getsize(f2))
        
        ctd_mine = True
        ctd_jsondiff = True
        for i in range(3):
            if ctd_mine and test_mine(f1, f2, file_label):
                    ctd_mine = False
                    print("mine-{} timed out, skipping in future".format(file_label))

            if ctd_jsondiff and test_jsondiff(f1, f2, file_label):
                    ctd_jsondiff = False
                    print("jsondiff-{} timed out, skipping in future".format(file_label))

    os.system("rm reference.exe")

main()
