import os
import subprocess

logpath = '/home/<user>/bench_map_nomap.log'
timepath = '/usr/bin/time'
timeout_sec = 10*60 # 10 min time limit.

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
    else:
        with open(logpath, "a") as myfile:
            myfile.write("{} interrupted\n".format(label))


    return interrupted


def test(size, ver):
    label = "{}-{}".format(ver, size)

    cmd = 'dune exec bench/bench_test.exe -- -s {} -v {}'.format(size, ver)
    return test_cmd(cmd, label)

def main():
    vers = [0,1,2]
    sizes = [10,20,40,80,160,320,640,1280,2560,5120,10240]

    if os.path.exists(logpath):
      os.remove(logpath)

    ctd_ver = [True,True,True]

    for size in sizes:
        for ver in vers:
            for i in range(5):
                if ctd_ver[ver] and test(size,ver):
                        ctd_ver[ver] = False
                        print("{}-{} timed out, skipping in future".format(ver, size))

main()
