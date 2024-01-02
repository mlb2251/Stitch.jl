import sys
import subprocess

from sys import argv

branches = argv[1:]

for branch in branches:
    print(branch)
    subprocess.check_call(["git", "checkout", branch])
    subprocess.check_call(["julia", "--project=.", "tests/timing.jl"])
