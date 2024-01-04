import sys
import subprocess

from sys import argv

branches = argv[1:]

for branch in branches:
    subprocess.check_call(["git", "checkout", branch])
    print()
    print("*" * 80)
    print(branch)
    subprocess.check_call(["julia", "--project=.", "tests/timing.jl"])
