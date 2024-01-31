import shlex
import sys
import subprocess
import argparse


def arguments():
    parser = argparse.ArgumentParser()
    parser.add_argument("--args", nargs="+", default=None)
    parser.add_argument("--branches", nargs="+")
    args = parser.parse_args()
    if args.args is None:
        args.args = [[]]
    elif args.args == ["/imperative_realistic_each"]:
        args.args = [
            ["--paths", f"data/imperative_realistic/{i}.json"] for i in range(10)
        ]
    else:
        args.args = [shlex.split(arg) for arg in args.args]
    return args


def tee_cmd(cmd):
    result = []
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    for line in iter(p.stdout.readline, b""):
        sys.stdout.write(line.decode())
        sys.stdout.flush()
        result.append(line.decode())
    p.wait()
    return "".join(result)


def last_line(string):
    return string.strip().split("\n")[-1]


def median_time(string):
    return float(last_line(string).split()[-1])


def print_comparison(branch1, branch2, output1, output2):
    print()
    print("*" * 80)
    print(f"Time change from {branch1} to {branch2}")
    for arg in output1:
        if arg:
            print(arg + ":")
        print(f"Before: {last_line(output1[arg])}")
        print(f"After: {last_line(output2[arg])}")
        print(
            f"Change: {median_time(output2[arg]) / median_time(output1[arg]) - 1:+.1%}"
        )
        print()
    print("*" * 80)


def main():
    args = arguments()
    branches = []
    outputs = []
    for branch in args.branches:
        output = {}
        for arg in args.args:
            subprocess.check_call(["git", "checkout", branch])
            print()
            print("*" * 80)
            print(branch, " ".join(arg))
            result = tee_cmd(
                [
                    "julia",
                    "--project=.",
                    "tests/timing.jl",
                    *arg,
                ]
            )
            output[" ".join(arg)] = result
        branches.append(branch)
        outputs.append(output)
        if len(branches) > 1:
            print_comparison(branches[-2], branches[-1], outputs[-2], outputs[-1])


if __name__ == "__main__":
    main()
