import time
import subprocess


def run_and_gather_time(*cmd):
    start = time.time()
    subprocess.check_output(cmd)
    end = time.time()
    print("Time taken: ", end - start)
    return end - start


def run_several_times(*cmd, n=4):
    times = []
    for _ in range(n):
        times.append(run_and_gather_time(*cmd))
    return times


def run_several_times_and_average(*cmd, n=4):
    _, *times = run_several_times(*cmd, n=n)
    avg = sum(times) / len(times)
    # Times taken X, Y, Z; average: W
    # round to 1 decimal place
    times = [f"{t:.1f}" for t in times]
    print(f"Times taken {', '.join(times)}; average: {avg:.1f}")


# julia --project=../Stitch.jl/ tests/integration.jl

run_several_times_and_average("julia", "--project=../Stitch.jl/", "tests/integration.jl")
