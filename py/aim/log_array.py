import sys

import aim
import numpy as np


def main():
    _, repo_path = sys.argv

    repo = aim.Repo(repo_path)
    run = aim.Run(repo=repo)

    value = np.array([0, 1, 4, 9, 16])
    run.track(value, "arr_1d")


if __name__ == "__main__":
    main()
