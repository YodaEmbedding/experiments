import sys
from pprint import pprint

import aim


def main():
    _, repo_path = sys.argv

    repo = aim.Repo(repo_path)
    runs = list(repo.iter_runs())
    run = runs[-1]

    params = run[...]
    pprint(params)


if __name__ == "__main__":
    main()
