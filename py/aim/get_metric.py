import sys

import aim
from aim.storage.context import Context


def print_metric(metric: aim.Metric):
    print(metric.run)
    print(metric.dataframe())
    steps, metric_values = metric.values.sparse_numpy()
    print("\nFirst 4 steps/values:")
    print(steps[:4])
    print(metric_values[:4])


def main():
    _, repo_path = sys.argv

    metric_name = "bpp"
    context = {"loader": "infer", "scope": "epoch"}

    repo = aim.Repo(repo_path)
    runs = list(repo.iter_runs())
    run = runs[-1]

    metric = run.get_metric(metric_name, Context(context))  # type: ignore
    assert metric is not None
    assert isinstance(metric, aim.Metric)
    print_metric(metric)


if __name__ == "__main__":
    main()
