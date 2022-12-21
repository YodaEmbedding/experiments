import sys

import aim


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
    # runs = list(repo.iter_runs())
    # run = runs[-1]

    query = (
        # f"run.hash == '{run.hash}'"
        f"metric.name == '{metric_name}'"
        f"and metric.context == {context}"
    )

    for metric in repo.query_metrics(query).iter():
        assert isinstance(metric, aim.Metric)
        print_metric(metric)

    # for run_metrics_collection in repo.query_metrics(query).iter_runs():
    #     for metric in run_metrics_collection:
    #         assert isinstance(metric, aim.Metric)
    #         print(metric)


if __name__ == "__main__":
    main()
