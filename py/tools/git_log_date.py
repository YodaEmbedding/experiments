import argparse
import datetime
import itertools
import re
import subprocess
import sys
from types import SimpleNamespace


def parse_git_log(lines):
    d = None
    for line in lines:
        if m := re.match(r"^commit (?P<commit_hash>[0-9a-f]{7,40})$", line):
            if d:
                yield d
            d = SimpleNamespace(commit_hash=None, commit_date=None, sign_date=None)
            d.commit_hash = m.group("commit_hash")
        elif d is None:
            pass
        elif m := re.match(r"^gpg: Signature made (?P<sign_date>.*)$", line):
            d.sign_date = datetime.datetime.strptime(
                m.group("sign_date"), "%a %b %d %H:%M:%S %Y %Z"
            ).replace(tzinfo=None)
        elif m := re.match(r"^Date:\s+(?P<commit_date>.*)$", line):
            d.commit_date = datetime.datetime.strptime(
                m.group("commit_date"), "%a %b %d %H:%M:%S %Y %z"
            ).replace(tzinfo=None)
    if d:
        yield d


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--max-count",
        type=int,
        default=None,
        help="Maximum number of commits to check.",
    )
    return parser.parse_args()


def main():
    args = parse_args()

    p = subprocess.Popen(
        [
            "git",
            "log",
            "--show-signature",
            *(
                (f"--max-count={args.max_count + 1}",)
                if args.max_count is not None
                else ()
            ),
        ],
        stdout=subprocess.PIPE,
        text=True,
    )

    records = parse_git_log(p.stdout)
    date_fmt = "%Y-%m-%d %H:%M:%S"
    errors = []

    for x, y in itertools.pairwise(records):
        error = False

        if x.sign_date and x.commit_date and x.sign_date < x.commit_date:
            msg = f"[sign_date < commit_date] {x.commit_hash}"
            error = True
            errors.append(msg)

        if x.sign_date and y.sign_date and x.sign_date < y.sign_date:
            msg = f"[sign_date non-monotonic] {x.commit_hash} -> {y.commit_hash}"
            error = True
            errors.append(msg)

        friendly = {
            "commit_hash": x.commit_hash,
            "commit_date": x.commit_date.strftime(date_fmt) if x.commit_date else None,
            "sign_date": x.sign_date.strftime(date_fmt) if x.sign_date else None,
        }
        print(f"{friendly}" if not error else f"\033[91m{friendly}\033[0m")

    print("----------------")

    for error in errors:
        print(f"\033[91m{error}\033[0m", file=sys.stderr)


if __name__ == "__main__":
    main()
