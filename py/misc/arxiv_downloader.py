import csv
import os
import re
import subprocess
import sys

import arxiv

LOG_FILE = os.path.expanduser("~/Dropbox/books/papers/arxiv_papers.tsv")


def fix_title(title: str) -> str:
    title = " ".join(str.strip(x) for x in title.split("\n"))
    title = title.replace(":", "_")
    return title


def paper_to_filename(paper: arxiv.Result) -> str:
    authors = paper.authors
    title_str = fix_title(paper.title)
    author_str = authors[0] if len(authors) == 1 else f"{authors[0]} et al."
    date_str = paper.published.strftime("%y%m")
    filename = f"{date_str} {author_str} - {title_str}.pdf"
    return filename


def parse_line(line: str):
    id_pattern = r"(\d{4}\.\d{4,6}(v\d+)?)"
    m = re.match(rf".*{id_pattern}(\.pdf)?$", line)
    return m.group(1) if m is not None else None


def set_metadata(filename: str, title: str, author: str):
    args = [
        "exiftool",
        filename,
        "-overwrite_original",
        f"-Author={author}",
        f"-Title={title}",
    ]
    subprocess.run(args, capture_output=True, check=True)


lines = sys.stdin.readlines()
paper_ids = [parse_line(line.strip()) for line in lines]
paper_ids = [x for x in paper_ids if x is not None]
papers = arxiv.Search(id_list=paper_ids).results()

for paper, paper_id in zip(papers, paper_ids):
    src_filename = f"{paper_id}.pdf"
    dst_filename = paper_to_filename(paper)
    if os.path.exists(dst_filename):
        print(f"[TargetExists] {dst_filename}")
    elif os.path.exists(src_filename):
        print(f"[Rename] {src_filename}")
        os.rename(src_filename, dst_filename)
    else:
        print("[Download]")
        paper.download_pdf(filename=dst_filename)
    author_str = "; ".join(str(x) for x in paper.authors)
    print(f"file:    {dst_filename}")
    print(f"url:     {paper.entry_id}")
    print(f"authors: {author_str}")
    print(f"title:   {paper.title}\n")
    with open(LOG_FILE, "a") as f:
        writer = csv.writer(f, delimiter="\t")
        writer.writerow([paper_id, paper.entry_id, author_str, paper.title])
    set_metadata(
        dst_filename,
        title=fix_title(paper.title),
        author=author_str,
    )
