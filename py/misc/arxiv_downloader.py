import os
import re
import subprocess
import sys

import arxiv


def fix_title(title: str) -> str:
    return " ".join(map(str.strip, title.split("\n")))


def paper_to_filename(paper: dict) -> str:
    authors = paper["authors"]
    title_str = fix_title(paper["title"])
    author_str = authors[0] if len(authors) == 0 else f"{authors[0]} et al."
    filename = f"{author_str} - {title_str}"
    return filename


def parse_line(line: str):
    id_pattern = r"(\d{4}\.\d{4,6}(v\d+)?)"
    m = re.match(fr".*{id_pattern}(\.pdf)?$", line)
    return m.group(1) if m is not None else None


paper_ids = [parse_line(line.strip()) for line in sys.stdin.readlines()]
paper_ids = [x for x in paper_ids if x is not None]
papers = arxiv.query(id_list=paper_ids)

for paper, paper_id in zip(papers, paper_ids):
    basename = paper_to_filename(paper)
    dst_filename = f"{basename}.pdf"
    src_filename = f"{paper_id}.pdf"
    if os.path.exists(src_filename):
        print(f"[Rename] {src_filename}")
        os.rename(src_filename, dst_filename)
    else:
        print("[Download]")
        arxiv.download(paper, slugify=lambda _, basename=basename: basename)
    print(dst_filename)
    print(f"url:     {paper['arxiv_url']}")
    print(f"author:  {paper['author']}")
    print(f"authors: {paper['authors']}")
    print(f"title:   {paper['title']}\n")
    args = [
        "exiftool",
        dst_filename,
        "-overwrite_original",
        f"-Author={'; '.join(paper['authors'])}",
        f"-Title={fix_title(paper['title'])}",
    ]
    subprocess.run(args, capture_output=True, check=True)
