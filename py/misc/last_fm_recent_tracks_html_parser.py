"""
Usage:
curl -L 'https://www.last.fm/user/USERNAME' | \
    python last_fm_recent_tracks_html_parser.py
"""

import sys
from bs4 import BeautifulSoup


def get_info(row):
    name = row.find("td", class_="chartlist-name").find("a").text
    ts = row.find("td", class_="chartlist-timestamp").find("span")
    timestamp = ts["title"] if "title" in ts.attrs else ts["class"][0]
    return name, timestamp


def main():
    html = sys.stdin.read()
    soup = BeautifulSoup(html, "lxml")
    rows = soup.find_all("tr", class_="chartlist-row")[:10]

    for row in rows:
        name, timestamp = get_info(row)
        print(f"{name:30} {timestamp}")


if __name__ == "__main__":
    main()
