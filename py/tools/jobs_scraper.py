import sys
import time
from pprint import pprint

import pandas as pd
import requests
from bs4 import BeautifulSoup

RATE_LIMIT = 1

SELECTOR_CONFIGS = {
    # NOTE: To scrape saved URLs from the sidebar:
    # console.log([...document.getElementsByClassName("Si6A0c Qai30b")].map(x => x.href.replace("/saved/", "/results/")).join("\n"))
    #
    "google": {
        # id: selector
        "location": "span.vo5qdf",
        "location_more": "b",
        "job_title": "h2.p1N2lc",
        "minimum_qualifications": ".KwJkGe ul:nth-of-type(1)",
        "preferred_qualifications": ".KwJkGe ul:nth-of-type(2)",
        "responsibilities": ".BDNOWe ul",
        "description": "div.aG5W3",
    },
}


def skip_blanklines(s):
    return "\n".join(x for x in s.splitlines() if x.strip())


def postprocess_result(result, config_key):
    if config_key == "google":
        result = result.copy()
        result["description"] = result["description"].removeprefix("About the job")
        result["location"] = result["location"].removeprefix("place")
        if result["location_more"]:
            result["location"] = result["location_more"]
        del result["location_more"]
        result = {k: skip_blanklines(v.strip()) if v else v for k, v in result.items()}
    return result


def run_scraper(url):
    config_key = next(key for key in SELECTOR_CONFIGS if key in url)
    config = SELECTOR_CONFIGS[config_key]

    response = requests.get(url)
    soup = BeautifulSoup(response.text, "html.parser")
    result = {"url": url}

    for key, selector in config.items():
        elements = soup.select(selector)
        assert len(elements) <= 1
        value = elements[0].text if elements else None
        result[key] = value

    result = postprocess_result(result, config_key)
    pprint(result, stream=sys.stderr)
    return result


def main():
    urls = sys.stdin.read().splitlines()
    items = []
    for url in urls:
        if not url.strip():
            continue
        items.append(run_scraper(url))
        time.sleep(RATE_LIMIT)
    pd.DataFrame(items).to_csv(sys.stdout, index=False)


if __name__ == "__main__":
    main()
