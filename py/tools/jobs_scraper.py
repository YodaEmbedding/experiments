import re
import sys
import time
from pprint import pprint

import pandas as pd
import requests
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait

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
        "yoe": None,
        "p": None,
        "interest": None,
        "salary": None,
        "tech": None,
        "minimum_qualifications": ".KwJkGe ul:nth-of-type(1)",
        "preferred_qualifications": ".KwJkGe ul:nth-of-type(2)",
        "responsibilities": ".BDNOWe ul",
        "description": "div.aG5W3",
        "research": None,
        "degree": None,
    },
    "microsoft": {
        "location": ".css-530.ms-Stack-inner > p",
        "job_title": ".SearchJobDetailsCard > h1:nth-child(2)",
        "yoe": None,
        "p": None,
        "interest": None,
        "salary": None,
        "tech": None,
        "minimum_qualifications": ".WzU5fAyjS4KUVs1QJGcQ > div:nth-child(1) > div:nth-child(2) > ul:nth-child(2)",
        "preferred_qualifications": ".WzU5fAyjS4KUVs1QJGcQ > div:nth-child(1) > div:nth-child(2) > ul:nth-child(4)",
        "responsibilities": "div.MKwm2_A5wy0mMoh9vTuX:nth-child(3) > div:nth-child(1) > div:nth-child(2) > ul:nth-child(1)",
        "description": [
            ".WzU5fAyjS4KUVs1QJGcQ > div:nth-child(1) > div:nth-child(2) > p:nth-child(5)",
            "div.MKwm2_A5wy0mMoh9vTuX:nth-child(1) > div:nth-child(1) > div:nth-child(2)",
        ],
        "research": None,
        "degree": None,
    },
}


def skip_blanklines(s):
    return "\n".join(x for x in s.splitlines() if x.strip())


# NOTE: Not yet implemented.
# def extract_yoe(s):
#     s = s.lower()
#     pattern = r"\d+\D* year"
#     matches = re.findall(pattern, s)
#     return ", ".join(matches)


def extract_tech(result):
    s = "\n".join(
        [
            result["minimum_qualifications"],
            result["preferred_qualifications"],
            result["responsibilities"],
            result["description"],
        ]
    ).lower()
    # fmt: off
    keywords = [
        " c,", "c++", "python", "java",
        "pytorch", "tensorflow", "keras", "scikit-learn", "pandas", "sql", "spark", "hadoop",
        "aws", "azure", "gcp", "docker", "kubernetes",
        "git", "linux", "unix", "bash", "shell",
        "javascript", "typescript", "react", "angular", "vue", "node.js", "express", "flask", "django", "html", "css", "sass", "bootstrap", "tailwind", "webpack", "babel", "eslint", "prettier", "jest", "mocha", "chai", "cypress", "selenium", "webdriver", "appium",
        "jenkins", "circleci", "travis",
        "cuda", "llvm", "llm",
        # "research",
        "publication",
    ]
    # fmt: on
    tech = [x for x in keywords if x in s]
    return ", ".join(tech)


def extract_salary(s):
    s = s.lower()
    pattern = r"\$?((\d{3})[, ]?\d{3}.*\$?(\d{3}),?\d{3})"
    match = re.search(pattern, s)
    if not match:
        return None
    return f"{match.group(2)}-{match.group(3)}"


def extract_degree(s):
    s = s.lower()
    keywords = ["bachelor", "master", "phd"]
    found = [x for x in keywords if x in s]
    return " ".join(found)


def postprocess_result(result, config_key):
    result = result.copy()
    if config_key == "google":
        result["description"] = result["description"].removeprefix("About the job")
        result["location"] = result["location"].removeprefix("place")
        if result["location_more"]:
            result["location"] = result["location_more"]
        del result["location_more"]
        result = {k: skip_blanklines(v.strip()) if v else v for k, v in result.items()}
    # result["yoe"] = extract_yoe(result["description"])
    result["salary"] = extract_salary(result["description"])
    result["tech"] = extract_tech(result)
    result["research"] = any(
        x in result["tech"] for x in ["pytorch", "tensorflow", "publication"]
    )
    result["degree"] = extract_degree(result["minimum_qualifications"])
    return result


def run_scraper(url):
    config_key = next(key for key in SELECTOR_CONFIGS if key in url)
    config = SELECTOR_CONFIGS[config_key]

    # Simple method to get HTML:
    # response = requests.get(url).text

    # More complex method to get HTML (for JS-rendered pages):
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless")
    driver = webdriver.Chrome(options=options)
    driver.get(url)

    timeout = 10
    element_present = EC.presence_of_element_located(
        (By.CSS_SELECTOR, ".css-530.ms-Stack-inner > p")
    )
    WebDriverWait(driver, timeout).until(element_present)

    html = driver.page_source

    # print(html, file=sys.stderr)
    soup = BeautifulSoup(html, "html.parser")

    result = {"url": url}

    for key, selector in config.items():
        if not selector:
            result[key] = None
            continue
        if not isinstance(selector, list):
            selector = [selector]
        elements = [x for s in selector for x in soup.select(s)]
        # assert len(elements) <= 1
        # value = elements[0].text if elements else None
        value = "\n".join(x.text for x in elements)
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
    pd.DataFrame(items).to_csv(sys.stdout, sep="\t", index=False)


if __name__ == "__main__":
    main()
