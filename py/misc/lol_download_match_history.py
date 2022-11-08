import json
import pathlib
from functools import wraps
from pprint import pprint

import requests
from ratelimit import limits, sleep_and_retry


def cache_path(kwargs, ignore_keys):
    kwargs = {k: v for k, v in kwargs.items() if k not in ignore_keys}
    basename = "&".join(f"{k}={v}" for k, v in sorted(kwargs.items()))
    path = f"{basename}.json"
    return path


def cache(base_dir, ignore_keys=None):
    if ignore_keys is None:
        ignore_keys = []
    ignore_keys = ignore_keys + [
        "api_key",
        "api_version",
        "server",
        "encrypted_account_id",
    ]

    def decorator(func):
        @wraps(func)
        def wrapper(**kwargs):
            pathlib.Path(base_dir).mkdir(parents=True, exist_ok=True)
            path = f"{base_dir}/{cache_path(kwargs, ignore_keys)}"
            try:
                with open(path) as f:
                    result = json.load(f)
            except FileNotFoundError:
                result = func(**kwargs)
                with open(path, "w") as f:
                    json.dump(result, f, indent=2)
            return result

        return wrapper

    return decorator


def get_settings():
    try:
        with open("settings.json") as f:
            return json.load(f)
    except FileNotFoundError:
        return {}


@sleep_and_retry
@limits(calls=20 - 5, period=1)
@limits(calls=100 - 10, period=120)
def get_api_response(url):
    response = requests.get(url, allow_redirects=True)
    if response.status_code != 200:
        raise Exception(f"API response: {response.status_code}")
    return response.json()


@cache("cache/matchlists")
def get_matchlist(
    *, encrypted_account_id=None, server=None, api_version=None, api_key=None
):
    base = rf"https://{server}.api.riotgames.com/lol/match/{api_version}"
    query = rf"api_key={api_key}"
    url = rf"{base}/matchlists/by-account/{encrypted_account_id}?{query}"
    return get_api_response(url)


@cache("cache/matches")
def get_match(*, match_id=None, server=None, api_version=None, api_key=None):
    query = rf"api_key={api_key}"
    base = rf"https://{server}.api.riotgames.com/lol/match/{api_version}"
    url = rf"{base}/matches/{match_id}?{query}"
    return get_api_response(url)


@cache("cache/summoners")
def get_summoner(
    *, summoner_name=None, server=None, api_version=None, api_key=None
):
    base = rf"https://{server}.api.riotgames.com/lol/summoner/{api_version}"
    query = rf"api_key={api_key}"
    url = rf"{base}/summoners/by-name/{summoner_name}?{query}"
    return get_api_response(url)


def _main():
    settings = get_settings()
    common_kwargs = {
        "api_key": settings["api_key"],
        "api_version": settings["api_version"],
        "server": settings["server"],
    }
    summoner_name = settings["summoner_name"]
    if "encrypted_account_id" not in settings:
        summoner = get_summoner(summoner_name=summoner_name, **common_kwargs)
        settings["encrypted_account_id"] = summoner["accountId"]
    encrypted_account_id = settings["encrypted_account_id"]
    pprint(settings)

    matchlist = get_matchlist(
        encrypted_account_id=encrypted_account_id, **common_kwargs
    )

    for match_info in matchlist["matches"]:
        match_id = match_info["gameId"]
        print(f"Retrieving match_id={match_id}...")
        match = get_match(match_id=match_id, **common_kwargs)


if __name__ == "__main__":
    _main()
