from __future__ import annotations

import json
import os
from functools import partial

import tekore as tk

PLAYLISTS_ROOT = "playlists"


def load_spotify() -> tuple[str, tk.Spotify]:
    with open("./secret.json") as f:
        config = json.load(f)

    user_id = config.pop("user_id")
    config.pop("redirect_uri", None)
    token = tk.request_client_token(**config)
    spotify = tk.Spotify(token)
    return user_id, spotify


def query_all_batches(query, *, offset=0, total=None):
    response = query(offset=offset)
    if total is None:
        total = response.total
    limit = response.limit
    yield response
    if limit == 0:
        return
    for offset in range(offset + limit, total, limit):
        response = query(limit=limit, offset=offset)
        yield response


def query_all(query, *, offset=0, total=None):
    return (
        item
        for batch in query_all_batches(query, offset=offset, total=total)
        for item in batch.items
    )


def main():
    user_id, spotify = load_spotify()
    playlists = query_all(partial(spotify.playlists, user_id=user_id))
    os.makedirs(PLAYLISTS_ROOT, exist_ok=True)

    for playlist in playlists:
        name_sanitized = playlist.name.replace("/", "_")
        filename = f"{PLAYLISTS_ROOT}/{playlist.id}_{name_sanitized}.spt"
        print(f"{filename}...")

        playlist_items = query_all(
            partial(spotify.playlist_items, playlist_id=playlist.id)
        )
        tracks = [x.track for x in playlist_items]
        track_uris = [x.uri for x in tracks]

        with open(filename, "w") as f:
            print("\n".join(track_uris), file=f)


if __name__ == "__main__":
    main()
