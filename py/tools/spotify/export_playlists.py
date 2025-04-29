from __future__ import annotations

import json
import os

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


def query_all_batches(query, offset=0, total=None, *args, **kwargs):
    x = query(*args, **kwargs, offset=offset)
    if total is None:
        total = x.total
    limit = x.limit
    yield x
    if limit == 0:
        return
    for offset in range(offset + limit, total, limit):
        yield query(*args, **kwargs, limit=limit, offset=offset)


def query_all(
    query, iterate_batch=lambda batch: batch.items, args=(), **kwargs
):
    batches = query_all_batches(query, *args, **kwargs)
    return [x for batch in batches for x in iterate_batch(batch)]


def main():
    user_id, spotify = load_spotify()
    playlists = query_all(spotify.playlists, user_id=user_id)
    os.makedirs(PLAYLISTS_ROOT, exist_ok=True)

    for playlist in playlists:
        name_sanitized = playlist.name.replace("/", "_")
        filename = f"{PLAYLISTS_ROOT}/{playlist.id}_{name_sanitized}.spt"
        print(f"{filename}...")

        playlist_items = query_all(
            spotify.playlist_items, playlist_id=playlist.id
        )
        tracks = [x.track for x in playlist_items]
        track_uris = [x.uri for x in tracks]

        with open(filename, "w") as f:
            print("\n".join(track_uris), file=f)


if __name__ == "__main__":
    main()
