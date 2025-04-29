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
    # token = tk.request_client_token(**config)
    token = tk.prompt_for_user_token(**config, scope=tk.scope.every)
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


def chunk(xs, length):
    for i in range(0, len(xs), length):
        yield xs[i : i + length]


def main():
    user_id, spotify = load_spotify()
    playlists = query_all(partial(spotify.playlists, user_id=user_id))
    os.makedirs(PLAYLISTS_ROOT, exist_ok=True)

    for playlist in playlists:
        name_sanitized = playlist.name.replace("/", "_")
        filename = f"{PLAYLISTS_ROOT}/{playlist.id}_{name_sanitized}.spt"
        print(f"{filename}...")

        if input("Enter y to deduplicate: ") != "y":
            continue

        playlist_items = query_all(
            partial(spotify.playlist_items, playlist_id=playlist.id)
        )
        tracks = [x.track for x in playlist_items]

        visited_albums = set()
        unique_album_tracks = []
        unique_indices = []
        remove_indices = []

        for i, track in enumerate(tracks):
            album_id = track.album.id
            if album_id in visited_albums:
                remove_indices.append(i)
                continue
            unique_indices.append(i)
            unique_album_tracks.append(track)
            visited_albums.add(album_id)

        # Reverse since mutating playlist...
        # Though I'm guessing snapshot_id takes care of that already.
        remove_indices = remove_indices[::-1]

        for indices in chunk(remove_indices, 100):
            spotify.playlist_remove_indices(
                playlist.id, indices, snapshot_id=playlist.snapshot_id
            )
            # time.sleep(5)


if __name__ == "__main__":
    main()
