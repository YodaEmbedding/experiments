#!/bin/bash

# See also: https://superuser.com/questions/1106343/determine-video-bitrate-using-ffmpeg

IGNORE_EXTENSIONS=(jpg jpeg png webp heic txt)

for f in *; do
  # Skip directories.
  if [[ -d "$f" ]]; then
    continue
  fi

  # Skip files with ignored extensions.
  extension="${f##*.}"
  # NOTE: A bit hacky.
  if [[ " ${IGNORE_EXTENSIONS[@]} " =~ " ${extension} " ]]; then
    continue
  fi

  # WARNING: The output order of the entries doesn't seem to depend on the specified order of the stream keys.
  info="$(ffprobe -v quiet -select_streams v:0 -show_entries stream=codec_name,width,height,duration,bit_rate -of default=noprint_wrappers=1:nokey=1 "$f")"

  codec_name="$(echo "$info" | cut -d$'\n' -f1)"
  width="$(echo "$info" | cut -d$'\n' -f2)"
  height="$(echo "$info" | cut -d$'\n' -f3)"
  duration="$(echo "$info" | cut -d$'\n' -f4)"
  bit_rate="$(echo "$info" | cut -d$'\n' -f5)"
  file_size="$(du -h "$f" | cut -f1)"

  printf '%5s %12s %4sx%4s %6s %12s %s\n' "$codec_name" "$bit_rate" "$width" "$height" "$file_size" "$duration" "$f"
done

