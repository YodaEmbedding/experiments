#!/bin/bash

# http://bdd-data.berkeley.edu/download.html
# https://web.archive.org/web/20241108192523/https://doc.bdd100k.com/download.html

DATASETS_DIR="$1"
ARCHIVES_DIR="$DATASETS_DIR/bdd100k/archives"
TMP_DIR="$DATASETS_DIR/bdd100k/tmp"

download_single() {
  local filename=$1
  local src_subdir=$2
  local dest_subdir=$3
  tmp_dir=$(mktemp -d "$TMP_DIR/$filename.XXXXXX") || { echo "Failed to create temp directory"; return 1; }
  [ -z "$tmp_dir" ] && { echo "tmp_dir == ''"; return 1; }
  echo ">>> $(date) | $filename"
  wget -nc "http://128.32.162.150/bdd100k/$filename" -O "$ARCHIVES_DIR/$filename"
  unzip "$ARCHIVES_DIR/$filename" -d "$tmp_dir"
  mkdir -p "$DATASETS_DIR/bdd100k/${dest_subdir}"
  mv "$tmp_dir/${src_subdir}"* "$DATASETS_DIR/bdd100k/${dest_subdir}"
  echo "Remaining files in tmp_dir ($(du -sh "$tmp_dir")):"
  find "$tmp_dir" -type f -print
}

download_video_part() {
  local split=$1
  local i=$2
  filename="bdd100k_videos_${split}_$(printf '%02d' "$i").zip"
  echo ">>> $(date) | $filename"
  wget -nc "http://128.32.162.150/bdd100k/video_parts/$filename" -O "$ARCHIVES_DIR/$filename"
  unzip "$ARCHIVES_DIR/$filename" -d "$DATASETS_DIR/"
  # Downloads to bdd100k/videos/.
  # WARN: Safer to do this explicitly via tmp_dir.
}

download_video_parts() {
  for i in $(seq 0 1 9); do
    download_video_part "val" "$i"
  done

  for i in $(seq 0 1 19); do
    download_video_part "test" "$i"
  done

  for i in $(seq 0 1 69); do
    download_video_part "train" "$i"
  done
}

mkdir -p "$ARCHIVES_DIR"
mkdir -p "$TMP_DIR"

download_video_parts

# Rename source -> destination since they're... very inconsistent.
download_single "bdd100k_images_10k.zip" "10k/" "images/10k/"
download_single "bdd100k_images_100k.zip" "100k/" "images/100k/"
download_single "bdd100k_seg_track_20_images.zip" "bdd100k/images/seg_track_20/" "images/seg_track_20/"
download_single "bdd100k_info.zip" "100k/" "info/100k/"
download_single "bdd100k_labels.zip" "100k/" "labels/100k/"
download_single "bdd100k_det_20_labels.zip" "" "labels/det_20/"
download_single "bdd100k_seg_maps.zip" "" "labels/seg_maps/"
download_single "bdd100k_drivable_maps.zip" "labels/" "labels/drivable_maps/"

# MOT20. Skipped, but you can download it manually if you need it.
# http://128.32.162.150/bdd100k/mot20/images20-track-*-*.zip
