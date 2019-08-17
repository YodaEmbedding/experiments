#!/bin/bash

git clone https://github.com/SicariusNoctis/frece
cd frece || exit

# Setup
ls >files.txt
file -b --mime-type -f files.txt >mimetypes.txt
sed -e "
    s/\(inode\/directory\)/<\!-- & --> <span lang='folder' foreground='#3daee9' face='Font Awesome'>\&#xf07b;<\/span>/
    s/\(text.*\)/<\!-- & --> <span lang='file-text' foreground='#ffcfa0' face='Font Awesome'>\&#xf15c;<\/span>/
" mimetypes.txt >mimetypes_sed.txt
pr -tmJ mimetypes_sed.txt files.txt >entries.txt
frece init files.db entries.txt

# Usage
item=$(frece print files.db | rofi -dmenu -markup-rows)
if [[ -n $item ]]; then
    frece increment files.db "$item"
    filepath=$(sed -e 's/^<!-- [^ ]* --> <span[^>]*>[^<]*<\/span[^>]*>\t//g' <<<"$item")
    mimetype=$(sed -e 's/^<!-- \([^ ]*\) --> .*$/\1/g' <<<"$item")

    echo ""
    echo "filepath: $filepath"
    echo "mimetype: $mimetype"
    echo ""
    frece print files.db --verbose
fi
