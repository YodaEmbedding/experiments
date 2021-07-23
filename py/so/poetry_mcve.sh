#!/bin/bash

mkdir myproj
cd myproj

eval "$(pyenv init -)"
pyenv install 3.8.9
pyenv local 3.8.9

poetry init --no-interaction --python="3.8.9"
poetry env use 3.8.9
poetry add numpy

echo '
import sys
print(sys.version)

import numpy
print(numpy.__version__)
' > main.py
