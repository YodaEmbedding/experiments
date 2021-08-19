import os
import subprocess
import sys

ghostscript_path = r'C:\Program Files\gs\gs9.20\bin\gswin64c.exe'
convert_path = r'C:\Program Files\ImageMagick-7.0.3-Q16\convert.exe'

for input_path in sys.argv[1:]:
	root, filename = os.path.split(input_path)
	name, ext = os.path.splitext(filename)

	inverted_path = os.path.join(root, name + "_inverted" + ext)
	output_path = os.path.join(root, name + "_output" + ext)

	subprocess.call([ghostscript_path,
		'-o', inverted_path,
		'-sDEVICE=pdfwrite',
		'-c', '{1 exch sub}{1 exch sub}{1 exch sub}{1 exch sub} setcolortransfer',
		'-f', input_path])

	subprocess.call([convert_path,
		'-brightness-contrast', '3,-18',
		'-compress', 'lzw',
		'-density', '300',
		'-colorspace', 'Gray',
		inverted_path,
		output_path])
