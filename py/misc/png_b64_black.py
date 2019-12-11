import base64
from io import BytesIO

import numpy as np
from PIL import Image

def b64_png(arr: np.ndarray) -> str:
    img = Image.fromarray(arr)
    with BytesIO() as buffer:
        img.save(buffer, "png")
        raw = base64.b64encode(buffer.getvalue()).decode("utf8")
    return f"data:image/png;base64,{raw}"

def main():
    h, w = 224, 224
    arr = np.zeros((h, w), dtype=np.uint8)
    print(b64_png(arr))

main()
