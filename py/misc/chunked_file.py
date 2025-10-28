from itertools import count
from pathlib import Path


class ChunkedFile:
    def __init__(self, filepath: str | Path) -> None:
        self.filepath = Path(filepath)
        self.parts_dir = self.filepath.parent / f"{self.filepath.name}.parts"

    def split(self, chunk_size: int = 16 * 1024 * 1024) -> None:
        self.parts_dir.mkdir(parents=True, exist_ok=True)

        for part in self.parts_dir.iterdir():
            part.unlink()

        with self.filepath.open("rb") as f:
            for idx in count():
                chunk = f.read(chunk_size)
                if not chunk:
                    break
                part = self.parts_dir / f"{idx:04d}"
                part.write_bytes(chunk)

    def ensure(self) -> None:
        if self.filepath.is_file():
            return

        if not self.parts_dir.is_dir():
            raise FileNotFoundError(self.filepath)

        with self.filepath.open("wb") as f:
            for idx in count():
                part = self.parts_dir / f"{idx:04d}"
                if not part.is_file():
                    break
                chunk = part.read_bytes()
                f.write(chunk)
