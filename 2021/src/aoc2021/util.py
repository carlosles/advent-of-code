"""Shared utilities."""

from pathlib import Path
from typing import Callable


def read_as_list(fpath: Path, func: Callable = str) -> list:
    """Return file contents as list of lines mapped using `func`."""
    with open(fpath, 'r') as f:
        return [func(line) for line in f]
