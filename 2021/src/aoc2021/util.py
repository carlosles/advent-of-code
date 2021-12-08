"""Shared utilities."""

from pathlib import Path
from typing import Callable, Any


def read_as_list(fpath: Path, func: Callable[[str], Any] = str) -> list:
    """Return file contents as list of lines mapped using `func`."""
    with open(fpath, 'r') as f:
        return [func(line) for line in f]


def chunks(xs: list, n: int) -> list[list]:
    """Yield successive n-sized chunks from list."""
    return [xs[i:i+n] for i in range(0, len(xs), n)]


def invert_dict(d: dict[Any, Any]) -> dict[Any, Any]:
    """Return dictionary with keys and values inverted."""
    return {val: key for key, val in d.items()}
