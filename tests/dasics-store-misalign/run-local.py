#!/usr/bin/env python3
"""Freeze released store-misalignment inputs and run the selected validation suite."""

from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "dasics-validation"))
from runner import main

if __name__ == "__main__":
    sys.exit(main(Path(__file__).resolve().parent))
