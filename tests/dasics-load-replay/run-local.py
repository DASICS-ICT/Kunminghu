#!/usr/bin/env python3
"""Run the task's explicitly frozen Scala validation inputs."""

from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "dasics-validation"))
from runner import main

if __name__ == "__main__":
    sys.exit(main(Path(__file__).resolve().parent))
