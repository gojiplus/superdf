"""Read and validate studyframe bundles without executing R code."""

from .reader import ResearchData, read_studyframe

__all__ = ["ResearchData", "read_studyframe"]
