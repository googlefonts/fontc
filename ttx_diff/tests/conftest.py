import pytest
from absl.testing import flagsaver

import ttx_diff.__main__  # noqa: F401 -- defines the CLI flags
from ttx_diff import core


@pytest.fixture
def cli_flags():
    """ttx-diff's CLI flags, restored after the test."""
    with flagsaver.flagsaver():
        core.FLAGS.mark_as_parsed()
        yield core.FLAGS
