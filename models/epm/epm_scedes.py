"""Interface for handling the scedes data and accessing its key-value pairs.

The class defined in this module is a thin wrapper around a dict. It mediates
EPM's access to the scedes data and also handles loading the scedes data (which
may include manually parsing the scedes file from disk for standalone runs).
"""

from pathlib import Path
from typing import Final

from epm_common import get_epm_path, get_input_path, running_integrated


SCEDES_FILE_NAME: Final[str] = "scedes.all"
SCEDES_ALL:  Final[str] = "scedes.all"


class Scedes:
    """A wrapper class for a dict containing the scedes file information."""

    def __init__(self, initializer: dict | None = None) -> None:
        """Create a new, empty scedes dict object.

        After creating an empty object, call the `read` method to fill it with
        data. The `initializer` argument controls the data source that will be
        used when `read` is called.

        Parameters
        ----------
        initializer : dict | None, optional
            The scedes data source to be used when `read` is called. If a dict
            is passed, then its key-value pairs will be copied. Otherwise, the
            default behavior is to attempt to read the scedes file from disk.
        """
        self._scedes_dict: dict[str, str] = {}

        if isinstance(initializer, dict):
            self._initializer = initializer
        else:
            self._initializer = self._find_path()

    @staticmethod
    def _find_path() -> Path:
        """Try to locate the scedes file on disk for manual parsing.

        Returns
        -------
        Path
            Possible path to the scedes file.
        """
        if running_integrated():
            return get_epm_path().parent / SCEDES_FILE_NAME
        return get_input_path() / SCEDES_FILE_NAME

    def read(self) -> None:
        """Read the scedes data to finish initializing a new object.

        The data source is determined by the `initializer` argument passed to
        the `__init__` method when the object was first created.
        """
        if isinstance(self._initializer, dict):
            self._load_dict(self._initializer)
        else:
            self._load_file(self._initializer)

    def _load_dict(self, initializer_dict: dict) -> None:
        """Copy the scedes data from an existing dict object in memory.

        Parameters
        ----------
        initializer_dict : dict
            Dict to copy keys and values from. This will usually be the SCEDES
            attribute from the NEMS user object.
        """
        for key, value in initializer_dict.items():
            key = str(key).upper()
            value = str(value)
            self._scedes_dict[key] = value

    def _load_file(self, initializer_path: Path) -> None:
        """Manually parse the scedes data from a scedes file on disk.

        Parameters
        ----------
        initializer_path : Path
            File path to open for reading.
        """
        with initializer_path.open("r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line == "":
                    continue  # Skip blank lines
                key, value = line.split("=")
                key = key.upper()
                if key in self._scedes_dict:
                    continue  # Ignore duplicate keys
                self._scedes_dict[key] = value

    def __getitem__(self, key: str) -> str:
        """Retrieve the value associated with a scedes key.

        Parameters
        ----------
        key : str
            Case-insensitive scedes key.

        Returns
        -------
        str
            Associated value from the scedes.
        """
        key = key.upper()
        return self._scedes_dict[key]

    def get(self, key: str, default: str) -> str:
        """Get the value for a scedes key, or return default if not present.

        Parameters
        ----------
        key : str
            Case-insensitive scedes key.
        default : str
            Value to return if the key is not present.

        Returns
        -------
        str
            Associated value from the scedes file or `default`.
        """
        try:
            return self[key]
        except KeyError:
            return default
