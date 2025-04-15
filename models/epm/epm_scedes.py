"""Interface for finding the keys.sed file and accessing its key-value pairs.

This module contains the `Scedes` class, which can be used to read the keys.sed
file from disk and store its contents for lookup later. The class automatically
locates the keys.sed file.
"""

from pathlib import Path
from typing import Final

from epm_common import get_epm_path, get_input_path, running_integrated


KEYS_SED: Final[str] = "keys.sed"


class Scedes:
    """A thin wrapper for a dict containing the scedes file information."""

    def __init__(self) -> None:
        """Initialize a new scedes file dict.

        The dict is initially empty and not associated with any file.
        """
        self._scedes: dict[str, str] = {}
        self.path: Path | None = None

    def read_file(self) -> None:
        """Read in the data from the keys.sed file on disk.

        This method will try to locate the keys.sed file and parse its
        contents. The file's path will be assigned to the instance's `path`
        attribute.
        """
        if running_integrated():
            self.path = get_epm_path().parent / KEYS_SED
        else:
            self.path = get_input_path() / KEYS_SED

        with self.path.open("r", encoding="utf-8") as f:
            for line in f:
                if line == "" or line.isspace():
                    continue  # Skip any blank lines
                key, value = line.rstrip('\n').split('=')
                if value == "nullstr":  # Scedes null string token
                    value = ""
                self._insert(key, value)

    def _insert(self, key: str, value: str) -> None:
        """Insert a new key-value pair into the internal scedes dict.

        Using this method ensures that all keys are uppercase and no
        duplicate keys are included.

        Parameters
        ----------
        key : str
            Key to be inserted, which will be converted to uppercase.
        value : str
            Value to be inserted.

        Raises
        ------
        ValueError
            If `key` already exists in the scedes dict.
        """
        if key in self:
            raise ValueError(f"found duplicate key {key.upper()!r} in scedes")
        self._scedes[key.upper()] = value

    def __contains__(self, key: str) -> bool:
        """Determine whether a scedes key is present.

        Parameters
        ----------
        key : str
            Scedes key to check (case insensitive).

        Returns
        -------
        bool
            True if the key is present and False otherwise.
        """
        return key.upper() in self._scedes

    def __getitem__(self, key: str) -> str:
        """Retrieve the value associated with a scedes key.

        Parameters
        ----------
        key : str
            Scedes key to look up (case insensitive).

        Returns
        -------
        str
            Value associated with the given key.

        Raises
        ------
        KeyError
            If the scedes key was not present.
        """
        if key not in self:
            raise KeyError(f"scedes key {key.upper()!r} is not present")
        return self._scedes[key.upper()]

    def get(self, key: str, default: str) -> str:
        """Get the value for a scedes key, or return a default if not present.

        Parameters
        ----------
        key : str
            Scedes key to look up (case insensitive).
        default : str
            Value to return if the scedes key is not present.

        Returns
        -------
        str
            The value from the scedes file or `default` as appropriate.
        """
        try:
            return self[key]
        except KeyError:
            return default
