import os
import fnmatch
from DataModel.NEMSFiles import NEMSFiles
import pandas as pd

class Model:
    _instance = None

    @staticmethod
    def getInstance():
        if Model._instance is None:
            Model()
        return Model._instance

    def __init__(self):
        if Model._instance is not None:
            raise Exception('NEMSFiles is a singleton class.')
        else:
            Model._instance = self

    def load_files(self, dir):
        self.files = NEMSFiles(dir)
