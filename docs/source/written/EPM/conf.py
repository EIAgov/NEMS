# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

import os
import sys
sys.path.insert(0, os.path.abspath('../..'))  # Needed so that Sphinx can find EPM

project = 'EPM'
copyright = '2025, U.S. Energy Information Administration'
author = 'U.S. Energy Information Administration'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['sphinx.ext.mathjax',
              'sphinx.ext.todo',
              'sphinx.ext.autodoc',
              'sphinx.ext.napoleon',
              'sphinx.ext.autosectionlabel']

templates_path = ['_templates']
exclude_patterns = []

# configure todo extension, https://www.sphinx-doc.org/en/master/usage/extensions/todo.html#confval-todo_include_todos
todo_include_todos = True

autodoc_default_options = {
    'members': True,
    'member-order': 'bysource',
    'special-members': '__init__',
    'undoc-members': True,
    'exclude-members': '__weakref__'
}
# automatically number figures and tables
numfig = True
numfig_format = {
    'code-block': 'Listing %s',
    'figure': 'Figure %s.',
    'section': 'Section',
    'table': 'Table %s.',
}

# =======================
# Use EIA theme 
# =======================
# user inputs
EIA_themes_path = os.path.abspath("../EIA_themes") # path to EIA_themes directory
html_theme = 'EIA_theme1' # Options are 'EIA_theme1' or 'EIA_theme2'

# ---------------------------------------------------------------------------
# additional EIA theme settings (not intended to be modified)
html_theme_path = [EIA_themes_path]
html_logo = os.path.join(EIA_themes_path, "logos", "logo_files_forInsideEIAlogopage_fulllogo.png")

# set options
sys.path.insert(0, os.path.abspath(EIA_themes_path)) # Needed to find customize_theme.py
from customize_theme import set_EIA_theme_options
html_theme_options = set_EIA_theme_options(html_theme)

# additionall CSS
html_static_path = [os.path.join(EIA_themes_path, html_theme, "_static")]
html_css_files = ["custom.css"]