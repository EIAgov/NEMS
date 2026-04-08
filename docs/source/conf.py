# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'National Energy Modeling System'
copyright = '2025, U.S. Energy Information Administration'
author = 'U.S. Energy Information Administration'

import os, sys
import sphinx_rtd_theme

sys.path.insert(0, os.path.abspath(os.path.join("..", "..","source")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","reporter")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","reporter",'RW Tables')))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","converge")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","main")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","ngpl")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","epm")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","models","ccats")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","scripts")))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","scripts",'pyfiler')))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","scripts",'setup')))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..","scripts",'setup','src')))
sys.path.insert(0, os.path.abspath(os.path.join("..", "..")))


project = "National Energy Modeling System (NEMS)"
copyright = "2024, Energy Information Administration"
author = "U.S. Energy Information Administration"

master_doc = 'index'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    "sphinx.ext.napoleon",
    "sphinx.ext.autodoc",
    'sphinx.ext.mathjax',
    'sphinx.ext.todo',
    "sphinx.ext.autosummary",
    "sphinx.ext.todo",
    "sphinx.ext.viewcode",
    'sphinx.ext.autosectionlabel',
    'sphinxcontrib.bibtex'
]

bibtex_bibfiles = ['refs.bib']

autosummary_generate = True
autosummary_imported_members = True

numpydoc_class_members_toctree = False
numpydoc_show_class_members = False

autodoc_default_options = {
    'members': True,
    'private-members': True,
    'undoc-members' :True,
    'member-order': 'bysource',
    'special-members': '__init__',
    'exclude-members': '__weakref__'

    }

templates_path = ["_templates"]
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

# html_theme = "alabaster"
# html_theme = "sphinx_rtd_theme"


################################### FROM JEFF

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

##################################


html_static_path = ['_static', '_static/images', '_static/css']

numfig = True
numfig_sequence_per_section = True

numfig_format = {
    'code-block': 'Listing %s',
    'figure': 'Figure %s.',
    'section': 'Section',
    'table': 'Table %s.',
}

primary_domain = 'py'

suppress_warnings = ['autosectionlabel.*']

rst_epilog = """
   .. role:: small
      :class: small
"""

def setup(app):
    app.add_css_file('css/custom.css')





# =======================
# LaTeX pdf
# =======================
 
latex_engine = 'pdflatex'
latex_documents = [('index','hsm.tex', u'hsm', u'hsm', 'manual')]
latex_elements = {
    'papersize': 'letterpaper',
    'pointsize': '11pt',
    'preamble': r'''
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{tabulary}
''',
    'maketitle': r'''
\begin{titlepage}
\begin{center}
\vspace*{40mm}
{\LARGE \textbf{Your Project Title} \par}
\vspace*{20mm}
{\large Your Name \par}
\vspace*{20mm}
{\large \today \par}
\end{center}
\end{titlepage}
''',
}
latex_show_urls = 'footnote'


