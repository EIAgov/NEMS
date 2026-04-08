   
def set_EIA_theme_options(theme):

    # from style guide
    EIA_blue = "#0096d7"
    EIA_gray = "#626262"
    EIA_black = "#33333"
    EIA_font = "Jost, sans-serif"

    # from website
    # @import "https://www.eia.gov/global/survey/engine/css/layout.css";
    # @import "https://www.eia.gov/todayinenergy/styles/tie-styles.css";
    # @import "https://www.eia.gov/todayinenergy/styles/tie-print-styles.css";
    # @import "https://www.eia.gov/global/styles/global.print.min.css?v=10.1";
    # @import "https://www.eia.gov/global/styles/global.min.css?v=10.8"; */
    EIA_web_darkblue = "#00405d"
    EIA_web_green = "#5d9732"
    EIA_web_white = "#ffffff"

    # -- Options for HTML output -------------------------------------------------
    # https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

    if theme == "EIA_theme1": # Based on classic

        html_theme_options = {
            # ======================
            # classic options
            # ======================
            "rightsidebar": False, # (true or false): Put the sidebar on the right side. Defaults to False.
            'stickysidebar': True, # (true or false): Make the sidebar “fixed” so that it doesn’t scroll out of view for long body content. This may not work well with all browsers. Defaults to False.
            # "collapsiblesidebar": False, # (true or false): Add an experimental JavaScript snippet that makes the sidebar collapsible via a button on its side. Defaults to False.
            # "externalrefs": False, # (true or false): Display external links differently from internal links. Defaults to False.
            # -----------
            "footerbgcolor": EIA_web_darkblue,  # (CSS color): Background color for the footer line.
            "footertextcolor": EIA_web_white,  # (CSS color): Text color for the footer line.
            "sidebarbgcolor": EIA_web_white,  # (CSS color): Background color for the sidebar.
            # "sidebarbtncolor": EIA_blue,  # (CSS color): Background color for the sidebar collapse button (used when collapsiblesidebar is True).
            "sidebartextcolor": EIA_black,  # (CSS color): Text color for the sidebar.
            "sidebarlinkcolor": EIA_blue,  # (CSS color): Link color for the sidebar.
            "relbarbgcolor": EIA_web_darkblue,  #(CSS color): Background color for the relation bar.
            "relbartextcolor": EIA_web_white,  # (CSS color): Text color for the relation bar.
            # "relbarlinkcolor": EIA_blue,  # (CSS color): Link color for the relation bar.
            # "bgcolor": EIA_blue,  # (CSS color): Body background color.
            "textcolor": EIA_black,  # (CSS color): Body text color.
            "linkcolor": EIA_blue,  # (CSS color): Body link color.
            "visitedlinkcolor": EIA_blue,  # (CSS color): Body color for visited links.
            "headbgcolor": EIA_web_white,  # (CSS color): Background color for headings.
            "headtextcolor": EIA_web_darkblue,  # (CSS color): Text color for headings.
            "headlinkcolor": EIA_blue,  # (CSS color): Link color for headings.
            # "codebgcolor": EIA_blue,  # (CSS color): Background color for code blocks.
            # "codetextcolor": EIA_blue,  # (CSS color): Default text color for code blocks, if not set differently by the highlighting style.
            # ======================
            # additional options
            # ======================
            "bodyfont": EIA_font,  # (CSS font-family): Font for normal text.
            "headfont": EIA_font,  # (CSS font-family): Font for headings.
            "sidebarwidth": "20%",
            "globaltoc_collapse": True,
            "globaltoc_maxdepth": 3
        }

    elif theme == "EIA_theme2": # Based on Alabaster
        html_theme_options = {}

    else:
        print("Theme does not exist!")
        html_theme_options = {}

    return html_theme_options