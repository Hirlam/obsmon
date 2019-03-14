# Readme
Obsmon is a tool for observation monitoring in the [Harmonie-Arome NWP
System](http://hirlam.org/). It is composed of two parts: The backend
and the frontend. The backend is part of the scripting system and produces
databases with the relevant information as part of the post-processing.
The frontend allows the analysis and visualization of this data. ***This
software, and all files contained here, correspond only to the frontend
part of the full Obsmon package***. In this document, however, "obsmon"
will always refer to the frontend part, unless otherwise specified.

Obsmon is implemented as a web interface written in
[R](https://www.r-project.org/), built on the
[Shiny](https://shiny.rstudio.com/) web application framework.
It can be run as a standalone server, or inside the [Shiny
Server](https://www.rstudio.com/products/shiny/shiny-server/).

For instructions on how to configure and run obsmon, please read the
[obsmon_documentation.pdf file](./docs/obsmon_documentation.pdf) located
under the `docs` directory. Noteworthy changes to the code are listed in
the [CHANGELOG.md file](./docs/CHANGELOG.md) located under the same directory.
