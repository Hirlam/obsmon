# Readme

Obsmon is a tool for observation monitoring in the [Harmonie-Arome NWP
System](http://hirlam.org). It is composed of two parts: The backend
and the frontend. The backend is part of the scripting system and produces
databases with the relevant information as part of the post-processing.
The frontend allows the analysis and visualization of this data. ***This
software, and all files contained here, correspond only to the frontend
part of the full Obsmon package***. In this document, however, "obsmon"
will always refer to the frontend part, unless otherwise specified.

Obsmon is implemented as a web interface written in
[R](https://www.r-project.org), built on the
[Shiny](https://shiny.rstudio.com) web application framework.
It can be run as a standalone server, or inside the [Shiny
Server](https://www.rstudio.com/products/shiny/shiny-server).

For instructions on how to configure and run obsmon, please read the
[obsmon documentation](https://hirlam.github.io/obsmon/). *Please pay special attention to the section
on the installation of system dependencies*. Noteworthy changes to the
code are listed in the [CHANGELOG.md file](./docs/CHANGELOG.md) 


### Repo Info

This repo was created using code imported from [hirlams's internal obsmon repo](git@hirlam.org:Obsmon)
on 2021-02-25. The imported code was restructured for publication on GitHub.
*The hirlam internal obsmon repo will no longer be updated.*

Git history was kept for reference, but a few commits made prior to the import date were
edited to remove some large files that no longer are needed. Due to this, the commit IDs
in this repo do not match those in the [parent one](git@hirlam.org:Obsmon).

The commit hash of the parent repo's  HEAD at the time of import was
`8907238c702bc0c178fdcb02455c4954fd31e49d`.

*N.B.:* If you wish/need to install Obsmon versions earlier than v4.0.0, please use
code from the [hirlams's internal git repo](git@hirlam.org:Obsmon).
