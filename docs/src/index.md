Introduction
============

Obsmon is a tool for observation monitoring in the [Harmonie-Arome NWP
System](http://hirlam.org/index.php/documentation/harmonie). It allows
the analysis and visualisation of data produced by `prepObsmon`. *This
document refers only to `obsmon`, and not to `prepObsmon`*.

Obsmon is written in [R](https://www.r-project.org) using the
[Shiny](https://shiny.rstudio.com) web application framework. It can be
deployed as a local, standalone application or even remotely through the
use of a web server (for instance, using a [Shiny
Server](https://www.rstudio.com/products/shiny/shiny-server)). This
document contains information about how to get
([2.1](@ref getting-the-source), install
([3](@ref install) and
configure ([4](@ref config-file) obsmon, as well as tips on how to use some
of the non-trivial features of the code
([5](@ref usage)). If you
find that something is missing or needs to be corrected, please feel
free to contact us.

