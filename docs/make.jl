using Documenter


pages = [
    "Introduction" => "index.md", 
    "Getting Obsmon" => "Getting_obsmon.md",
    "Installing, Execturing updating" => "Installing.md",
    "The config file" => "configfile.md",
    "Using Obsmon" => "usingObsmon.md",
    "Frequently asked Questions" => "faq.md",
    "Getting Support" => "gettingSupport.md",
    "Collaborators" => "Collaborators.md"
]


prettyurls = get(ENV, "CI", nothing) == "true" 

format = Documenter.HTML(prettyurls = prettyurls,inventory_version="")   

makedocs(
    sitename = "Obsmon",
    format = format,
    pages = pages
)

deploydocs(
    repo = "github.com/Hirlam/obsmon.git",
    devbranch = "master",   
    devurl = "master",
)
