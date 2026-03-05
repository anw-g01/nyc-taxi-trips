
# run the setup script

source(here::here("scripts", "00_setup.R"))

# run all project scripts

scripts <- c(
    "01_import.R",
    "02_clean.R",
    "03_prepare.R"
)

for (script in scripts) {
    source(here("scripts", script))
}