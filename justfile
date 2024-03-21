open:
    zathura -x "code-insiders --no-sandbox -r -g %{input}:%{line}" thesis.pdf

data:
    cd code; quarto render data_commuting.qmd --to pdf --output $(mktemp --suffix ".pdf")
    cd code; quarto render data_rki.qmd --to pdf --output $(mktemp --suffix ".pdf")