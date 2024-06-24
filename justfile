open:
    zathura -x "code-insiders --no-sandbox -r -g %{input}:%{line}" thesis.pdf

data:
    cd code; quarto render data_commuting.qmd --to pdf --output $(mktemp --suffix ".pdf")
    cd code; quarto render data_rki.qmd --to pdf --output $(mktemp --suffix ".pdf")

commit_yesterday:
    git commit --amend --date="yesterday 8PM" --no-edit

clean:
    rm -f thesis.{acn,aux,auxlock,bbl,bcf,fdb_latexmk,fls,glo,ist,log,out,run.xml,synctex.gz,toc,bbl-SAVE-ERROR,bcf-SAVE-ERROR}
    rm -rf $(biber --cache)
