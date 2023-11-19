# Note: Due to misconfigured git (easy to do) everything had been committed as LF. This script
# switches CRLF files back to LF files, which we do NOT want to do for Delphi project files in
# particular because RAD Studio says it requries CRLF line endings. Unfortunately that means that
# commit 136ecd8 had to change all the Delphi files line endings to CRLF in order to be consistent
# with other projects and so that the repo itself had line endings in the right format.
# Shouldn't need to run this script again.
Get-ChildItem -Recurse -Include *.html, *.js, *.json, *.css, *.ts, LICENSE, *.php, *.c, *.cpp, .gitignore, *.md | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $unixContent = $content -replace "`r`n", "`n"
    Set-Content -Path $_.FullName -Value $unixContent -NoNewline
}