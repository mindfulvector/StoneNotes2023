Get-ChildItem -Recurse -Include *.html, *.js, *.json, *.css, *.ts, LICENSE, *.php, *.c, *.cpp, .gitignore, *.md | ForEach-Object {
    $content = Get-Content $_.FullName -Raw
    $unixContent = $content -replace "`r`n", "`n"
    Set-Content -Path $_.FullName -Value $unixContent -NoNewline
}