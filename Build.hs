import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["README.md", "_build/index.html"]

    let title    = "Adhoc Continuous Integration"
        subtitle = "Building a CI tool from the ground up with Docker"
        -- Common Pandoc args
        args     = [
            "--bibliography=docs/bibliography.yaml",
            "--smart",
            "--standalone",
            "--table-of-contents",
            "--toc-depth=2",
            "--variable", "title:" ++ title,
            "--variable", "subtitle:" ++ subtitle,
            "docs/adhoc-continuous-integration.md"]

    phony "clean" $
        removeFilesAfter "_build" ["//*"]

    "_build/index.html" %> \out -> do
        inputs <- getDirectoryFiles "" ["docs/*"]

        need inputs

        cmd "pandoc"
            "--output" out
            "--template=docs/github.template.html5"
            "--to=html"
            args 

    "README.md" %> \out -> do
        inputs <- getDirectoryFiles "" ["docs/*"]

        need inputs

        cmd "pandoc" 
            "--output" out
            "--template=docs/template.md"
            "--to=markdown"
            args
