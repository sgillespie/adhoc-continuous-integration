import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/index.html"]

    -- Common Pandoc args
    let args = [
            "--bibliography=docs/bibliography.yaml",
            "--smart",
            "--standalone",
            "--table-of-contents",
            "--toc-depth=2",
            "docs/metadata.yaml",
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
