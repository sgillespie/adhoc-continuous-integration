import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/index.html"]

    phony "clean" $
        removeFilesAfter "_build" ["//*"]

    "_build/index.html" %> \out -> do
        templates <- getDirectoryFiles "" ["templates/*"]

        need $ ["README.md", "bibliography.yaml"] ++ templates

        cmd "pandoc"
            "--bibliography=bibliography.yaml"
            "--output" out
            "--smart"
            "--standalone"
            "--table-of-contents"
            "--template" templates
            "--to=html"
            "--toc-depth=2"
            "README.md"
