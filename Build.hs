import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/index.html"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    "_build/index.html" %> \out -> do
        putNormal "Building with Pandoc"
        need ["README.md", "bibliography.yaml"]

        cmd "pandoc"
            "--bibliography=bibliography.yaml"
            "--output" out
            "--smart"
            "--standalone"
            "--table-of-contents"
            "--to=html"
            "--toc-depth=2"
            "README.md"
