# VSCode Extension

This is very hacky and probably not a great experience right now; it was
intended as a proof of concept. Use at your peril.


Installation:

this extension is not published, and has to be run manually. First, you will
need to compile the `fea-lsp` crate in the parent directory, and move the
binary somewhere where you can easily find it, such as `~/.fea-lsp/fea-lsp`.

Then from from this directory, run:

```sh
npm install
npm run compile
```

Open this directory in VSCode. From the 'run and debug' sidebar, run the 'Launch
Client' command. This will open a new VSCode window with the extenstion enabled.

To actually make it work, you need to point it the location of the `fea-lsp`
binary you compiled earlier. Open VSCode settings, and search for 'fea'. At the
bottom you should find a setting "fea-lsp > Server: Path", where you can enter
the path to the binary.
