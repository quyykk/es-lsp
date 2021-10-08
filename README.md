# es-lsp

An unofficial [LSP server](https://microsoft.github.io/language-server-protocol/) for [Endless Sky](https://github.com/endless-sky/endless-sky/).

## Building

You need [meson](https://mesonbuild.com/), which can typically be installed using `pip` or your favorite package manager.

```bash
$ meson build --buildtype release # creates a build directory for the 'release' target.
$ cd build
$ ninja
```
