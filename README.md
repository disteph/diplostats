# DiploStats

DiploStats collects completed-game data from
[webDiplomacy](https://webdiplomacy.net/) and
[vDiplomacy](https://vdiplomacy.com/), then calculates per-country statistics
for a Diplomacy variant. It can be used as a command-line program or run as an
HTTP server.

The generated table includes each country's average ranking, share of
victories, average number of supply centres, and average number of units. By
default, DiploStats searches won, anonymous, no-messaging games on both sites.

## Requirements

- OCaml and opam
- Dune 2.7 or newer
- Internet access at run time, so game listing pages can be downloaded

The project has been tested with OCaml 5.0 and Dune 3.9. Create a local opam
switch and install the build dependencies with:

```sh
opam switch create . 5.0.0
eval "$(opam env)"
opam install dune containers yojson lwt cohttp cohttp-lwt \
  cohttp-lwt-unix tls-lwt lambdasoup ppx_deriving webmachine sexplib0
```

You may use an existing compatible switch instead. If the switch is already
active, only the `opam install` command is needed.

## Build

From the repository root, run:

```sh
dune build
```

This produces the stand-alone tool at `_build/default/src/main.exe` and the
server at `_build/default/src/server.exe`. The Dune configuration also promotes
copies named `main.exe` and `server.exe` into the repository root. `make build`
is an equivalent shorthand, and `make clean` removes build output.

## Run as a stand-alone tool

Pass the variant identifier used by the source sites:

```sh
dune exec ./src/main.exe -- Classic > classic.html
```

Open `classic.html` in a browser to view the resulting table. The program logs
collection progress to standard error and writes the HTML result to standard
output.

To stop when a particular older game is reached, supply its exact name as a
second positional argument:

```sh
dune exec ./src/main.exe -- Classic "Name of older game" > classic.html
```

Options must precede the positional arguments:

```text
-verb N            Set logging verbosity (default: 0)
-finished          Include all finished games instead of won games only
-nono_messaging    Exclude no-messaging games
-norm_messaging    Include normal-messaging games
-rule_messaging    Include rulebook-messaging games
-pub_messaging     Include public-messaging games
-no-anonymity      Include non-anonymous games instead of anonymous games
-novdiplo          Do not query vDiplomacy
-nowebdiplo        Do not query webDiplomacy
```

For example, this queries only vDiplomacy for finished Classic games with
normal messaging:

```sh
dune exec ./src/main.exe -- -finished -nono_messaging -norm_messaging \
  -nowebdiplo Classic > classic.html
```

Fetching continues through every available result page, so variants with a
large game history can take some time. A site or page that cannot be fetched
within ten seconds is skipped.

## Run as a server

Start the server on the default TCP port, 8080:

```sh
dune exec ./src/server.exe
```

To choose another port:

```sh
dune exec ./src/server.exe -- -port 9000
```

The server accepts `GET /:options`. Put URL-encoded `key=value` pairs in the
single path segment, separated by `&`. The required key is `variant`; all
others are optional:

```sh
curl -H 'Accept: text/html' \
  'http://localhost:8080/variant=Classic'

curl -H 'Accept: application/json' \
  'http://localhost:8080/variant=Classic&finished=true&nowebdiplo=true'
```

Supported response types are `text/html`, `text/plain`, and
`application/json`, selected with the `Accept` header. Server option keys and
values are:

| Key | Value | Effect |
| --- | --- | --- |
| `variant` | string | Variant identifier (required) |
| `older` | string | Stop at this exact older game name |
| `verb` | integer | Logging verbosity |
| `finished` | boolean | Include all finished games |
| `nono_messaging` | boolean | Exclude no-messaging games |
| `norm_messaging` | boolean | Include normal-messaging games |
| `rule_messaging` | boolean | Include rulebook-messaging games |
| `pub_messaging` | boolean | Include public-messaging games |
| `no_anonymity` | boolean | Include non-anonymous games instead |
| `novdiplo` | boolean | Disable vDiplomacy |
| `nowebdiplo` | boolean | Disable webDiplomacy |

Boolean values must be `true` or `false`. Percent-encode spaces and other
reserved characters in values; `+` is also decoded as a space. For example:

```sh
curl 'http://localhost:8080/variant=Classic&older=An+Older+Game'
```

The server binds to all interfaces. If it should only be locally accessible,
apply the appropriate host firewall or container port-binding rules.

## License

DiploStats is available under the [MIT License](LICENSE).
