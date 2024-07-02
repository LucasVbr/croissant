<h1 align="center">Croissant 🥐</h1>
<p>
  <img alt="Version" src="https://img.shields.io/badge/version-0.0.1-blue.svg?cacheSeconds=2592000" />
  <a href="./LICENSE" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
</p>

> Custom programming language in french

## Install

This project uses [Dune](https://dune.build/) as a build system. You can install it with the following command:

Install Dune with the following command:
```sh
opam install dune
```

Generate the `.opam` file with the following command if it doesn't exist:
```sh
dune build croissant.opam
```

Install the dependencies with the following command:
```sh
opam install . --deps-only
eval $(opam env)
```

## Usage

Run the following commands to build and run the project:

```sh
dune build
echo "1 + 3;" | dune exec croissant
```

## Run tests

```sh
dune test
```

## Author(s) 👤

- **[@LucasVbr](https://github.com/LucasVbr)**

## Show your support

Give a ⭐️ if this project helped you!

## 📝 License

Copyright © 2024 [LucasVbr](https://github.com/LucasVbr).<br />
This project is [MIT](LICENSE) licensed.