# File Exchange server

## Docker

Build it:

```sh
docker build -t file-exchange .
```

Run it:

```sh
docker run -it -p3000:3000 -v/path/to/folder:/exchange file-exchange
```
