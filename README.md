# GraphQL Data Specification

The specification is written in `gds-spec.md` and uses [spec-md][https://spec-md.com].

## Build & preview

```bash
npm i -g spec-md http-server

# build
spec-md gds-spec.md > html/index.html

# preview
cd html
http-server .

# Open browser at localhost:8080
```

## Deploy

The HTML in the html/ directory is automatically deployed via GitHub pages.
