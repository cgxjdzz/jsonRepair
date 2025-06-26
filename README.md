# jsonRepair  






*A lightweight R package to automatically fix malformed JSON strings—especially those produced by Large Language Models (LLMs).*  


> *"Some LLMs are a bit iffy when it comes to returning well‑formed JSON data—sometimes they skip a parenthesis, sometimes they add an extra word. Luckily, the mistakes LLMs make are simple enough to be fixed without destroying the content… I searched for a lightweight R package that was able to reliably fix this problem but couldn't find any. So I copy one."*

## ✨ Key Features

* **Self‑healing JSON** – Detects and repairs the most common structural issues: missing/extra commas, mismatched brackets, single quotes, comments, unescaped control characters, and more.
* **`jsonlite` friendly** – Optionally returns an R object via `jsonlite::fromJSON()` or the repaired JSON text.
* **Zero heavy dependencies** – Only needs base R and `jsonlite`.

## 🧑‍💻 Why This Package?

1. **LLMs love to bend the JSON spec.** If you call an LLM from R and ask it to emit JSON, chances are you'll face dangling commas, stray quotes, or plain text mixed in the payload.
2. **Existing solutions are Python‑only.** The excellent [`json_repair`](https://github.com/mangiucugna/json_repair) library inspired this work, but I needed the same logic in native R for seamless data pipelines.
3. **One‑to‑one logic port.** This package is a *line‑by‑line* re‑implementation of the original algorithm, translated from Python to R.

## 🚀 Installation

```r
# install.packages("devtools") if you haven't yet
# install directly from GitHub
remotes::install_github("cgxjdzz/jsonRepair")
```

## ⚡ Quick Start

```r
library(jsonRepair)

broken <- '{"number": 1,"reason": "According...""ans": "YES"}'

# Return as an R list (default)
repaired_obj <- repair_json(broken)
print(repaired_obj)
#> $number
#> [1] 1
#> $reason
#> [1] "According..."
#> $ans
#> [1] "YES"

# Or get the repaired JSON string
repaired_txt <- repair_json(broken, return_objects = FALSE)
cat(repaired_txt)
# {"number":1,"reason":"According...","ans":"YES"}
```
