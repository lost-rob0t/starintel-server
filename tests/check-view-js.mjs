import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";

const directory = new URL("../source/views/", import.meta.url);
const files = (await readdir(directory)).filter((name) => name.endsWith(".json")).sort();

function compile(source, label) {
  if (typeof source !== "string") return;
  try {
    Function("emit", "index", `"use strict"; return (${source});`);
  } catch (error) {
    error.message = `${label}: ${error.message}`;
    throw error;
  }
}

for (const name of files) {
  const document = JSON.parse(await readFile(join(directory.pathname, name), "utf8"));
  for (const [viewName, view] of Object.entries(document.views || {})) {
    compile(view.map, `${name}:${viewName}:map`);
    if (view.reduce && !String(view.reduce).startsWith("_")) {
      compile(view.reduce, `${name}:${viewName}:reduce`);
    }
  }
  for (const [indexName, index] of Object.entries(document.indexes || {})) {
    compile(index.index, `${name}:${indexName}:index`);
  }
}

console.log(`compiled ${files.length} CouchDB design documents`);
