import * as esbuild from "esbuild"
import fs from "fs"
import { join } from "path"

const entryDir = "./output"
const outDir = "./public"

const entryPoints = fs.readdirSync(entryDir)
  .filter(d => d.startsWith("EntryPoints."))
  .map(f => join(entryDir, f, "index.js"))

const ctx = await esbuild.context({
  entryPoints,
  outdir: outDir,
  bundle: true,
  format: "iife",
  globalName: "Main",
  sourcemap: true,
})

fs.cpSync("assets", `${outDir}/assets`, { recursive: true })

entryPoints.forEach(file => {
  const scriptName = file.replace(/^.*EntryPoints\./, "").replace(/\/index.js$/, "")
  const name = scriptName.split(/(?=[A-Z])/).map(s => s.toLowerCase()).join("_")
  const htmlFile = join(outDir, `${name}.html`)
  const content = fs.readFileSync(
    "index.html",
    { encoding: "utf-8" }
  ).replace("{{script_name}}", scriptName)
  fs.writeFileSync(htmlFile, content)
})

await ctx.watch()

console.log("👀 Watching for changes and serving at http://localhost:8000")
