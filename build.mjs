import { build } from "esbuild"
import fs from "fs"
import { join } from "path"

const entryDir = "./output"
const outDir = "./public"

const entryPointDirs = fs.readdirSync(entryDir).filter(d => d.startsWith("EntryPoints."))
const entryPoints = entryPointDirs.map(f => join(entryDir, f, "index.js"))

await build({
  entryPoints,
  outdir: outDir,
  bundle: true,
  format: "iife",
  globalName: "Main",
  sourcemap: true,
  minify: true,
})

fs.cpSync("assets", join(outDir, "assets"), { recursive: true })

entryPoints.forEach(file => {
  const scriptName = file.replace(/^.*EntryPoints\./, "").replace(/\/index.js$/, "")
  const name = scriptName.split(/(?=[A-Z])/).map(s => s.toLowerCase()).join("_")
  const htmlFile = join(outDir, `${name}.html`)
  const content = fs.readFileSync(
    "entrypoints/index.html",
    { encoding: "utf-8" }
  ).replace("{{script_name}}", scriptName)
  fs.writeFileSync(htmlFile, content)
})

console.log("✅ Build complete!")
