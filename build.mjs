import { build } from "esbuild"
import fs from "fs"
import path from "path"

const entryDir = "./output"
const outDir = "./public"

const entryPointDirs = fs.readdirSync(entryDir).filter(d => d.startsWith("EntryPoints."))
const entryPoints = entryPointDirs.map(f => path.join(entryDir, f, "index.js"))

await build({
  entryPoints,
  outdir: outDir,
  bundle: true,
  format: "iife",
  globalName: "Main",
  sourcemap: true,
  minify: true,
})

fs.cpSync("assets", path.join(outDir, "assets"), { recursive: true })

entryPoints.forEach(file => {
  const scriptName = file.replace(/^.*EntryPoints\./, "").replace(/\/index.js$/, "")
  const name = scriptName.split(".").map(path => path.split(/(?=[A-Z])/).map(s => s.toLowerCase()).join("-")).join("/")
  const dir = path.dirname(name)
  fs.mkdirSync(path.join(outDir, dir), { recursive: true })

  const htmlFile = path.join(outDir, `${name}.html`)
  const content = fs.readFileSync(
    "index.html",
    { encoding: "utf-8" }
  ).replace("{{script_name}}", scriptName)
  fs.writeFileSync(htmlFile, content)
})

console.log("✅ Build complete!")
