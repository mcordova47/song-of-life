import { build } from "esbuild"
import { cpSync } from "fs"
import { readdirSync } from "fs"
import { join } from "path"

const entryDir = "./output"
const outDir = "./public"

const entryPoints = readdirSync(entryDir)
  .filter(d => d.startsWith("EntryPoints."))
  .map(f => join(entryDir, f, "index.js"))

await build({
  entryPoints,
  outdir: outDir,
  bundle: true,
  format: "iife",
  globalName: "Main",
  sourcemap: true,
  minify: true,
})

cpSync("entrypoints", outDir, { recursive: true })
cpSync("assets", join(outDir, "assets"), { recursive: true })

console.log("✅ Build complete!")
