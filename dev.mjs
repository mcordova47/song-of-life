import * as esbuild from "esbuild"
import { cpSync, readdirSync } from "fs"
import { join } from "path"

const entryDir = "./output"
const outDir = "./public"

const entryPoints = readdirSync(entryDir)
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

cpSync("entrypoints", outDir, { recursive: true })
cpSync("assets", `${outDir}/assets`, { recursive: true })

await ctx.watch()

console.log("👀 Watching for changes and serving at http://localhost:8000")
